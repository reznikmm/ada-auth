@Part(predefio, Root="ada.mss")

@Comment{$Date: 2012/11/28 23:53:05 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/pre_io.mss,v $}
@Comment{$Revision: 1.67 $}
@LabeledClause{Input-Output}
@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@Redundant[@Defn{input}@Defn{output}
Input-output is provided through
language-defined packages, each of which is a child of the root package Ada.
The generic packages Sequential_IO and Direct_IO
define input-output operations applicable to files containing elements
of a given type. The generic package Storage_IO supports reading from and
writing to an in-memory buffer.
Additional operations for text input-output are
supplied in the packages Text_IO@Chg{Version=[2],New=[,],Old=[ and]}
Wide_Text_IO@Chg{Version=[2],New=[, and Wide_Wide_Text_IO],Old=[]}.
Heterogeneous input-output is provided through the child packages
Streams.@!Stream_IO and Text_IO.@!Text_@!Streams
(see also @RefSecNum{Streams}).
The package IO_Exceptions defines the
exceptions needed by the predefined input-output packages.]
@end{Intro}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
The introduction of Append_File as a new element of the enumeration type
File_Mode in Sequential_IO and Text_IO, and the introduction of
several new declarations
in Text_IO, may result in name clashes in the
presence of @key[use] clauses.
@end{Inconsistent83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Text_IO enhancements
(Get_Immediate, Look_Ahead, Standard_Error, Modular_IO, Decimal_IO),
Wide_Text_IO, and the stream input-output facilities are new in Ada 95.
@end{Extend83}

@begin{DiffWord83}
RM83-14.6, "Low Level Input-Output,"
is removed. This has no semantic effect,
since the package was entirely implementation defined,
nobody actually implemented it,
and if they did, they can always provide it as a vendor-supplied
package.
@end{DiffWord83}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Included package Wide_Wide_Text_IO
  in this description.]}
@end{Diffword95}


@LabeledClause{External Files and File Objects}

@begin{StaticSem}
@Defn{external file}
@Defn2{Term=[name], Sec=(of an external file)}
@Defn2{Term=[form], Sec=(of an external file)}
Values input from the external environment of the program, or output to
the external environment,
are considered to occupy @i{external files}.
An external file can be anything external to the program that can
produce a value to be read or receive a value to be written. An
external file is identified by a string (the @i{name}). A second string
(the @i{form}) gives further system-dependent characteristics that may
be associated with the file, such as the physical organization or access
rights. The conventions governing the interpretation of such strings
shall be documented.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Defn2{Term=[file], Sec=(as file object)}
Input and output operations are expressed as operations on objects of
some @i{file type}, rather than directly in terms of the external files. In
the remainder of this @Chg{Version=[3],New=[clause],Old=[section]}, the term
@i{file} is always used to refer to a file object; the term @i{external file} is
used otherwise.

Input-output for sequential files of values of a single element type is
defined by means of the generic package Sequential_IO.
In order to define sequential input-output for a given element type, an
instantiation of this generic unit, with the given type as actual
parameter, has to be declared. The resulting package contains the
declaration of a file type (called File_Type) for files of such
elements, as well as the operations applicable to these files, such as
the Open, Read, and Write procedures.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
Input-output for direct access files is likewise defined by a generic
package called Direct_IO. Input-output in human-readable form is
defined by the (nongeneric) packages Text_IO for Character and String
data, @Chg{Version=[2],New=[],Old=[and ]}Wide_Text_IO for Wide_Character and
Wide_String data@Chg{Version=[2],New=[, and Wide_Wide_Text_IO for
Wide_Wide_Character and Wide_Wide_String data],Old=[]}.
Input-output for files containing streams of
elements representing values of possibly different types is defined by means of
the (nongeneric) package Streams.Stream_IO.

Before input or output operations can be performed on a file, the file
first has to be associated with an external file. While such an
association is in effect, the file is said to be @i{open}, and otherwise the
file is said to be @i{closed}.

The language does not define what happens to external files after the
completion of the main program and all the library tasks (in particular,
if corresponding files have not been closed).
@Defn2{Term=[access types], Sec=(input-output unspecified)}
@Defn2{Term=[input-output], Sec=(unspecified for access types)}
@PDefn{unspecified}
The effect of input-output for access types is unspecified.

@Leading@Keepnext@Defn2{Term=[current mode], Sec=(of an open file)}
An open file has a @i{current mode}, which is a value of one of the
following enumeration types:
@begin{DescribeCode}
@begin{Example}
@key[type] File_Mode @key[is] (In_File, Inout_File, Out_File);  --@RI{  for Direct_IO}
@end{Example}

These values correspond respectively to the cases where only reading,
both reading and writing, or only writing are to be performed.
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@key[type] File_Mode @key[is] (In_File, Out_File, Append_File);
--@RI{  for Sequential_IO, Text_IO, Wide_Text_IO, @Chg{Version=[2],New=[Wide_Wide_Text_IO, ],
Old=[]}and Stream_IO}
@end{Example}

These values correspond respectively to the cases where only reading,
only writing, or only appending are to be performed.

@Trailing@;The mode of a file can be changed.
@end{DescribeCode}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
Several file management operations are common to Sequential_IO,
Direct_IO, Text_IO, @Chg{Version=[2],New=[],Old=[and ]}Wide_Text_IO@Chg{Version=[2],
New=[, and Wide_Wide_Text_IO],Old=[]}.
These operations are described in subclause
@RefSecNum{File Management} for
sequential and direct files.
Any additional effects concerning text
input-output are described in subclause @RefSecNum{Text File Management}.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The exceptions that can be propagated by the execution of an input-output
subprogram are defined in the package IO_Exceptions; the situations
in which they can be propagated are described following the description of
the subprogram (and in @Chg{Version=[3],New=[subclause],Old=[clause]}
@RefSecNum{Exceptions in Input-Output}).
@Defn2{Term=[Storage_Error],Sec=(raised by failure of run-time check)}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
The exceptions Storage_Error and Program_Error may be propagated.
(Program_Error can only be propagated due to errors made by the
caller of the subprogram.) Finally, exceptions can be propagated
in certain implementation-defined situations.
@ChgImplDef{Version=[2],Kind=[Deleted],InitialVersion=[0],
Text=[@ChgDeleted{Version=[2],
Text=[Any implementation-defined characteristics of the
input-output packages.]}]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The last sentence here is referring to the
documentation requirements in @RefSec{Exceptions in Input-Output}, and
the documentation summary item is provided there.]}
@end{Discussion}
@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
Each instantiation of the generic packages Sequential_IO and Direct_IO
declares a different type File_Type. In the case of Text_IO, Wide_Text_IO,
@Chg{Version=[2],New=[Wide_Wide_Text_IO, ],Old=[]}and Streams.Stream_IO,
the corresponding type File_Type is unique.

A bidirectional device can often be modeled as two sequential files
associated with the device, one of mode In_File, and one of mode
Out_File. An implementation may restrict the number of files that may
be associated with a given external file.
@end{Notes}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Included package Wide_Wide_Text_IO
  in this description.]}
@end{Diffword95}


@RMNewPageVer{Version=[1]}@Comment{Break here so printed Ada 95 w/ corrigendum RM looks better.}
@LabeledClause{Sequential and Direct Files}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00283-01]}
@Defn{sequential file}
@Defn{direct file}@Chg{Version=[2],New=[@Defn{stream file}],Old=[]}
Two kinds of access to external files are defined in this subclause:
@i{sequential access} and @i{direct access}.
The corresponding file types and the associated
operations are provided by the generic packages Sequential_IO and
Direct_IO. A file
object to be used for sequential access is called a @i{sequential file},
and one to be used for direct access is called a @i{direct file}.
Access to @Chg{Version=[2],New=[@i{stream file}s],Old=[stream files]}
is described in @RefSecNum(The Package Streams.Stream_IO).

@Defn{sequential access}
For sequential access, the file is viewed as a sequence of values that
are transferred in the order of their appearance (as produced by the
program or by the external environment).
When the file is opened with mode
In_File or Out_File, transfer starts respectively
from or to the beginning of the file. When the file is opened with mode
Append_File, transfer to the file starts after the last element of the
file.
@begin{Discussion}
Adding stream I/O necessitates a review of the terminology. In
Ada 83, `sequential' implies both the access method (purely sequential
@em that is, no indexing or positional access) and homogeneity. Direct
access includes purely sequential access and indexed access, as well as
homogeneity. In Ada 95, streams allow purely sequential access but also
positional access to an individual
element, and are
heterogeneous. We considered generalizing the notion of
`sequential file' to include both Sequential_IO and Stream_IO files, but
since streams allow positional access it seems misleading to call them
sequential files. Or, looked at differently, if the criterion for
calling something a sequential file is whether it permits (versus
requires) purely sequential access, then one could just as soon regard a
Direct_IO file as a sequential file.

It seems better to regard `sequential file' as meaning `only permitting
purely sequential access'; hence we have decided to supplement `sequential
access' and `direct access' with a third category, informally called `access
to streams'. (We decided against the term `stream access' because of possible
confusion with the Stream_Access type declared in one of the stream packages.)@end{discussion}

@Defn{direct access}
@Defn2{Term=[index], Sec=(of an element of an open direct file)}
@Defn2{Term=[current size], Sec=(of an external file)}
For direct access, the file is viewed as a set of elements occupying
consecutive positions in linear order; a value can be transferred to or
from an element of the file at any selected position. The position of
an element is specified by its @i{index}, which is a number, greater than
zero, of the implementation-defined integer type Count. The first
element, if any, has index one; the index of the last element, if any,
is called the @i{current size}; the current size is zero if there are no
elements. The current size is a property of the external file.

@Defn2{Term=[current index], Sec=(of an open direct file)}
An open direct file has a @i{current index}, which is the index that will be
used by the next read or write operation. When a direct file is opened,
the current index is set to one. The current index of a direct file is
a property of a file object, not of an external file.
@end{StaticSem}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00283-01]}
  @ChgAdded{Version=[2],Text=[Italicized @lquotes@;stream file@rquotes@;
  to clarify that this is another kind of file.]}
@end{DiffWord95}


@LabeledSubClause{The Generic Package Sequential_IO}

@begin{StaticSem}
@Leading@;The generic library package Sequential_IO has the following declaration:
@begin{Example}
@key[with] Ada.IO_Exceptions;
@key[generic]
   @key[type] Element_Type(<>) @key[is] @key[private];
@key[package] Ada.Sequential_IO @key[is]@ChildUnit{Parent=[Ada],Child=[Sequential_IO]}

   @key[type] @AdaTypeDefn{File_Type} @key[is] @key[limited] @key[private];

   @key[type] @AdaTypeDefn{File_Mode} @key[is] (In_File, Out_File, Append_File);

   @RI{-- File management}

   @key[procedure] @AdaSubDefn{Create}(File : @key[in] @key[out] File_Type;
                    Mode : @key[in] File_Mode := Out_File;
                    Name : @key[in] String := "";
                    Form : @key[in] String := "");

   @key[procedure] @AdaSubDefn{Open}  (File : @key[in] @key[out] File_Type;
                    Mode : @key[in] File_Mode;
                    Name : @key[in] String;
                    Form : @key[in] String := "");

   @key[procedure] @AdaSubDefn{Close} (File : @key[in] @key[out] File_Type);
   @key[procedure] @AdaSubDefn{Delete}(File : @key[in] @key[out] File_Type);
   @key[procedure] @AdaSubDefn{Reset} (File : @key[in] @key[out] File_Type; Mode : @key[in] File_Mode);
   @key[procedure] @AdaSubDefn{Reset} (File : @key[in] @key[out] File_Type);

   @key[function] @AdaSubDefn{Mode}   (File : @key[in] File_Type) @key[return] File_Mode;
   @key[function] @AdaSubDefn{Name}   (File : @key[in] File_Type) @key[return] String;
   @key[function] @AdaSubDefn{Form}   (File : @key[in] File_Type) @key[return] String;

   @key[function] @AdaSubDefn{Is_Open}(File : @key[in] File_Type) @key[return] Boolean;

   --@RI{ Input and output operations}

   @key[procedure] @AdaSubDefn{Read}  (File : @key[in] File_Type; Item : @key[out] Element_Type);
   @key[procedure] @AdaSubDefn{Write} (File : @key[in] File_Type; Item : @key[in] Element_Type);

   @key[function] @AdaSubDefn{End_Of_File}(File : @key[in] File_Type) @key[return] Boolean;

@keepnext   --@RI{ Exceptions}

   @AdaExcDefn{Status_Error} : @key[exception] @key[renames] IO_Exceptions.Status_Error;
   @AdaExcDefn{Mode_Error}   : @key[exception] @key[renames] IO_Exceptions.Mode_Error;
   @AdaExcDefn{Name_Error}   : @key[exception] @key[renames] IO_Exceptions.Name_Error;
   @AdaExcDefn{Use_Error}    : @key[exception] @key[renames] IO_Exceptions.Use_Error;
   @AdaExcDefn{Device_Error} : @key[exception] @key[renames] IO_Exceptions.Device_Error;
   @AdaExcDefn{End_Error}    : @key[exception] @key[renames] IO_Exceptions.End_Error;
   @AdaExcDefn{Data_Error}   : @key[exception] @key[renames] IO_Exceptions.Data_Error;

@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Sequential_IO;
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Text=[The type File_Type
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization})
in every instantiation of Sequential_IO.]}

@end{StaticSem}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
The new enumeration element Append_File may introduce upward incompatibilities.
It is possible that a program based on the assumption that File_Mode'Last = Out_File
will be illegal (e.g., case statement choice coverage)
or execute with a different effect in Ada 95.
@end{Incompatible83}

@begin{ImplNote}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0097],ARef=[AI95-00115-01]}
@ChgRef{Version=[2],Kind=[DeletedAdded],ARef=[AI95-00344-01]}
@ChgDeleted{Version=[2],Text=[@Chg{Version=[1],New=[File_Type cannot be
implemented as a (directly) controlled type, as Ada.Sequential_IO can be
instantiated at any nesting depth. File_Type could have a component of a
controlled type, as long as that type is declared in some other (nongeneric)
package.],Old=[]}]}@ChgNote{AI-344 allows controlled types to be declared at
any nesting depth, so this note is obsolete.}
@end{ImplNote}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @B[Amendment Correction:] File_Type in
  an instance of Sequential_IO is defined to need finalization. If the
  restriction No_Nested_Finalization (see @RefSecNum{Tasking Restrictions})
  applies to the partition, and File_Type does not have a controlled part, it
  will not be allowed in local objects in Ada 2005 whereas it would be allowed
  in original Ada 95. Such code is not portable, as another Ada compiler
  may have a controlled part in File_Type, and thus would be illegal.]}
@end{Incompatible95}


@LabeledSubClause{File Management}

@begin{StaticSem}

The procedures and functions described in this subclause provide for the
control of external files; their declarations are repeated in each of
the packages for sequential, direct,
text, and stream
input-output. For
text input-output, the procedures Create, Open, and Reset have
additional effects described in subclause
@RefSecNum{Text File Management}.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Create(File : @key[in] @key[out] File_Type;
                 Mode : @key[in] File_Mode := @RI{default_mode};
                 Name : @key[in] String := "";
                 Form : @key[in] String := "");
@end{Example}
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00283-01]}
    Establishes a new external file, with the given name and form,
    and associates this external file with the given file. The
    given file is left open. The current mode of the given file
    is set to the given access mode. The default access mode is
    the mode Out_File for sequential@Chg{Version=[2],New=[, stream,],Old=[]}
    and text input-output; it is
    the mode Inout_File for direct input-output. For direct
    access, the size of the created file is implementation defined.

    A null string for Name specifies an
    external file that is not accessible after the completion of
    the main program (a temporary file). A null string for Form
    specifies the use of the default options of the implementation
    for the external file.

    @Trailing@;The exception Status_Error is propagated if the given file is
    already open. The exception Name_Error is propagated if the
    string given as Name does not allow the identification of an
    external file. The exception Use_Error is propagated if, for the
    specified mode, the external environment does not support creation of
    an external file with the given name (in the absence of
    Name_Error) and form.

@begin{Example}@Keepnext
@key[procedure] Open(File : @key[in] @key[out] File_Type;
               Mode : @key[in] File_Mode;
               Name : @key[in] String;
               Form : @key[in] String := "");
@end{Example}
    Associates the given file with an existing external file
    having the given name and form, and sets the current mode of
    the given file to the given mode. The given file is left open.

    @Trailing@;The exception Status_Error is propagated if the given file is
    already open. The exception Name_Error is propagated if the
    string given as Name does not allow the identification of an
    external file; in particular, this exception is propagated if no
    external file with the given name exists. The exception
    Use_Error is propagated if, for the specified mode, the
    external environment does not support opening for an external file with
    the given name (in the absence of Name_Error) and form.

@begin{Example}@Keepnext
@key[procedure] Close(File : @key[in] @key[out] File_Type);
@end{Example}
    Severs the association between the given file and its
    associated external file. The given file is left closed.
    In addition, for sequential files, if the file
    being closed has mode Out_File or Append_File, then the last element
    written since the most recent open or reset is the last element that
    can be read from the file. If no elements have been written and the
    file mode is Out_File, then the closed file is empty.
    If no elements have been written and the file mode is Append_File,
    then the closed file is unchanged.

    @Trailing@;The exception Status_Error is propagated if the given file is not open.

@begin{Example}@Keepnext
@key[procedure] Delete(File : @key[in] @key[out] File_Type);
@end{Example}
    Deletes the external file associated with the given file. The
    given file is closed, and the external file ceases to exist.

    @Trailing@;The exception Status_Error is propagated if the given file is not
    open. The exception Use_Error is propagated if
    deletion of the external file is not supported
    by the external environment.

@begin{Example}@Keepnext
@key[procedure] Reset(File : @key[in] @key[out] File_Type; Mode : @key[in] File_Mode);
@key[procedure] Reset(File : @key[in] @key[out] File_Type);
@end{Example}
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00085-01]}
    Resets the given file so that reading from its
    elements can be restarted from the beginning of the
    @Chg{Version=[2],New=[external ],Old=[]}file (for
    modes In_File and Inout_File), and so that writing to its elements can be
    restarted at the beginning of the
    @Chg{Version=[2],New=[external ],Old=[]}file (for modes Out_File and Inout_File)
    or after the last element of the @Chg{Version=[2],New=[external ],Old=[]}file
    (for mode Append_File).
    In particular, for direct access this means that the current
    index is set to one. If a Mode parameter is supplied, the
    current mode of the given file is set to the given mode. In addition, for
    sequential files, if the given file has mode Out_File or Append_File when
    Reset is called, the last element written since the most recent open or
    reset is the last element that can be read from the
    @Chg{Version=[2],New=[external ],Old=[]}file. If no elements
    have been written and the file mode is Out_File, the reset file is empty.
    If no elements have been written and the file mode is Append_File,
    then the reset file is unchanged.

    @Trailing@;The exception Status_Error is propagated if the file is not open.
    The exception Use_Error is propagated if the external environment
    does not support resetting for the external file and, also, if the
    external environment does not support resetting to the specified mode
    for the external file.

@begin{Example}@Keepnext
@key[function] Mode(File : @key[in] File_Type) @key[return] File_Mode;
@end{Example}
    Returns the current mode of the given file.

    @Trailing@;The exception Status_Error is propagated if the file is not open.

@begin{Example}@Keepnext
@key[function] Name(File : @key[in] File_Type) @key[return] String;
@end{Example}
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00248-01]}
    Returns a string which uniquely identifies the external file
    currently associated with the given file (and may thus be used
    in an Open operation).@Chg{Version=[2],New=[],Old=[ If an external
    environment allows alternative
    specifications of the name (for example, abbreviations), the string
    returned by the function should correspond to a full specification of
    the name.]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
  @ChgAdded{Version=[2],Text=[Retrieving the full path can be
  accomplished by passing the result of Name to Directories.Full_Name
  (see @RefSecNum{The Package Directories}). It is important to drop the
  requirement on Name, as the only way to accomplish this requirement given
  that the current directory can be changed with package Directories is to
  store the full path when the file is opened. That's expensive, and it's
  better for users that need the full path to explicitly request it.]}
@end{Discussion}

    @Trailing@;The exception Status_Error is propagated if the given file is not
    open. The exception Use_Error is propagated if the associated
    external file is a temporary file that cannot be opened
    by any name.

@begin{Example}@Keepnext
@key[function] Form(File : @key[in] File_Type) @key[return] String;
@end{Example}
    Returns the form string for the external file currently
    associated with the given file. If an
    external environment allows
    alternative specifications of the form (for example,
    abbreviations using default options), the string returned by
    the function should correspond to a full specification (that
    is, it should indicate explicitly all options selected,
    including default options).

    @Trailing@;The exception Status_Error is propagated if the given file is not open.

@begin{Example}@Keepnext
@key[function] Is_Open(File : @key[in] File_Type) @key[return] Boolean;
@end{Example}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
    Returns True if the file is open (that is, if it is associated
    with an external file)@Chg{Version=[3],New=[;],Old=[,]}
    otherwise@Chg{Version=[3],New=[,],Old=[]} returns False.
@end{DescribeCode}
@end{StaticSem}

@begin{ImplPerm}
An implementation may propagate
Name_Error or Use_Error if an attempt is
made to use an I/O feature that cannot be supported by the
implementation due to limitations in the external environment.
Any such restriction should be documented.
@end{ImplPerm}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00085-01]}
  @ChgAdded{Version=[2],Text=[Clarified that Reset affects and depends on the
  external file.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00248-01]}
  @ChgAdded{Version=[2],Text=[Removed the requirement for Name to return
  a full path; this is now accomplished by Directories.Full_Name(Name(File))
  (see @RefSecNum{The Package Directories}). This is not documented as
  an inconsistency, because there is no requirement for implementations to
  change @em the Ada 95 behavior is still allowed, it just is no longer
  required.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00283-01]}
  @ChgAdded{Version=[2],Text=[Added text to specify the default mode for
  a stream file.]}
@end{DiffWord95}


@LabeledSubClause{Sequential Input-Output Operations}

@begin{StaticSem}
The operations available for sequential input and output are described
in this subclause. The exception Status_Error is propagated if any of these
operations is attempted for a file that is not open.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Read(File : @key[in] File_Type; Item : @key[out] Element_Type);
@end{Example}
Operates on a file of mode In_File. Reads an element
from the given file, and returns the value of this element
in the Item parameter.
@begin{Discussion}
  We considered basing Sequential_IO.Read on Element_Type'Read from
  an implicit stream associated with the sequential file.
  However, Element_Type'Read is a type-related
  attribute, whereas Sequential_IO should take advantage of the
  particular constraints of the actual subtype corresponding
  to Element_Type to minimize the size of the external file.
  Furthermore, forcing the implementation of Sequential_IO to
  be based on Element_Type'Read would create an upward incompatibility
  since existing data files written by an Ada 83 program using
  Sequential_IO might not be readable by the identical program
  built with an Ada 95 implementation of Sequential_IO.

  An Ada 95 implementation might still use an implementation-defined
  attribute analogous to 'Read to implement the procedure Read,
  but that attribute will likely have to be subtype-specific
  rather than type-related, and it need not be user-specifiable.
  Such an attribute will presumably be needed to implement the
  generic package Storage_IO (see @RefSecNum{The Generic Package Storage_IO}).
@end{Discussion}

@Trailing@;The exception Mode_Error is propagated if the mode is not In_File.
The exception End_Error is propagated if no more elements can be
read from the given file.
The exception Data_Error can be propagated if the element read cannot
be interpreted as a value of the subtype Element_Type
(see @RefSec{Exceptions in Input-Output}).
@begin{Discussion}
  Data_Error need not be propagated if the check is too complex.
  See @RefSec{Exceptions in Input-Output}.
@end{Discussion}

@begin{Example}@Keepnext
@key[procedure] Write(File : @key[in] File_Type; Item : @key[in] Element_Type);
@end{Example}
Operates on a file of mode Out_File or Append_File.
Writes the value of Item to the given file.

@Trailing@;The exception Mode_Error is propagated if the mode is not
Out_File or Append_File. The exception Use_Error is propagated if the capacity
of the external file is exceeded.

@begin{Example}@Keepnext
@key[function] End_Of_File(File : @key[in] File_Type) @key[return] Boolean;
@end{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
Operates on a file of mode In_File. Returns True if no more
elements can be read from the given file;
otherwise@Chg{Version=[3],New=[,],Old=[]} returns False.

The exception Mode_Error is propagated if the mode is not In_File.
@end{DescribeCode}
@end{StaticSem}

@LabeledSubClause{The Generic Package Direct_IO}

@begin{StaticSem}
@Leading@;The generic library package Direct_IO has the following declaration:
@begin{Example}
@key[with] Ada.IO_Exceptions;
@key[generic]
   @key[type] Element_Type @key[is] @key[private];
@key[package] Ada.Direct_IO @key[is]@ChildUnit{Parent=[Ada],Child=[Direct_IO]}

   @key[type] @AdaTypeDefn{File_Type} @key[is] @key[limited] @key[private];

   @key[type] @AdaTypeDefn{File_Mode} @key[is] (In_File, Inout_File, Out_File);
   @key[type] @AdaTypeDefn{Count}     @key[is] @key[range] 0 .. @RI[implementation-defined];
   @key[subtype] @AdaSubtypeDefn{Name=[Positive_Count],Of=[Count]} @key[is] Count @key[range] 1 .. Count'Last;

   --@RI{ File management}

   @key[procedure] @AdaSubDefn{Create}(File : @key[in] @key[out] File_Type;
                    Mode : @key[in] File_Mode := Inout_File;
                    Name : @key[in] String := "";
                    Form : @key[in] String := "");

   @key[procedure] @AdaSubDefn{Open}  (File : @key[in] @key[out] File_Type;
                    Mode : @key[in] File_Mode;
                    Name : @key[in] String;
                    Form : @key[in] String := "");

   @key[procedure] @AdaSubDefn{Close} (File : @key[in] @key[out] File_Type);
   @key[procedure] @AdaSubDefn{Delete}(File : @key[in] @key[out] File_Type);
   @key[procedure] @AdaSubDefn{Reset} (File : @key[in] @key[out] File_Type; Mode : @key[in] File_Mode);
   @key[procedure] @AdaSubDefn{Reset} (File : @key[in] @key[out] File_Type);

   @key[function] @AdaSubDefn{Mode}   (File : @key[in] File_Type) @key[return] File_Mode;
   @key[function] @AdaSubDefn{Name}   (File : @key[in] File_Type) @key[return] String;
   @key[function] @AdaSubDefn{Form}   (File : @key[in] File_Type) @key[return] String;

   @key[function] @AdaSubDefn{Is_Open}(File : @key[in] File_Type) @key[return] Boolean;

   --@RI{ Input and output operations}

   @key[procedure] @AdaSubDefn{Read} (File : @key[in] File_Type; Item : @key[out] Element_Type;
                                        From : @key[in] Positive_Count);
   @key[procedure] @AdaSubDefn{Read} (File : @key[in] File_Type; Item : @key[out] Element_Type);

   @key[procedure] @AdaSubDefn{Write}(File : @key[in] File_Type; Item : @key[in]  Element_Type;
                                        To   : @key[in] Positive_Count);
   @key[procedure] @AdaSubDefn{Write}(File : @key[in] File_Type; Item : @key[in] Element_Type);

   @key[procedure] @AdaSubDefn{Set_Index}(File : @key[in] File_Type; To : @key[in] Positive_Count);

   @key[function] @AdaSubDefn{Index}(File : @key[in] File_Type) @key[return] Positive_Count;
   @key[function] @AdaSubDefn{Size} (File : @key[in] File_Type) @key[return] Count;

   @key[function] @AdaSubDefn{End_Of_File}(File : @key[in] File_Type) @key[return] Boolean;

@keepnext   --@RI{ Exceptions}

   @AdaExcDefn{Status_Error} : @key[exception] @key[renames] IO_Exceptions.Status_Error;
   @AdaExcDefn{Mode_Error}   : @key[exception] @key[renames] IO_Exceptions.Mode_Error;
   @AdaExcDefn{Name_Error}   : @key[exception] @key[renames] IO_Exceptions.Name_Error;
   @AdaExcDefn{Use_Error}    : @key[exception] @key[renames] IO_Exceptions.Use_Error;
   @AdaExcDefn{Device_Error} : @key[exception] @key[renames] IO_Exceptions.Device_Error;
   @AdaExcDefn{End_Error}    : @key[exception] @key[renames] IO_Exceptions.End_Error;
   @AdaExcDefn{Data_Error}   : @key[exception] @key[renames] IO_Exceptions.Data_Error;

@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Direct_IO;
@end{Example}
@begin{Reason}
The Element_Type formal of Direct_IO does not have an
@nt{unknown_discriminant_part} (unlike Sequential_IO)
so that the implementation can make use of the ability to declare
uninitialized variables of the type.
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Text=[The type File_Type
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization})
in every instantiation of Direct_IO.]}

@begin{ImplNote}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0097],ARef=[AI95-00115-01]}
@ChgRef{Version=[2],Kind=[DeletedAdded],ARef=[AI95-00344-01]}
@ChgDeleted{Version=[2],Text=[@Chg{Version=[1],New=[File_Type cannot be
implemented as a (directly)
controlled type, as Ada.Direct_IO can be instantiated at any nesting depth.
File_Type could have a component of a controlled type, as long as that type is
declared in some other (nongeneric) package.],Old=[]}]}
@end{ImplNote}

@end{StaticSem}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @B[Amendment Correction:] File_Type in
  an instance of Direct_IO is defined to need finalization. If the
  restriction No_Nested_Finalization (see @RefSecNum{Tasking Restrictions})
  applies to the partition, and File_Type does not have a controlled part, it
  will not be allowed in local objects in Ada 2005 whereas it would be allowed
  in original Ada 95. Such code is not portable, as another Ada compiler
  may have a controlled part in File_Type, and thus would be illegal.]}
@end{Incompatible95}


@LabeledSubClause{Direct Input-Output Operations}

@begin{StaticSem}
The operations available for direct input and output are described in
this subclause. The exception Status_Error is propagated if any of these
operations is attempted for a file that is not open.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Read(File : @key[in] File_Type; Item : @key[out] Element_Type;
                                    From : @key[in]  Positive_Count);
@key[procedure] Read(File : @key[in] File_Type; Item : @key[out] Element_Type);
@end{Example}
Operates on a file of mode In_File or Inout_File. In the case
of the first form, sets the current index of the given file to
the index value given by the parameter From. Then (for both
forms) returns, in the parameter Item, the value of the
element whose position in the given file is specified by the
current index of the file; finally, increases the current
index by one.

@Trailing@;The exception Mode_Error is propagated if the mode of the given
file is Out_File. The exception End_Error is propagated if the
index to be used exceeds the size of the external file. The
exception Data_Error can be propagated if the element read cannot be
interpreted as a value of the subtype Element_Type
(see @RefSecNum{Exceptions in Input-Output}).

@begin{Example}@Keepnext
@key[procedure] Write(File : @key[in] File_Type; Item : @key[in] Element_Type;
                                     To   : @key[in] Positive_Count);
@key[procedure] Write(File : @key[in] File_Type; Item : @key[in] Element_Type);
@end{Example}
Operates on a file of mode Inout_File or Out_File. In the
case of the first form, sets the index of the given file to
the index value given by the parameter To. Then (for both
forms) gives the value of the parameter Item to the element
whose position in the given file is specified by the current
index of the file; finally, increases the current index by
one.

@Trailing@;The exception Mode_Error is propagated if the mode of the given
file is In_File. The exception Use_Error is propagated if the
capacity of the external file is exceeded.

@begin{Example}@Keepnext
@key[procedure] Set_Index(File : @key[in] File_Type; To : @key[in] Positive_Count);
@end{Example}
@Trailing@;Operates on a file of any mode. Sets the current index of the
given file to the given index value (which may exceed the
current size of the file).

@begin{Example}@Keepnext
@key[function] Index(File : @key[in] File_Type) @key[return] Positive_Count;
@end{Example}
@Trailing@;Operates on a file of any mode. Returns the current index of
the given file.

@begin{Example}@Keepnext
@key[function] Size(File : @key[in] File_Type) @key[return] Count;
@end{Example}
@Trailing@;Operates on a file of any mode. Returns the current size of
the external file that is associated with the given file.

@begin{Example}@Keepnext
@key[function] End_Of_File(File : @key[in] File_Type) @key[return] Boolean;
@end{Example}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
Operates on a file of mode In_File or Inout_File. Returns
True if the current index exceeds the size of the external
file; otherwise@Chg{Version=[3],New=[,],Old=[]} returns False.

The exception Mode_Error is propagated if the mode of the given file is
Out_File.

@end{DescribeCode}
@end{StaticSem}

@begin{Notes}
Append_File mode is not supported for the generic package Direct_IO.
@end{Notes}

@LabeledClause{The Generic Package Storage_IO}

@begin{Intro}
The generic package Storage_IO provides for reading from and
writing to an in-memory buffer. This generic package supports
the construction of user-defined input-output packages.
@begin{Reason}
  This package exists to allow the portable construction of
  user-defined direct-access-oriented input-output packages.
  The Write procedure writes a value of type Element_Type into a
  Storage_Array of size Buffer_Size, flattening out any implicit
  levels of indirection used in the representation of the
  type. The Read procedure reads
  a value of type Element_Type from the buffer, reconstructing
  any implicit levels of indirection used in the representation
  of the type. It also properly initializes any type tags that appear
  within the value, presuming that the buffer was written by
  a different program and that tag values for the@lquotes@;same@rquotes@; type
  might vary from one executable to another.
@end{Reason}
@end{Intro}

@begin{StaticSem}
@Leading@;The generic library package Storage_IO has the following declaration:
@begin{Example}
@key[with] Ada.IO_Exceptions;
@key[with] System.Storage_Elements;
@key[generic]
   @key[type] Element_Type @key[is] @key[private];
@key[package] Ada.Storage_IO @key[is]@ChildUnit{Parent=[Ada],Child=[Storage_IO]}
   @key[pragma] Preelaborate(Storage_IO);

   @AdaObjDefn{Buffer_Size} : @key(constant) System.Storage_Elements.Storage_Count :=
      @RI(implementation-defined);
   @key(subtype) @AdaSubtypeDefn{Name=[Buffer_Type],Of=[Storage_Array]} @key(is)
      System.Storage_Elements.Storage_Array(1..Buffer_Size);

   --@RI{ Input and output operations}

   @key[procedure] @AdaSubDefn{Read} (Buffer : @key[in]  Buffer_Type; Item : @key[out] Element_Type);

   @key[procedure] @AdaSubDefn{Write}(Buffer : @key[out] Buffer_Type; Item : @key[in]  Element_Type);

@keepnext   --@RI{ Exceptions}

   @AdaExcDefn{Data_Error}   : @key[exception] @key[renames] IO_Exceptions.Data_Error;
@key[end] Ada.Storage_IO;
@end{Example}

In each instance,
the constant Buffer_Size has a value that is the size (in storage elements)
of the buffer required to represent the content of an object of
subtype Element_Type, including any implicit levels of indirection used by
the implementation.
The Read and Write procedures of Storage_IO correspond to the Read and Write
procedures of Direct_IO (see @RefSecNum{The Generic Package Direct_IO}),
but with the content of the Item parameter being read from
or written into the specified Buffer, rather than an external file.

@begin{Reason}
@Leading@;As with Direct_IO, the Element_Type formal of Storage_IO does not
have an @nt{unknown_discriminant_part}
so that there is a well-defined upper bound on the size of
the buffer needed to hold the content of an object of the
formal subtype (i.e. Buffer_Size). If there are no implicit levels
of indirection, Buffer_Size will typically equal:
@begin{Example}
(Element_Type'Size + System.Storage_Unit - 1) / System.Storage_Unit
@end{Example}
@end{Reason}
@ImplDef{The value of Buffer_Size in Storage_IO.}
@end{StaticSem}

@begin{Notes}
A buffer used for Storage_IO holds only one element at a time; an external
file used for Direct_IO holds a sequence of elements.
@end{Notes}

@begin{Extend83}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 83}
Storage_IO is new in Ada 95.]}
@end{Extend83}


@LabeledClause{Text Input-Output}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]} describes the
package Text_IO, which provides facilities
for input and output in human-readable form. Each file is read or
written sequentially, as a sequence of characters grouped into lines,
and as a sequence of lines grouped into pages. The specification of the
package is given below in subclause
@RefSecNum{The Package Text_IO}.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The facilities for file management given above, in subclauses
@RefSecNum{File Management} and
@RefSecNum{Sequential Input-Output Operations},
are available for text input-output. In place of Read and
Write, however, there are procedures Get and Put that input values of
suitable types from text files, and output values to them. These values
are provided to the Put procedures, and returned by the Get procedures,
in a parameter Item. Several overloaded procedures of these names
exist, for different types of Item. These Get procedures analyze the
input sequences of characters based on
lexical elements (see
@Chg{Version=[3],New=[Clause],Old=[Section]} @RefSecNum{Lexical Elements}) and
return the corresponding values; the Put procedures output the given
values as appropriate lexical elements. Procedures Get and Put are also
available that input and output individual characters treated as
character values rather than as lexical elements.
Related to character input are procedures to look ahead at the
next character without reading it, and to read a character @lquotes@;immediately@rquotes@;
without waiting for an end-of-line to signal availability.

In addition to the procedures Get and Put for numeric and enumeration
types of Item that operate on text files, analogous procedures are
provided that read from and write to a parameter of type String. These
procedures perform the same analysis and composition of character
sequences as their counterparts which have a file parameter.

For all Get and Put procedures that operate on text files, and for many
other subprograms, there are forms with and without a file parameter.
Each such Get procedure operates on an input file, and each such Put
procedure operates on an output file. If no file is specified, a
default input file or a default output file is used.

@Defn{standard input file}
@Defn{standard output file}
At the beginning of program execution the default input and output files
are the so-called standard input file and standard output file. These
files are open, have respectively the current modes In_File and
Out_File, and are associated with two implementation-defined external
files. Procedures are provided to change the current default input file
and the current default output file.
@ChgNote{The following was poorly formatted.}
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[@Chg{Version=[2],New=[The ],
Old=[]}external files @Chg{Version=[2],New=[associated with the],Old=[for]} standard
input, standard output, and standard error@Chg{Version=[2],New=[ files.],Old=[]}]}
@begin{ImplNote}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0113],ARef=[AI95-00087-01]}
@ChgAdded{Version=[1],Text=[The default input file and default output file
are not the names
of distinct file objects, but rather the @i<role> played by one or more
(other) file object(s). Thus, they generally will be implemented as accesses
to another file object. An implementation that implements them by copying
them is incorrect.]}
@end{ImplNote}

@Defn{standard error file}
At the beginning of program execution a default file for
program-dependent error-related text output is the
so-called standard error file. This file is open, has the current
mode Out_File, and is associated with an implementation-defined
external file. A procedure is provided to change the current
default error file.

@Defn{line terminator}
@Defn{page terminator}
@Defn{file terminator}
From a logical point of view, a text file is a sequence of pages, a page
is a sequence of lines, and a line is a sequence of characters; the end
of a line is marked by a @i{line terminator}; the end of a page is marked by
the combination of a line terminator immediately followed by a @i{page
terminator}; and the end of a file is marked by the combination of a line
terminator immediately followed by a page terminator and then a @i{file
terminator}. Terminators are generated during output; either by calls of
procedures provided expressly for that purpose; or implicitly as part of
other operations, for example, when a bounded line length, a bounded
page length, or both, have been specified for a file.

The actual nature of terminators is not defined by the language and
hence depends on the implementation. Although terminators are
recognized or generated by certain of the procedures that follow, they
are not necessarily implemented as characters or as sequences of
characters. Whether they are characters (and if so which ones) in any
particular implementation need not concern a user who neither explicitly
outputs nor explicitly inputs control characters. The effect of input
(Get)
or output (Put)
of control characters (other than horizontal tabulation) is
not specified by the language.
@PDefn{unspecified}

@Defn{column number}
@Defn{current column number}
@Defn{current line number}
@Defn{current page number}
The characters of a line are numbered, starting from one; the number of
a character is called its @i{column number}. For a line terminator, a
column number is also defined: it is one more than the number of
characters in the line. The lines of a page, and the pages of a file,
are similarly numbered. The current column number is the column number
of the next character or line terminator to be transferred. The current
line number is the number of the current line. The current page number
is the number of the current page. These numbers are values of the
subtype Positive_Count of the type Count (by convention, the value zero
of the type Count is used to indicate special conditions).
@begin{Example}
@Trailing@key[type] Count @key[is] @key[range] 0 .. @RI[implementation-defined];
@key[subtype] Positive_Count @key[is] Count @key[range] 1 .. Count'Last;
@end{Example}

@Defn{maximum line length}
@Defn{maximum page length}
For an output file or an append file, a @i{maximum line length} can be specified
and a @i{maximum page length} can be specified. If a value to be output cannot
fit on the current line, for a specified maximum line length, then a new line
is automatically started before the value is output; if, further, this new
line cannot fit on the current page, for a specified maximum page
length, then a new page is automatically started before the value is
output. Functions are provided to determine the maximum line length and
the maximum page length. When a file is opened with mode Out_File
or Append_File, both values are zero: by convention, this means that the line
lengths and
page lengths are unbounded. (Consequently, output consists of a single
line if the subprograms for explicit control of line and page structure
are not used.) The constant Unbounded is provided for this purpose.
@end{StaticSem}

@begin{Extend83}
@Defn{extensions to Ada 83}
Append_File is new in Ada 95.
@end{Extend83}

@LabeledSubClause{The Package Text_IO}

@begin{StaticSem}
@Leading@;The library package Text_IO has the following declaration:
@begin{Example}
@key[with] Ada.IO_Exceptions;
@key[package] Ada.Text_IO @key[is]@ChildUnit{Parent=[Ada],Child=[Text_IO]}

   @key[type] @AdaTypeDefn{File_Type} @key[is] @key[limited] @key[private];

   @key[type] @AdaTypeDefn{File_Mode} @key[is] (In_File, Out_File, Append_File);

   @key[type] @AdaTypeDefn{Count} @key[is] @key[range] 0 .. @RI[implementation-defined];
   @key[subtype] @AdaSubtypeDefn{Name=[Positive_Count],Of=[Count]} @key[is] Count @key[range] 1 .. Count'Last;
   @AdaObjDefn{Unbounded} : @key[constant] Count := 0; --@RI{ line and page length}

   @key[subtype] @AdaSubtypeDefn{Name=[Field],Of=[Integer]}       @key[is] Integer @key[range] 0 .. @RI[implementation-defined];
   @key[subtype] @AdaSubtypeDefn{Name=[Number_Base],Of=[Integer]} @key[is] Integer @key[range] 2 .. 16;

   @key[type] @AdaTypeDefn{Type_Set} @key[is] (Lower_Case, Upper_Case);

   --@RI{ File Management}

   @key[procedure] @AdaSubDefn{Create} (File : @key[in] @key[out] File_Type;
                     Mode : @key[in] File_Mode := Out_File;
                     Name : @key[in] String    := "";
                     Form : @key[in] String    := "");

   @key[procedure] @AdaSubDefn{Open}   (File : @key[in] @key[out] File_Type;
                     Mode : @key[in] File_Mode;
                     Name : @key[in] String;
                     Form : @key[in] String := "");

   @key[procedure] @AdaSubDefn{Close}  (File : @key[in] @key[out] File_Type);
   @key[procedure] @AdaSubDefn{Delete} (File : @key[in] @key[out] File_Type);
   @key[procedure] @AdaSubDefn{Reset}  (File : @key[in] @key[out] File_Type; Mode : @key[in] File_Mode);
   @key[procedure] @AdaSubDefn{Reset}  (File : @key[in] @key[out] File_Type);

   @key[function]  @AdaSubDefn{Mode}   (File : @key[in] File_Type) @key[return] File_Mode;
   @key[function]  @AdaSubDefn{Name}   (File : @key[in] File_Type) @key[return] String;
   @key[function]  @AdaSubDefn{Form}   (File : @key[in] File_Type) @key[return] String;

   @key[function]  @AdaSubDefn{Is_Open}(File : @key[in] File_Type) @key[return] Boolean;

   --@RI{ Control of default input and output files}

   @key[procedure] @AdaSubDefn{Set_Input} (File : @key[in] File_Type);
   @key[procedure] @AdaSubDefn{Set_Output}(File : @key[in] File_Type);
   @key[procedure] @AdaSubDefn{Set_Error} (File : @key[in] File_Type);

   @key[function] @AdaSubDefn{Standard_Input}  @key[return] File_Type;
   @key[function] @AdaSubDefn{Standard_Output} @key[return] File_Type;
   @key[function] @AdaSubDefn{Standard_Error}  @key[return] File_Type;

   @key[function] @AdaSubDefn{Current_Input}   @key[return] File_Type;
   @key[function] @AdaSubDefn{Current_Output}  @key[return] File_Type;
   @key[function] @AdaSubDefn{Current_Error}   @key[return] File_Type;

   @key[type] @AdaTypeDefn{File_Access} @key[is] @key[access] @key[constant] File_Type;

   @key[function] @AdaSubDefn{Standard_Input}  @key[return] File_Access;
   @key[function] @AdaSubDefn{Standard_Output} @key[return] File_Access;
   @key[function] @AdaSubDefn{Standard_Error}  @key[return] File_Access;

   @key[function] @AdaSubDefn{Current_Input}   @key[return] File_Access;
   @key[function] @AdaSubDefn{Current_Output}  @key[return] File_Access;
   @key[function] @AdaSubDefn{Current_Error}   @key[return] File_Access;


@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0051],ARef=[AI95-00057-01]}
--@RI{Buffer control}
   @key[procedure] @AdaSubDefn{Flush} (File : @key[in] @Chg{New=[],Old=[@key[out] ]}File_Type);
   @key[procedure] @AdaSubDefn{Flush};


   --@RI{ Specification of line and page lengths}

   @key[procedure] @AdaSubDefn{Set_Line_Length}(File : @key[in] File_Type; To : @key[in] Count);
   @key[procedure] @AdaSubDefn{Set_Line_Length}(To   : @key[in] Count);

   @key[procedure] @AdaSubDefn{Set_Page_Length}(File : @key[in] File_Type; To : @key[in] Count);
   @key[procedure] @AdaSubDefn{Set_Page_Length}(To   : @key[in] Count);

   @key[function]  @AdaSubDefn{Line_Length}(File : @key[in] File_Type) @key[return] Count;
   @key[function]  @AdaSubDefn{Line_Length} @key[return] Count;

   @key[function]  @AdaSubDefn{Page_Length}(File : @key[in] File_Type) @key[return] Count;
   @key[function]  @AdaSubDefn{Page_Length} @key[return] Count;

   --@RI{ Column, Line, and Page Control}

   @key[procedure] @AdaSubDefn{New_Line}   (File    : @key[in] File_Type;
                         Spacing : @key[in] Positive_Count := 1);
   @key[procedure] @AdaSubDefn{New_Line}   (Spacing : @key[in] Positive_Count := 1);

   @key[procedure] @AdaSubDefn{Skip_Line}  (File    : @key[in] File_Type;
                         Spacing : @key[in] Positive_Count := 1);
   @key[procedure] @AdaSubDefn{Skip_Line}  (Spacing : @key[in] Positive_Count := 1);

   @key[function]  @AdaSubDefn{End_Of_Line}(File : @key[in] File_Type) @key[return] Boolean;
   @key[function]  @AdaSubDefn{End_Of_Line} @key[return] Boolean;

   @key[procedure] @AdaSubDefn{New_Page}   (File : @key[in] File_Type);
   @key[procedure] @AdaSubDefn{New_Page};

   @key[procedure] @AdaSubDefn{Skip_Page}  (File : @key[in] File_Type);
   @key[procedure] @AdaSubDefn{Skip_Page};

   @key[function]  @AdaSubDefn{End_Of_Page}(File : @key[in] File_Type) @key[return] Boolean;
   @key[function]  @AdaSubDefn{End_Of_Page} @key[return] Boolean;

   @key[function]  @AdaSubDefn{End_Of_File}(File : @key[in] File_Type) @key[return] Boolean;
   @key[function]  @AdaSubDefn{End_Of_File} @key[return] Boolean;

   @key[procedure] @AdaSubDefn{Set_Col} (File : @key[in] File_Type; To : @key[in] Positive_Count);
   @key[procedure] @AdaSubDefn{Set_Col} (To   : @key[in] Positive_Count);

   @key[procedure] @AdaSubDefn{Set_Line}(File : @key[in] File_Type; To : @key[in] Positive_Count);
   @key[procedure] @AdaSubDefn{Set_Line}(To   : @key[in] Positive_Count);

   @key[function] @AdaSubDefn{Col} (File : @key[in] File_Type) @key[return] Positive_Count;
   @key[function] @AdaSubDefn{Col}  @key[return] Positive_Count;

   @key[function] @AdaSubDefn{Line}(File : @key[in] File_Type) @key[return] Positive_Count;
   @key[function] @AdaSubDefn{Line} @key[return] Positive_Count;

   @key[function] @AdaSubDefn{Page}(File : @key[in] File_Type) @key[return] Positive_Count;
   @key[function] @AdaSubDefn{Page} @key[return] Positive_Count;

   --@RI{ Character Input-Output}

   @key[procedure] @AdaSubDefn{Get}(File : @key[in]  File_Type; Item : @key[out] Character);
   @key[procedure] @AdaSubDefn{Get}(Item : @key[out] Character);

   @key[procedure] @AdaSubDefn{Put}(File : @key[in]  File_Type; Item : @key[in] Character);
   @key[procedure] @AdaSubDefn{Put}(Item : @key[in]  Character);

   @key[procedure] @AdaSubDefn{Look_Ahead} (File        : @key[in]  File_Type;
                         Item        : @key[out] Character;
                         End_Of_Line : @key[out] Boolean);
   @key[procedure] @AdaSubDefn{Look_Ahead} (Item        : @key[out] Character;
                         End_Of_Line : @key[out] Boolean);

   @key[procedure] @AdaSubDefn{Get_Immediate}(File      : @key[in]  File_Type;
                           Item      : @key[out] Character);
   @key[procedure] @AdaSubDefn{Get_Immediate}(Item      : @key[out] Character);

   @key[procedure] @AdaSubDefn{Get_Immediate}(File      : @key[in]  File_Type;
                           Item      : @key[out] Character;
                           Available : @key[out] Boolean);
   @key[procedure] @AdaSubDefn{Get_Immediate}(Item      : @key[out] Character;
                           Available : @key[out] Boolean);

   --@RI{ String Input-Output}

   @key[procedure] @AdaSubDefn{Get}(File : @key[in]  File_Type; Item : @key[out] String);
   @key[procedure] @AdaSubDefn{Get}(Item : @key[out] String);

   @key[procedure] @AdaSubDefn{Put}(File : @key[in]  File_Type; Item : @key[in] String);
   @key[procedure] @AdaSubDefn{Put}(Item : @key[in]  String);

   @key[procedure] @AdaSubDefn{Get_Line}(File : @key[in]  File_Type;
                      Item : @key[out] String;
                      Last : @key[out] Natural);
   @key[procedure] @AdaSubDefn{Get_Line}(Item : @key[out] String; Last : @key[out] Natural);

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Get_Line}(File : @key[in]  File_Type) @key[return] String;
   @key[function] @AdaSubDefn{Get_Line} @key[return] String;]}

   @key[procedure] @AdaSubDefn{Put_Line}(File : @key[in]  File_Type; Item : @key[in] String);
   @key[procedure] @AdaSubDefn{Put_Line}(Item : @key[in]  String);

--@RI{ Generic packages for Input-Output of Integer Types}

   @key[generic]
      @key[type] Num @key[is] @key[range] <>;
   @key[package] @AdaPackDefn{Integer_IO} @key[is]

      @AdaObjDefn{Default_Width} : Field := Num'Width;
      @AdaObjDefn{Default_Base}  : Number_Base := 10;

      @key[procedure] @AdaSubDefn{Get}(File  : @key[in]  File_Type;
                    Item  : @key[out] Num;
                    Width : @key[in] Field := 0);
      @key[procedure] @AdaSubDefn{Get}(Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);

      @key[procedure] @AdaSubDefn{Put}(File  : @key[in] File_Type;
                    Item  : @key[in] Num;
                    Width : @key[in] Field := Default_Width;
                    Base  : @key[in] Number_Base := Default_Base);
      @key[procedure] @AdaSubDefn{Put}(Item  : @key[in] Num;
                    Width : @key[in] Field := Default_Width;
                    Base  : @key[in] Number_Base := Default_Base);
      @key[procedure] @AdaSubDefn{Get}(From : @key[in]  String;
                    Item : @key[out] Num;
                    Last : @key[out] Positive);
      @key[procedure] @AdaSubDefn{Put}(To   : @key[out] String;
                    Item : @key[in] Num;
                    Base : @key[in] Number_Base := Default_Base);

   @key[end] Integer_IO;

   @key[generic]
      @key[type] Num @key[is] @key[mod] <>;
   @key[package] @AdaPackDefn{Modular_IO} @key[is]

      @AdaObjDefn{Default_Width} : Field := Num'Width;
      @AdaObjDefn{Default_Base}  : Number_Base := 10;

      @key[procedure] @AdaSubDefn{Get}(File  : @key[in]  File_Type;
                    Item  : @key[out] Num;
                    Width : @key[in] Field := 0);
      @key[procedure] @AdaSubDefn{Get}(Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);

      @key[procedure] @AdaSubDefn{Put}(File  : @key[in] File_Type;
                    Item  : @key[in] Num;
                    Width : @key[in] Field := Default_Width;
                    Base  : @key[in] Number_Base := Default_Base);
      @key[procedure] @AdaSubDefn{Put}(Item  : @key[in] Num;
                    Width : @key[in] Field := Default_Width;
                    Base  : @key[in] Number_Base := Default_Base);
      @key[procedure] @AdaSubDefn{Get}(From : @key[in]  String;
                    Item : @key[out] Num;
                    Last : @key[out] Positive);
      @key[procedure] @AdaSubDefn{Put}(To   : @key[out] String;
                    Item : @key[in] Num;
                    Base : @key[in] Number_Base := Default_Base);

   @key[end] Modular_IO;

   --@RI{ Generic packages for Input-Output of Real Types}

   @key[generic]
      @key[type] Num @key[is] @key[digits] <>;
   @key[package] @AdaPackDefn{Float_IO} @key[is]

      @AdaObjDefn{Default_Fore} : Field := 2;
      @AdaObjDefn{Default_Aft}  : Field := Num'Digits-1;
      @AdaObjDefn{Default_Exp}  : Field := 3;

      @key[procedure] @AdaSubDefn{Get}(File  : @key[in]  File_Type;
                    Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);
      @key[procedure] @AdaSubDefn{Get}(Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);

      @key[procedure] @AdaSubDefn{Put}(File : @key[in] File_Type;
                    Item : @key[in] Num;
                    Fore : @key[in] Field := Default_Fore;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);
      @key[procedure] @AdaSubDefn{Put}(Item : @key[in] Num;
                    Fore : @key[in] Field := Default_Fore;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);

      @key[procedure] @AdaSubDefn{Get}(From : @key[in] String;
                    Item : @key[out] Num;
                    Last : @key[out] Positive);
      @key[procedure] @AdaSubDefn{Put}(To   : @key[out] String;
                    Item : @key[in] Num;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);
   @key[end] Float_IO;

   @key[generic]
      @key[type] Num @key[is] @key[delta] <>;
   @key[package] @AdaPackDefn{Fixed_IO} @key[is]

      @AdaObjDefn{Default_Fore} : Field := Num'Fore;
      @AdaObjDefn{Default_Aft}  : Field := Num'Aft;
      @AdaObjDefn{Default_Exp}  : Field := 0;

      @key[procedure] @AdaSubDefn{Get}(File  : @key[in]  File_Type;
                    Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);
      @key[procedure] @AdaSubDefn{Get}(Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);

      @key[procedure] @AdaSubDefn{Put}(File : @key[in] File_Type;
                    Item : @key[in] Num;
                    Fore : @key[in] Field := Default_Fore;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);
      @key[procedure] @AdaSubDefn{Put}(Item : @key[in] Num;
                    Fore : @key[in] Field := Default_Fore;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);

      @key[procedure] @AdaSubDefn{Get}(From : @key[in]  String;
                    Item : @key[out] Num;
                    Last : @key[out] Positive);
      @key[procedure] @AdaSubDefn{Put}(To   : @key[out] String;
                    Item : @key[in] Num;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);
   @key[end] Fixed_IO;

   @key[generic]
      @key[type] Num @key[is] @key[delta] <> @key[digits] <>;
   @key[package] @AdaPackDefn{Decimal_IO} @key[is]

      @AdaObjDefn{Default_Fore} : Field := Num'Fore;
      @AdaObjDefn{Default_Aft}  : Field := Num'Aft;
      @AdaObjDefn{Default_Exp}  : Field := 0;

      @key[procedure] @AdaSubDefn{Get}(File  : @key[in]  File_Type;
                    Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);
      @key[procedure] @AdaSubDefn{Get}(Item  : @key[out] Num;
                    Width : @key[in]  Field := 0);

      @key[procedure] @AdaSubDefn{Put}(File : @key[in] File_Type;
                    Item : @key[in] Num;
                    Fore : @key[in] Field := Default_Fore;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);
      @key[procedure] @AdaSubDefn{Put}(Item : @key[in] Num;
                    Fore : @key[in] Field := Default_Fore;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);

      @key[procedure] @AdaSubDefn{Get}(From : @key[in]  String;
                    Item : @key[out] Num;
                    Last : @key[out] Positive);
      @key[procedure] @AdaSubDefn{Put}(To   : @key[out] String;
                    Item : @key[in] Num;
                    Aft  : @key[in] Field := Default_Aft;
                    Exp  : @key[in] Field := Default_Exp);
   @key[end] Decimal_IO;

   --@RI{ Generic package for Input-Output of Enumeration Types}

   @key[generic]
      @key[type] Enum @key[is] (<>);
   @key[package] @AdaPackDefn{Enumeration_IO} @key[is]

      @AdaObjDefn{Default_Width}   : Field := 0;
      @AdaObjDefn{Default_Setting} : Type_Set := Upper_Case;

      @key[procedure] @AdaSubDefn{Get}(File : @key[in]  File_Type;
                    Item : @key[out] Enum);
      @key[procedure] @AdaSubDefn{Get}(Item : @key[out] Enum);

      @key[procedure] @AdaSubDefn{Put}(File  : @key[in] File_Type;
                    Item  : @key[in] Enum;
                    Width : @key[in] Field    := Default_Width;
                    Set   : @key[in] Type_Set := Default_Setting);
      @key[procedure] @AdaSubDefn{Put}(Item  : @key[in] Enum;
                    Width : @key[in] Field    := Default_Width;
                    Set   : @key[in] Type_Set := Default_Setting);

      @key[procedure] @AdaSubDefn{Get}(From : @key[in]  String;
                    Item : @key[out] Enum;
                    Last : @key[out] Positive);
      @key[procedure] @AdaSubDefn{Put}(To   : @key[out] String;
                    Item : @key[in]  Enum;
                    Set  : @key[in]  Type_Set := Default_Setting);
   @key[end] Enumeration_IO;

@keepnext@;--@RI{ Exceptions}

   @AdaExcDefn{Status_Error} : @key[exception] @key[renames] IO_Exceptions.Status_Error;
   @AdaExcDefn{Mode_Error}   : @key[exception] @key[renames] IO_Exceptions.Mode_Error;
   @AdaExcDefn{Name_Error}   : @key[exception] @key[renames] IO_Exceptions.Name_Error;
   @AdaExcDefn{Use_Error}    : @key[exception] @key[renames] IO_Exceptions.Use_Error;
   @AdaExcDefn{Device_Error} : @key[exception] @key[renames] IO_Exceptions.Device_Error;
   @AdaExcDefn{End_Error}    : @key[exception] @key[renames] IO_Exceptions.End_Error;
   @AdaExcDefn{Data_Error}   : @key[exception] @key[renames] IO_Exceptions.Data_Error;
   @AdaExcDefn{Layout_Error} : @key[exception] @key[renames] IO_Exceptions.Layout_Error;
@key[private]
   ... -- @RI{not specified by the language}
@key[end] Ada.Text_IO;
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Text=[The type File_Type
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@end{StaticSem}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
Append_File is a new element of enumeration type File_Mode.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Get_Immediate, Look_Ahead, the subprograms for dealing with
standard error, the type File_Access and its associated subprograms,
and the
generic packages Modular_IO and Decimal_IO are new in Ada 95.
@end{Extend83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @B[Amendment Correction:] Text_IO.File_Type is defined to need finalization. If the
  restriction No_Nested_Finalization (see @RefSecNum{Tasking Restrictions})
  applies to the partition, and File_Type does not have a controlled part, it
  will not be allowed in local objects in Ada 2005 whereas it would be allowed
  in original Ada 95. Such code is not portable, as another Ada compiler
  may have a controlled part in File_Type, and thus would be illegal.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0051],ARef=[AI95-00057-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the parameter mode
  of Flush; otherwise it could not be used on Standard_Output.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[The Text_IO.Get_Line functions are new;
  they are described in @RefSec{Input-Output of Characters and Strings}.]}
@end{DiffWord95}


@LabeledSubClause{Text File Management}

@begin{StaticSem}
@Leading@;The only allowed file modes for text files are the modes In_File,
Out_File, and Append_File.
The subprograms given in subclause @RefSecNum{File Management} for the control of
external files, and the function End_Of_File given in subclause
@RefSecNum{Sequential Input-Output Operations} for
sequential input-output, are also available for text files. There is
also a version of End_Of_File that refers to the current default input
file. For text files, the procedures have the following additional
effects:
@begin{Itemize}
     For the procedures Create and Open: After a file with mode Out_File or
     Append_File is opened, the page length and line length are unbounded (both have
     the conventional value zero). After a file (of any mode)
     is opened, the current column, current line, and current
     page numbers are set to one. If the mode is Append_File, it
     is implementation defined whether a page terminator will separate
     preexisting text in the file from the new text to be written.
@begin{Reason}
For a file with mode Append_File, although it may seem more
sensible for Open to set the current column, line, and page number based on
the number of pages in the file, the number of lines on the last
page, and the number of columns in the last line, we rejected this
approach because of implementation costs; it would require
the implementation to scan the file before doing the append,
or to do processing that would be equivalent in effect.

For similar reasons, there is no requirement to erase
the last page terminator of the file, nor to insert
an explicit page terminator in the case when the final
page terminator of a file is represented implicitly by the
implementation.
@end{Reason}

     For the procedure Close: If the file has the current mode Out_File
     or Append_File, has the effect of calling New_Page, unless the current
     page is already terminated; then outputs a file terminator.


  For the procedure Reset: If the file has the current mode Out_File
  or Append_File, has the effect of calling New_Page, unless the
  current page is already terminated; then outputs a file terminator.
  The current column, line, and page numbers are set to
  one, and the line and page lengths to Unbounded.
  If the new mode is Append_File, it is implementation defined whether
  a page terminator will separate preexisting text in the file from the
  new text to be written.
@begin{Reason}
The behavior of Reset should be similar to closing a file and
       reopening it with the given mode@end{reason}
@end{Itemize}

The exception Mode_Error is propagated by the procedure Reset upon an
attempt to change the mode of a file that is
the current default
input file, the current default output file,
or the current default error file.
@end{StaticSem}

@begin{Notes}
@Leading@;An implementation can define the Form parameter of
Create and Open to control effects including the following:
@begin{itemize}
the interpretation of line and column numbers for an interactive file,
and

the interpretation of text formats in
a file created by a foreign program.
@end{itemize}
@end{Notes}

@LabeledSubClause{Default Input, Output, and Error Files}

@begin{StaticSem}
The following subprograms provide for the control of the particular
default files that are used when a file parameter is omitted from a Get,
Put, or other operation of text input-output described below,
or when application-dependent error-related text is to be output.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Set_Input(File : @key[in] File_Type);
@end{Example}
  Operates on a file of mode In_File. Sets the current default
  input file to File.

  @Trailing@;The exception Status_Error is propagated if the given file is not
  open. The exception Mode_Error is propagated if the mode of the
  given file is not In_File.

@begin{Example}@Keepnext
@key[procedure] Set_Output(File : @key[in] File_Type);
@key[procedure] Set_Error (File : @key[in] File_Type);
@end{Example}
  @Trailing@;Each operates on a file of mode Out_File or Append_File.
  Set_Output sets the current default output file to File.
  Set_Error sets the current default error file to File.
  The exception Status_Error is propagated if the given file is not
  open. The exception Mode_Error is propagated if the mode of the
  given file is not Out_File or Append_File.

@begin{Example}@Keepnext
@key[function] Standard_Input @key[return] File_Type;
@key[function] Standard_Input @key[return] File_Access;
@end{Example}
  @Trailing@;Returns the standard input file (see @RefSecNum{Text Input-Output}),
  or an access value designating the standard input file, respectively.

@begin{Example}@Keepnext
@key[function] Standard_Output @key[return] File_Type;
@key[function] Standard_Output @key[return] File_Access;
@end{Example}
  @Trailing@;Returns the standard output file (see @RefSecNum{Text Input-Output})
  or an access value designating the standard output file, respectively.

@begin{Example}@Keepnext
@key[function] Standard_Error @key[return] File_Type;
@key[function] Standard_Error @key[return] File_Access;
@end{Example}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0052],ARef=[AI95-00194-01]}
  @Trailing@;Returns the standard error file (see @RefSecNum{Text Input-Output}),
  or an access value designating the standard @Chg{New=[error],Old=[output]} file, respectively.

@end{DescribeCode}
@Comment{The following paragraph was originally in a DescribeCode section; but
that clearly was not intended; I've fixed it. (This changes the indentation of
the paragraph in the old version too, but the change is harmless.) RLB-21-08-2000}
@Trailing@;The Form strings implicitly associated with the opening of
Standard_Input, Standard_Output, and
Standard_Error at the start of program execution are implementation defined.
@begin{DescribeCode}

@begin{Example}@Keepnext
@key[function] Current_Input @key[return] File_Type;
@key[function] Current_Input @key[return] File_Access;
@end{Example}
  @Trailing@;Returns the current default input file,
  or an access value designating the current default input file,
  respectively.

@begin{Example}@Keepnext
@key[function] Current_Output @key[return] File_Type;
@key[function] Current_Output @key[return] File_Access;
@end{Example}
  @Trailing@;Returns the current default output file,
  or an access value designating the current default output file,
  respectively.

@begin{Example}@Keepnext
@key[function] Current_Error @key[return] File_Type;
@key[function] Current_Error @key[return] File_Access;
@end{Example}
  @Trailing@;Returns the current default error file,
  or an access value designating the current default error file,
  respectively.

@begin{Example}@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0051],ARef=[AI95-00057-01]}@Keepnext
@key[procedure] Flush (File : @key[in] @Chg{New=[],Old=[@key[out] ]}File_Type);
@key[procedure] Flush;
@end{Example}
The effect of Flush is the same as the corresponding subprogram
in Streams.Stream_IO (see @RefSecNum[The Package Streams.Stream_IO]).
If File is not explicitly specified, Current_Output is used.
@end{DescribeCode}
@end{StaticSem}

@begin{Erron}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0053],ARef=[AI95-00063-01]}
@PDefn2{Term=(erroneous execution),Sec=(cause)}
The execution of a program is erroneous if it @Chg{New=[invokes an operation on],
Old=[attempts to use]} a current default
input, default output, or default error file@Chg{New=[, and if the
corresponding file object is closed or],Old=[ that]} no longer exists.
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0053],ARef=[AI95-00063-01]}
@ChgAdded{Version=[1],Text=[Closing a default file, then setting the default
file to another open file before accessing it is not erroneous.]}
@end{Ramification}

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0053],ARef=[AI95-00063-01]}
@ChgDeleted{Version=[1],Text=[If the Close operation is applied to a file
object that is also serving as the default input, default output, or default
error file, then subsequent operations on such a default file are erroneous.]}
@end{Erron}

@begin{Notes}
The standard input, standard output, and standard error
files cannot be opened, closed, reset, or deleted, because the
parameter File of the corresponding procedures has the mode @key[in] @key[out].

The standard input, standard output, and standard error files are different
file objects, but not necessarily different external files.
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0051],ARef=[AI95-00057-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the parameter mode
  of Flush; otherwise it could not be used on Standard_Output.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0052],ARef=[AI95-00194-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected Standard_Error so it
  refers to the correct file.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0053],ARef=[AI95-00063-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that execution
  is erroneous only when a closed default file is accessed.]}
@end{DiffWord95}


@LabeledSubClause{Specification of Line and Page Lengths}

@begin{StaticSem}
The subprograms described in this subclause are concerned with the line
and page structure of a file of mode Out_File or Append_File. They operate
either on
the file given as the first parameter, or, in the absence of such a file
parameter, on the current default output file. They provide for output
of text with a specified maximum line length or page length. In these
cases, line and page terminators are output implicitly and automatically
when needed. When line and page lengths are unbounded (that is, when
they have the conventional value zero), as in the case of a newly opened
file, new lines and new pages are only started when explicitly called
for.

In all cases, the exception Status_Error is propagated if the file to be
used is not open; the exception Mode_Error is propagated if the mode of the
file is not Out_File or Append_File.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Set_Line_Length(File : @key[in] File_Type; To : @key[in] Count);
@key[procedure] Set_Line_Length(To   : @key[in] Count);
@end{Example}
  Sets the maximum line length of the specified output or
  append file to the number of characters specified by To. The value zero for
  To specifies an unbounded line length.
@begin{Ramification}
The setting
does not affect the lengths of lines in the existing file, rather it only
influences subsequent output operations.@end{ramification}

  @Trailing@;The exception Use_Error is propagated if the specified line length
  is inappropriate for the associated external file.

@begin{Example}@Keepnext
@key[procedure] Set_Page_Length(File : @key[in] File_Type; To : @key[in] Count);
@key[procedure] Set_Page_Length(To   : @key[in] Count);
@end{Example}
  Sets the maximum page length of the specified output or append file to
  the number of lines specified by To. The value zero for To
  specifies an unbounded page length.

  @Trailing@;The exception Use_Error is propagated if the specified page length
  is inappropriate for the associated external file.

@begin{Example}@Keepnext
@key[function] Line_Length(File : @key[in] File_Type) @key[return] Count;
@key[function] Line_Length @key[return] Count;
@end{Example}
  @Trailing@;Returns the maximum line length currently set for the
  specified output or append file, or zero if the line length is unbounded.

@begin{Example}@Keepnext
@key[function] Page_Length(File : @key[in] File_Type) @key[return] Count;
@key[function] Page_Length @key[return] Count;
@end{Example}
  Returns the maximum page length currently set for the
  specified output or append file, or zero if the page length is unbounded.
@end{DescribeCode}
@end{StaticSem}

@LabeledSubClause{Operations on Columns, Lines, and Pages}

@begin{StaticSem}
The subprograms described in this subclause provide for explicit control
of line and page structure; they operate either on the file given as the
first parameter, or, in the absence of such a file parameter, on the
appropriate (input or output) current default file. The exception
Status_Error is propagated by any of these subprograms if the file to be
used is not open.
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] New_Line(File : @key[in] File_Type; Spacing : @key[in] Positive_Count := 1);
@key[procedure] New_Line(Spacing : @key[in] Positive_Count := 1);
@end{Example}

  Operates on a file of mode Out_File or Append_File.

  For a Spacing of one: Outputs a line terminator and sets the
  current column number to one. Then increments the current
  line number by one, except in the case that the current line
  number is already greater than or equal to the maximum page
  length, for a bounded page length; in that case a page
  terminator is output, the current page number is incremented
  by one, and the current line number is set to one.

  For a Spacing greater than one, the above actions are
  performed Spacing times.

  @Trailing@;The exception Mode_Error is propagated if the mode is not
  Out_File or Append_File.

@begin{Example}@Keepnext
@key[procedure] Skip_Line(File  : @key[in] File_Type; Spacing : @key[in] Positive_Count := 1);
@key[procedure] Skip_Line(Spacing : @key[in] Positive_Count := 1);
@end{Example}

  Operates on a file of mode In_File.

  For a Spacing of one: Reads and discards all characters until
  a line terminator has been read, and then sets the current
  column number to one. If the line terminator is not
  immediately followed by a page terminator, the current line
  number is incremented by one. Otherwise, if the line
  terminator is immediately followed by a page terminator, then
  the page terminator is skipped, the current page number is
  incremented by one, and the current line number is set to one.

  For a Spacing greater than one, the above actions are
  performed Spacing times.

  @Trailing@;The exception Mode_Error is propagated if the mode is not In_File.
  The exception End_Error is propagated if an attempt is made to
  read a file terminator.

@begin{Example}@Keepnext
@key[function] End_Of_Line(File : @key[in] File_Type) @key[return] Boolean;
@key[function] End_Of_Line @key[return] Boolean;
@end{Example}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
  Operates on a file of mode In_File. Returns True if a line
  terminator or a file terminator is next;
  otherwise@Chg{Version=[3],New=[,],Old=[]} returns False.

  @Trailing@;The exception Mode_Error is propagated if the mode is not In_File.

@begin{Example}@Keepnext
@key[procedure] New_Page(File : @key[in] File_Type);
@key[procedure] New_Page;
@end{Example}

  Operates on a file of mode Out_File or Append_File. Outputs a line
  terminator if the current line is not terminated, or if the
  current page is empty (that is, if the current column and line
  numbers are both equal to one). Then outputs a page
  terminator, which terminates the current page. Adds one to
  the current page number and sets the current column and line
  numbers to one.

  @Trailing@;The exception Mode_Error is propagated if the mode is not
  Out_File or Append_File.

@begin{Example}@Keepnext
@key[procedure] Skip_Page(File : @key[in] File_Type);
@key[procedure] Skip_Page;
@end{Example}
  Operates on a file of mode In_File. Reads and discards all
  characters and line terminators until a page terminator has
  been read. Then adds one to the current page number, and sets
  the current column and line numbers to one.

  @Trailing@;The exception Mode_Error is propagated if the mode is not In_File.
  The exception End_Error is propagated if an attempt is made to
  read a file terminator.

@begin{Example}@Keepnext
@key[function] End_Of_Page(File : @key[in] File_Type) @key[return] Boolean;
@key[function] End_Of_Page @key[return] Boolean;
@end{Example}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
  Operates on a file of mode In_File. Returns True if the
  combination of a line terminator and a page terminator is
  next, or if a file terminator is next;
  otherwise@Chg{Version=[3],New=[,],Old=[]} returns False.

  @Trailing@;The exception Mode_Error is propagated if the mode is not In_File.

@begin{Example}@Keepnext
@key[function] End_Of_File(File : @key[in] File_Type) @key[return] Boolean;
@key[function] End_Of_File @key[return] Boolean;
@end{Example}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
  Operates on a file of mode In_File. Returns True if a file
  terminator is next, or if the combination of a line, a page,
  and a file terminator is next;
  otherwise@Chg{Version=[3],New=[,],Old=[]} returns False.

  @Trailing@;The exception Mode_Error is propagated if the mode is not In_File.

@end{DescribeCode}
@Comment{The following paragraph was originally in a DescribeCode section; but
that clearly was not intended; I've fixed it. (This changes the indentation of
the paragraph in the old version too, but the change is harmless.) RLB-21-08-2000}
The following subprograms provide for the control of the current
position of reading or writing in a file. In all cases, the default
file is the current output file.
@begin{DescribeCode}

@begin{Example}@Keepnext
@key[procedure] Set_Col(File : @key[in] File_Type; To : @key[in] Positive_Count);
@key[procedure] Set_Col(To   : @key[in] Positive_Count);
@end{Example}
  @Leading@;If the file mode is Out_File or Append_File:
@begin{itemize}
               If the value specified by To is greater than the current
               column number, outputs spaces, adding one to the current
               column number after each space, until the current column
               number equals the specified value. If the value
               specified by To is equal to the current column number,
               there is no effect. If the value specified by To is less
               than the current column number, has the effect of calling
               New_Line (with a spacing of one), then outputs (To @en 1)
               spaces, and sets the current column number to the
               specified value.

               The exception Layout_Error is propagated if the value
               specified by To exceeds Line_Length when the line length
               is bounded (that is, when it does not have the
               conventional value zero).
@end{itemize}

  @Leading@;If the file mode is In_File:
@begin{itemize}
               Reads (and discards) individual characters, line
               terminators, and page terminators, until the next
               character to be read has a column number that equals the
               value specified by To; there is no effect if the current
               column number already equals this value. Each transfer
               of a character or terminator maintains the current
               column, line, and page numbers in the same way as a Get
               procedure (see @RefSecNum{Get and Put Procedures}).
               (Short lines will be skipped
               until a line is reached that has a character at the
               specified column position.)

    @Trailing@;The exception End_Error is propagated if an attempt is made
    to read a file terminator.
@end{itemize}

@begin{Example}@Keepnext
@key[procedure] Set_Line(File : @key[in] File_Type; To : @key[in] Positive_Count);
@key[procedure] Set_Line(To   : @key[in] Positive_Count);
@end{Example}

  @Leading@;If the file mode is Out_File or Append_File:
@begin{itemize}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0038-1]}
               If the value specified by To is greater than the current
               line number, has the effect of repeatedly calling
               New_Line (with a spacing of one), until the current line
               number equals the specified value. If the value
               specified by To is equal to the current line number,
               there is no effect. If the value specified by To is less
               than the current line number, has the effect of calling
               New_Page followed@Chg{Version=[3],New=[, if To is greater
               than 1,],Old=[]} by a call of New_Line with a spacing
               equal to (To @en 1).

               The exception Layout_Error is propagated if the value
               specified by To exceeds Page_Length when the page length
               is bounded (that is, when it does not have the
               conventional value zero).
@end{itemize}

  @Leading@;If the mode is In_File:
@begin{itemize}
               Has the effect of repeatedly calling Skip_Line (with a
               spacing of one), until the current line number equals the
               value specified by To; there is no effect if the current
               line number already equals this value. (Short pages will
               be skipped until a page is reached that has a line at the
               specified line position.)

    @Trailing@;The exception End_Error is propagated if an attempt is made
    to read a file terminator.
@end{itemize}
@begin{Example}@Keepnext
@key[function] Col(File : @key[in] File_Type) @key[return] Positive_Count;
@key[function] Col @key[return] Positive_Count;
@end{Example}

  Returns the current column number.

  @Trailing@;The exception Layout_Error is propagated if this number exceeds
  Count'Last.

@begin{Example}@Keepnext
@key[function] Line(File : @key[in] File_Type) @key[return] Positive_Count;
@key[function] Line @key[return] Positive_Count;
@end{Example}

          Returns the current line number.

  @Trailing@;The exception Layout_Error is propagated if this number exceeds
  Count'Last.

@begin{Example}@Keepnext
@key[function] Page(File : @key[in] File_Type) @key[return] Positive_Count;
@key[function] Page @key[return] Positive_Count;
@end{Example}
  Returns the current page number.

  @Trailing@;The exception Layout_Error is propagated if this number exceeds
  Count'Last.

@end{DescribeCode}

The column number, line number, or page number are allowed to exceed
Count'Last (as a consequence of the input or output of sufficiently many
characters, lines, or pages). These events do not cause any exception
to be propagated.
However, a call of Col, Line, or Page propagates the
exception Layout_Error if the corresponding number exceeds Count'Last.
@end{StaticSem}

@begin{Notes}
A page terminator is always skipped whenever the preceding line
terminator is skipped. An implementation may represent the combination
of these terminators by a single character, provided that it is properly
recognized on input.
@end{Notes}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0038-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction:>
  Fixed a glitch in Set_Line such that we could have called New_Line(0), which
  would have to raise Constraint_Error. It's now defined to work. The bug
  occurred in Ada 95 and Ada 2005. It's very unlikely that
  any real programs depend on this exception being raised.]}
@end{Inconsistent2005}


@LabeledSubClause{Get and Put Procedures}

@begin{StaticSem}
The procedures Get and Put for items of the type Character, String,
numeric types, and enumeration types are described in subsequent
subclauses.
Features of these procedures that are common to most of these
types are described in this subclause. The Get and Put procedures for
items of type Character and String deal with individual character
values; the Get and Put procedures for numeric and enumeration types
treat the items as lexical elements.

All procedures Get and Put have forms with a
file parameter, written
first. Where this parameter is omitted, the appropriate (input or
output) current default file is understood to be specified. Each
procedure Get operates on a
file of mode In_File. Each procedure Put
operates on a file of mode Out_File or Append_File.

All procedures Get and Put maintain the current column, line, and page
numbers of the specified file: the effect of each of these procedures
upon these numbers is the result of the effects of individual
transfers of characters and of individual output or skipping of
terminators. Each transfer of a character adds one to the current
column number. Each output of a line terminator sets the current column
number to one and adds one to the current line number. Each output of a
page terminator sets the current column and line numbers to one and adds
one to the current page number. For input, each skipping of a line
terminator sets the current column number to one and adds one to the
current line number; each skipping of a page terminator sets the current
column and line numbers to one and adds one to the current page number.
Similar considerations apply to the procedures Get_Line, Put_Line, and
Set_Col.

Several Get and Put procedures, for numeric and enumeration types, have
@i{format} parameters which specify field lengths; these parameters are of
the nonnegative subtype Field of the type Integer.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00223-01]}
@Defn2{Term=[blank], Sec=(in text input for enumeration and numeric types)}
Input-output of enumeration values uses the syntax of the corresponding
lexical elements. Any Get procedure for an enumeration type begins by
skipping any leading blanks, or line or page terminators. @Chg{Version=[2],
New=[A],Old=[Get procedures
for numeric or enumeration types start by skipping leading blanks, where
a]} @i{blank} is defined as a space or a horizontal tabulation character.
Next,
characters are input only so long as the sequence input is an initial
sequence of an identifier or of a character literal (in particular,
input ceases when a line terminator is encountered). The character or
line terminator that causes input to cease remains available for
subsequent input.

For a numeric type, the Get procedures have a format parameter called
Width. If the value given for this parameter is zero, the Get procedure
proceeds in the same manner as for enumeration types, but using the
syntax of numeric literals instead of that of enumeration literals. If
a nonzero value is given, then exactly Width characters are input, or
the characters up to a line terminator, whichever comes first; any
skipped leading blanks are included in the count. The syntax used for
numeric literals is an extended syntax that allows a leading sign (but
no intervening blanks, or line or page terminators)
and that also allows (for real types) an integer literal
as well as forms that have digits only before the point
or only after the point.

Any Put procedure, for an item of a numeric or an enumeration type,
outputs the value of the item as a numeric literal, identifier, or
character literal, as appropriate. This is preceded by leading spaces
if required by the format parameters Width or Fore (as described in
later subclauses), and then a minus sign for a negative value; for an
enumeration type, the spaces follow instead of leading. The format
given for a Put procedure is overridden if it is insufficiently wide,
by using the minimum needed width.

Two further cases arise for Put procedures for numeric and enumeration
types, if the line length of the specified output file is bounded (that
is, if it does not have the conventional value zero). If the number of
characters to be output does not exceed the maximum line length, but is
such that they cannot fit on the current line, starting from the current
column, then (in effect) New_Line is called (with a spacing of one)
before output of the item. Otherwise, if the number of characters
exceeds the maximum line length, then the exception Layout_Error is
propagated and nothing is output.

The exception Status_Error is propagated by any of the procedures Get,
Get_Line, Put, and Put_Line if the file to be used is not open. The
exception Mode_Error is propagated by the procedures Get and Get_Line if the
mode of the file to be used is not In_File; and by the procedures Put
and Put_Line, if the mode is not Out_File or Append_File.

The exception End_Error is propagated by a Get procedure if an attempt is
made to skip a file terminator. The exception Data_Error is propagated by a
Get procedure if the sequence finally input is not a lexical element
corresponding to the type, in particular if no characters were input;
for this test, leading blanks are ignored; for an item of a numeric
type, when a sign is input, this rule applies to the succeeding numeric
literal. The exception Layout_Error is propagated by a Put procedure that
outputs to a parameter of type String, if the length of the actual
string is insufficient for the output of the item.
@end{StaticSem}

@begin{Examples}
In the examples, here and in subclauses
@RefSecNum{Input-Output for Integer Types}
and @RefSecNum{Input-Output for Real Types}, the string
quotes and the lower case letter b are not transferred: they are shown
only to reveal the layout and spaces.

@begin{Example}
N : Integer;
   ...
Get(N);

@tabclear()@tabset(P4, P22, P38)
@RI[--  @\Characters at input @\Sequence input @\Value of N]
@Comment{Blank line.}
@RI[--  @\bb@en@|12535b @\@en@|12535 @\@en@|12535]
@RI[--  @\bb12_535e1b @\12_535e1 @\125350]
@RI[--  @\bb12_535e; @\12_535e @\(none) Data_Error raised]
@end{Example}

@begin{WideAbove}
@Leading@;Example of overridden width parameter:
@end{WideAbove}

@begin{Example}
Put(Item => -23, Width => 2);  --@RI{  "@en@|23"}
@end{Example}
@end{Examples}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00223-01]}
  @ChgAdded{Version=[2],Text=[Removed conflicting text describing the
  skipping of blanks for a Get procedure.]}
@end{DiffWord95}


@LabeledSubClause{Input-Output of Characters and Strings}

@begin{StaticSem}
@Leading@Keepnext@;For an item of type Character the following procedures are
provided:
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Get(File : @key[in] File_Type; Item : @key[out] Character);
@key[procedure] Get(Item : @key[out] Character);
@end{Example}
  After skipping any line terminators and any page terminators,
  reads the next character from the specified input file and
  returns the value of this character in the out parameter Item.

  @Trailing@;The exception End_Error is propagated if an attempt is made to
  skip a file terminator.

@begin{Example}@Keepnext
@key[procedure] Put(File : @key[in] File_Type; Item : @key[in] Character);
@key[procedure] Put(Item : @key[in] Character);
@end{Example}
  @Trailing@;If the line length of the specified output file is bounded
  (that is, does not have the conventional value zero), and the
  current column number exceeds it, has the effect of calling
  New_Line with a spacing of one. Then, or otherwise, outputs
  the given character to the file.

@begin{Example}@Keepnext
@key[procedure] Look_Ahead (File        : @key[in]  File_Type;
                      Item        : @key[out] Character;
                      End_Of_Line : @key[out] Boolean);
@key[procedure] Look_Ahead (Item        : @key[out] Character;
                      End_Of_Line : @key[out] Boolean);
@end{Example}
  @Trailing@ChgRef{Version=[1],Kind=[Revised]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0038-1],ARef=[AI05-0264-1]}@Chg{Version=[3],
  New=[Status_Error is propagated if the file is not open. ],Old=[]}Mode_Error
  is propagated if
  the mode of the file is not In_File. Sets End_Of_Line to True if at end of
  line, including if at end of page or at end of file; in each of these cases
  the value of Item is not specified.
  @PDefn{unspecified}
  Otherwise@Chg{Version=[3],New=[,],Old=[]} End_Of_Line is set to
  False and Item is set to @Chg{New=[],Old=[the ]}the next character (without
  consuming it) from the file.

@begin{Example}@Keepnext
@key[procedure] Get_Immediate(File : @key[in]  File_Type;
                        Item : @key[out] Character);
@key[procedure] Get_Immediate(Item : @key[out] Character);
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0038-1]}Reads
the next character, either control or graphic, from the specified
File or the default input file. @Chg{Version=[3],New=[Status_Error is
propagated if the file is not open. ],Old=[]}Mode_Error is propagated
if the mode of the
file is not In_File. End_Error is propagated if at the end of the file.
The current column, line and page numbers for the file are not affected.

@begin{Example}@Keepnext
@key[procedure] Get_Immediate(File      : @key[in]  File_Type;
                        Item      : @key[out] Character;
                        Available : @key[out] Boolean);
@key[procedure] Get_Immediate(Item      : @key[out] Character;
                        Available : @key[out] Boolean);
@end{Example}
@Trailing@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0038-1]}If
a character, either control or graphic, is available from the
specified File or the default input file, then the character is read;
Available is True and Item contains the value of this character. If a character
is not available, then Available is False and the value of
Item is not specified.
@PDefn{unspecified}
@Chg{Version=[3],New=[Status_Error is propagated if the file is
not open. ],Old=[]}Mode_Error is propagated if the mode of the
file is not In_File. End_Error is propagated if at the end of the file.
The current column, line and page numbers for the file are not affected.

@end{DescribeCode}
@Comment{The following paragraph was originally in a DescribeCode section; but
that clearly was not intended; I've fixed it. (This changes the indentation of
the paragraph in the old version too, but the change is harmless.) RLB-21-08-2000}
@Leading@;
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00301-01]}
For an item of type String the following @Chg{Version=[2],New=[subprograms],
Old=[procedures]} are provided:
@begin{DescribeCode}

@begin{Example}@Keepnext
@key[procedure] Get(File : @key[in] File_Type; Item : @key[out] String);
@key[procedure] Get(Item : @key[out] String);
@end{Example}
  @Trailing@;Determines the length of the given string and attempts that
  number of Get operations for successive characters of the
  string (in particular, no operation is performed if the string is null).

@begin{Example}@Keepnext
@key[procedure] Put(File : @key[in] File_Type; Item : @key[in] String);
@key[procedure] Put(Item : @key[in] String);
@end{Example}
  @Trailing@;Determines the length of the given string and attempts that
  number of Put operations for successive characters of the
  string (in particular, no operation is performed if the string is null).

@begin{Example}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Keepnext=[T],Text=[@key{function} Get_Line(File : @key{in} File_Type) @key{return} String;
@key{function} Get_Line @b<return> String;]}
@end{Example}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[Returns a result string
  constructed by reading successive characters from the specified input file,
  and assigning them to successive characters of the result string. The result
  string has a lower bound of 1 and an upper bound of the number of characters
  read. Reading stops when the end of the line is met; Skip_Line is then (in
  effect) called with a spacing of 1.]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Type=[Trailing],Text=[Constraint_Error is raised if the
  length of the line exceeds Positive'Last; in this case, the line number and page
  number are unchanged, and the column number is unspecified but no less than
  it was before the call.@PDefn{unspecified} The exception End_Error is
  propagated if an attempt is made to skip a file terminator.]}

  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
    @ChgAdded{Version=[2],Text=[Precisely what is left in the file is unspecified
    if Constraint_Error is raised because the line doesn't fit in a String; it
    should be consistent with column number. This allows implementers to use
    whatever buffering scheme makes sense. But the line terminator is not
    skipped in this case.]}
  @end{Ramification}

@begin{Example}@Keepnext
@key[procedure] Get_Line(File : @key[in] File_Type;
                   Item : @key[out] String;
                   Last : @key[out] Natural);
@key[procedure] Get_Line(Item : @key[out] String;
                   Last : @key[out] Natural);
@end{Example}
  Reads successive characters from the specified input file and assigns
  them to successive characters of the specified string.
  Reading stops if the end of the string is met. Reading also stops if
  the end of the line is met before meeting the end of the string;
  in this case Skip_Line is (in effect) called with a spacing of 1.
  @PDefn{unspecified}
  The values of characters not assigned are not specified.

  @Trailing@;If characters are read, returns in Last the index value such
  that Item(Last) is the last character assigned (the index of
  the first character assigned is Item'First). If no characters
  are read, returns in Last an index value that is one less than
  Item'First. The exception End_Error is propagated if an attempt
  is made to skip a file terminator.

@begin{Example}@Keepnext
@key[procedure] Put_Line(File : @key[in] File_Type; Item : @key[in] String);
@key[procedure] Put_Line(Item : @key[in] String);
@end{Example}
   Calls the procedure Put for the given string, and then the
   procedure New_Line with a spacing of one.
@end{DescribeCode}
@end{StaticSem}

@begin{ImplAdvice}
The Get_Immediate procedures should be implemented with unbuffered
input. For a device such as a keyboard, input should be @lquotes@;available@rquotes@;
if a key has already been typed, whereas for a disk file, input
should always be available except at end of file. For a file
associated with a keyboard-like device, any line-editing features
of the underlying operating system should be disabled during
the execution of Get_Immediate.
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[Get_Immediate should be implemented with unbuffered input; input
should be available immediately; line-editing should be disabled.]}]}
@end{ImplAdvice}

@begin{Notes}
Get_Immediate can be used to read a single key from the
keyboard @lquotes@;immediately@rquotes@;; that is, without waiting for an end of line.
In a call of Get_Immediate without the parameter Available,
the caller will wait until a character is available.

In a literal string parameter of Put, the enclosing string bracket
characters are not output. Each doubled string bracket character in the
enclosed string is output as a single string bracket character, as a
consequence of the rule for string literals (see @RefSecNum{String Literals}).

A string read by Get or written by Put can extend over several lines.
An implementation is allowed to assume that certain external files do
not contain page terminators, in which case Get_Line and Skip_Line can
return as soon as a line terminator is read.
@end{Notes}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  The Get_Line functions are @Chg{Version=[3],New=[],Old=[newly ]}added
  to Ada.Text_IO.
  If Ada.Text_IO is referenced in a @nt{use_clause}, and a function Get_Line
  is defined in a package that is also referenced in a @nt{use_clause}, the
  user-defined Get_Line may no longer be use-visible, resulting in errors.
  This should be rare and is easily fixed if it does occur.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The Text_IO.Get_Line functions are new.]}
@end{Extend95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0038-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added missing wording about
  raising Status_Error to Look_Ahead and Get_Immediate.]}
@end{DiffWord2005}



@LabeledSubClause{Input-Output for Integer Types}

@begin{StaticSem}
The following procedures are defined in the generic packages Integer_IO
and Modular_IO, which have to be
instantiated for the appropriate signed
integer or modular
type respectively
(indicated by Num in the specifications).

@Leading@;Values are output as decimal or based literals, without low line
characters or exponent, and, for Integer_IO, preceded by a minus sign if negative. The
format (which includes any leading spaces and minus sign) can be
specified by an optional field width parameter. Values of widths of
fields in output formats are of the nonnegative integer subtype Field.
Values of bases are of the integer subtype Number_Base.
@begin{Example}
@Trailing@key[subtype] Number_Base @key[is] Integer @key[range] 2 .. 16;
@end{Example}

@Leading@;The default field width and base to be used by output procedures are
defined by the following variables that are declared in the generic
packages Integer_IO and Modular_IO:

@begin{Example}
@Trailing@;Default_Width : Field := Num'Width;
Default_Base  : Number_Base := 10;
@end{Example}

@Leading@Keepnext@;The following procedures are provided:
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Get(File : @key[in] File_Type; Item : @key[out] Num; Width : @key[in] Field := 0);
@key[procedure] Get(Item : @key[out] Num; Width : @key[in] Field := 0);
@end{Example}
      If the value of the parameter Width is zero, skips any leading
      blanks, line terminators, or page terminators, then
      reads a plus sign if present or (for a signed type only)
      a minus sign if present, then reads the longest possible
      sequence of characters matching the syntax of a numeric
      literal without a point.
      If a nonzero value of Width is supplied, then exactly Width
      characters are input, or the characters (possibly none) up to
      a line terminator, whichever comes first; any skipped leading
      blanks are included in the count.

      Returns, in the parameter Item, the value of type Num
      that corresponds to the sequence input.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0038-1]}
      @Trailing@;The exception Data_Error is propagated if the sequence of characters
      read does not form a legal integer literal or if the value obtained is
      not of the subtype Num@Chg{Version=[3],New=[],Old=[ (for Integer_IO) or
      is not in the base range of Num (for Modular_IO)]}.

@begin{Example}@Keepnext
@key[procedure] Put(File  : @key[in] File_Type;
              Item  : @key[in] Num;
              Width : @key[in] Field := Default_Width;
              Base  : @key[in] Number_Base := Default_Base);
@comment{Blank Line}
@key[procedure] Put(Item  : @key[in] Num;
              Width : @key[in] Field := Default_Width;
              Base  : @key[in] Number_Base := Default_Base);
@end{Example}
          Outputs the value of the parameter Item as an integer literal,
          with no low lines, no exponent, and no leading zeros (but a
          single zero for the value zero), and a preceding minus sign
          for a negative value.

          If the resulting sequence of characters to be output has fewer
          than Width characters, then leading spaces are first output to
          make up the difference.

          @Trailing@;Uses the syntax for decimal literal if the parameter Base has
          the value ten (either explicitly or through Default_Base);
          otherwise, uses the syntax for based literal, with any letters
          in upper case.

@begin{Example}@Keepnext
@key[procedure] Get(From : @key[in] String; Item : @key[out] Num; Last : @key[out] Positive);
@end{Example}
          Reads an integer value from the beginning of the given string,
          following the same rules as the Get procedure that reads an
          integer value from a file, but treating the end of the string
          as a file terminator. Returns, in the parameter Item, the
          value of type Num that corresponds to the sequence input.
          Returns in Last the index value such that From(Last) is the
          last character read.

          @Trailing@;The exception Data_Error is propagated if the sequence input does
          not have the required syntax or if the value obtained is not
          of the subtype Num.

@begin{Example}@Keepnext
@key[procedure] Put(To   : @key[out] String;
              Item : @key[in] Num;
              Base : @key[in] Number_Base := Default_Base);
@end{Example}
          @Trailing@;Outputs the value of the parameter Item to the given string,
          following the same rule as for output to a file, using the
          length of the given string as the value for Width.
@end{DescribeCode}

@Leading@;Integer_Text_IO is a library package that is a nongeneric equivalent
to Text_IO.Integer_IO for the predefined type Integer:
@begin{Example}
@key[with] Ada.Text_IO;@ChildUnit{Parent=[Ada],Child=[Integer_@!Text_IO]}
@key[package] Ada.Integer_Text_IO @key[is] @key[new] Ada.Text_IO.Integer_IO(Integer);
@end{Example}

For each predefined signed integer type,
a nongeneric equivalent to Text_IO.Integer_IO is provided,
with names such as Ada.Long_Integer_Text_IO.


@end{StaticSem}

@begin{ImplPerm}

The nongeneric equivalent packages may, but need not,
be actual instantiations of the generic package for the appropriate
predefined type.

@end{ImplPerm}

@begin{Notes}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0038-1]}
@ChgDeleted{Version=[3],Text=[For Modular_IO, execution of Get propagates
Data_Error if the sequence of
characters read forms an integer literal outside the range
0..Num'Last.]}
@end{Notes}
@begin{NotIso}
@ChgAdded{Version=[3],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 24 and 25 were
deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered. This includes the next paragraph.}
@end{NotIso}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[1], Kind=[DeletedNoDelMsg]}
@ChgDeleted[Version=[1],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

@ChgRef{Version=[3], Kind=[Revised],ARef=[AI05-0298-1]}
@Chg{Version=[3],New=[@key[subtype] Byte_Int @key[is] Integer @key[range] -127 .. 127;
],Old=[]}@key[package] Int_IO @key[is] @key[new] Integer_IO(@Chg{Version=[3],New=[Byte_Int],Old=[Small_Int]}); @key[use] Int_IO;
--@RI{ default format used at instantiation,}
--@RI{ Default_Width = 4, Default_Base = 10}

Put(126);                            --@RI{ "b126"}
Put(-126, 7);                        --@RI{ "bbb@en@|126"}
Put(126, Width => 13, Base => 2);    --@RI{ "bbb2#1111110#"}
@end{Example}
@end{Examples}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0038-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction:>
  Changed wording to make Integer_IO and Modular_IO raise Data_Error in the same
  way when the bounds of the subtype are exceeded. There is no value to
  different behavior, and all surveyed compilers already treat integer and
  modular values the same way. This could only cause a problem if a
  program was compiled with some unsurveyed compiler, and the Ada 95-defined
  behavior is expected for Modular_IO. But note that such code is not portable
  anyway, as most widely used compilers behave consistently with the
  new wording, so it is unlikely that such code exists.]}
@end{Inconsistent2005}


@LabeledSubClause{Input-Output for Real Types}

@begin{StaticSem}
The following procedures are defined in the generic packages Float_IO,
Fixed_IO, and Decimal_IO, which have to be instantiated for the
appropriate floating point, ordinary fixed point, or decimal fixed point
type respectively (indicated by Num in the specifications).

@Leading@;Values are output as decimal literals without low line characters.
The format of each value output consists of a Fore field, a decimal point,
an Aft field, and (if a nonzero Exp parameter is supplied) the letter E
and an Exp field. The two possible formats thus correspond to:

@begin{Example}
Fore  .  Aft
@end{Example}

@Leading@;and to:

@begin{Example}
Fore  .  Aft  E  Exp
@end{Example}

without any spaces between these fields. The Fore field may include
leading spaces, and a minus sign for negative values. The Aft field
includes only decimal digits (possibly with trailing zeros). The Exp
field includes the sign (plus or minus) and the exponent (possibly with
leading zeros).

@Leading@;For floating point types, the default lengths of these fields are
defined by the following variables that are declared in the generic
package Float_IO:

@begin{Example}
@Trailing@;Default_Fore : Field := 2;
Default_Aft  : Field := Num'Digits-1;
Default_Exp  : Field := 3;
@end{Example}

@Leading@;For ordinary or decimal fixed point types, the default lengths of these
fields are defined by the following variables that are declared in the
generic packages Fixed_IO and Decimal_IO, respectively:

@begin{Example}
@Trailing@;Default_Fore : Field := Num'Fore;
Default_Aft  : Field := Num'Aft;
Default_Exp  : Field := 0;
@end{Example}

@Leading@Keepnext@;The following procedures are provided:
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Get(File : @key[in] File_Type; Item : @key[out] Num; Width : @key[in] Field := 0);
@key[procedure] Get(Item : @key[out] Num; Width : @key[in] Field := 0);
@end{Example}

@Leading@;If the value of the parameter Width is zero, skips any leading
blanks, line terminators, or page terminators, then reads the longest
possible sequence of characters matching the syntax of any of the following
(see @RefSecNum(Numeric Literals)):
@begin[itemize]
[+|@en]@nt[numeric_literal]

[+|@en]@nt[numeral].[@nt[exponent]]

[+|@en].@nt[numeral][@nt[exponent]]

[+|@en]@nt[base]#@nt[based_numeral].#[@nt[exponent]]

[+|@en]@nt[base]#.@nt[based_numeral]#[@nt[exponent]]
@end{Itemize}


          If a nonzero value of Width is supplied, then exactly Width
          characters are input, or the characters (possibly none) up to
          a line terminator, whichever comes first; any skipped leading
          blanks are included in the count.

          Returns in the parameter Item the value of type Num that
          corresponds to the sequence input, preserving the sign (positive
          if none has been specified) of a zero value if Num is a floating
          point type and Num'Signed_Zeros is True.

          @Trailing@;The exception Data_Error is propagated if the sequence input does
          not have the required syntax or if the value obtained is not
          of the subtype Num.

@begin{Example}@Keepnext
@key[procedure] Put(File : @key[in] File_Type;
              Item : @key[in] Num;
              Fore : @key[in] Field := Default_Fore;
              Aft  : @key[in] Field := Default_Aft;
              Exp  : @key[in] Field := Default_Exp);
@comment{Blank Line}
@key[procedure] Put(Item : @key[in] Num;
              Fore : @key[in] Field := Default_Fore;
              Aft  : @key[in] Field := Default_Aft;
              Exp  : @key[in] Field := Default_Exp);
@end{Example}
Outputs the value of the parameter Item as a decimal literal
with the format defined by Fore, Aft and Exp. If the value is
negative, or if Num is a floating point type where Num'Signed_Zeros is True and
the value is a negatively signed zero, then
a minus sign is included in the integer part.
If Exp has the value zero, then the integer part to be output has
as many digits as are needed to represent the integer part of
the value of Item, overriding Fore if necessary, or consists
of the digit zero if the value of Item has no integer part.

If Exp has a value greater than zero, then the integer part to
be output has a single digit, which is nonzero except for the
value 0.0 of Item.

In both cases, however, if the integer part to be output has
fewer than Fore characters, including any minus sign, then
leading spaces are first output to make up the difference.
The number of digits of the fractional part is given by Aft,
or is one if Aft equals zero.
The value is rounded; a value of exactly one half in the last place
is rounded away from zero.

@Trailing@;If Exp has the value zero, there is no exponent part. If Exp
has a value greater than zero, then the exponent part to be
output has as many digits as are needed to represent the
exponent part of the value of Item (for which a single digit
integer part is used), and includes an initial sign (plus or
minus). If the exponent part to be output has fewer than Exp
characters, including the sign, then leading zeros precede the
digits, to make up the difference. For the value 0.0 of Item,
the exponent has the value zero.

@begin{Example}@Keepnext
@key[procedure] Get(From : @key[in] String; Item : @key[out] Num; Last : @key[out] Positive);
@end{Example}
          Reads a real value from the beginning of the given string,
          following the same rule as the Get procedure that reads a real
          value from a file, but treating the end of the string as a
          file terminator. Returns, in the parameter Item, the value of
          type Num that corresponds to the sequence input. Returns in
          Last the index value such that From(Last) is the last
          character read.

          @Trailing@;The exception Data_Error is propagated if the sequence input does
          not have the required syntax, or if the value obtained is not
          of the subtype Num.

@begin{Example}@Keepnext
@key[procedure] Put(To   : @key[out] String;
              Item : @key[in] Num;
              Aft  : @key[in] Field := Default_Aft;
              Exp  : @key[in] Field := Default_Exp);
@end{Example}
          @Trailing@;Outputs the value of the parameter Item to the given string,
          following the same rule as for output to a file, using a value
          for Fore such that the sequence of characters output exactly
          fills the string, including any leading spaces.
@end{DescribeCode}


@Leading@;Float_Text_IO is a library package that is a nongeneric equivalent
to Text_IO.Float_IO for the predefined type Float:
@begin{Example}
@key[with] Ada.Text_IO;@ChildUnit{Parent=[Ada],Child=[Float_@!Text_IO]}
@key[package] Ada.Float_Text_IO @key[is] @key[new] Ada.Text_IO.Float_IO(Float);
@end{Example}

For each predefined floating point type,
a nongeneric equivalent to Text_IO.Float_IO is provided,
with names such as Ada.Long_Float_Text_IO.


@end{StaticSem}

@begin{ImplPerm}

An implementation may extend Get @Redundant[and Put] for floating point
types to support special values such as infinities and NaNs.

@begin{Discussion}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
See also the similar permission for the @Chg{Version=[3],New=[Wide_Wide_Value, ],Old=[]}
Wide_Value@Chg{Version=[3],New=[, and Value attributes],Old=[attribute]}
in @RefSecNum{Scalar Types}.
@end{Discussion}


The implementation of Put need not produce an output value with greater
accuracy than is supported for the base subtype.
The additional accuracy, if any,
of the value produced by Put when the number of
requested digits in the integer and fractional parts exceeds
the required accuracy
is implementation defined.
@begin{Discussion}
The required accuracy is thus Num'Base'Digits digits if Num is
a floating point subtype.
For a fixed point subtype the required accuracy is a function
of the subtype's Fore, Aft, and Delta attributes.
@end{Discussion}

@ImplDef{The accuracy of the value produced by Put.}


The nongeneric equivalent packages may, but need not,
be actual instantiations of the generic package for the appropriate
predefined type.

@end{ImplPerm}

@begin{Notes}
For an item with a positive value, if output to a string exactly fills
the string without leading spaces, then output of the corresponding
negative value will propagate Layout_Error.

The rules for the Value attribute
(see @RefSecNum(Scalar Types)) and the rules for Get are based on the
same set of formats.
@end{Notes}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[1], Kind=[Deleted]}
@ChgDeleted[Version=[1],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

@key[package] Real_IO @key[is] @key[new] Float_IO(Real); @key[use] Real_IO;
--@RI{ default format used at instantiation, Default_Exp = 3}

X : Real := -123.4567;  --@RI{  digits 8      (see @RefSecNum{Floating Point Types})}

@tabclear()@tabset(P50)
Put(X);  @RI[-- default format] @\@RI["@en@|1.2345670E+02"]
Put(X, Fore => 5, Aft => 3, Exp => 2); @\@RI[-- "bbb@en@|1.235E+2"]
Put(X, 5, 3, 0);             @\@RI[-- "b@en@|123.457"]
@end{Example}
@end{Examples}

@LabeledSubClause{Input-Output for Enumeration Types}

@begin{StaticSem}
The following procedures are defined in the generic package
Enumeration_IO, which has to be instantiated for the appropriate
enumeration type (indicated by Enum in the specification).

Values are output using either upper or lower case letters for
identifiers. This is specified by the parameter Set, which is of the
enumeration type Type_Set.

@begin{Example}
@Trailing@key[type] Type_Set @key[is] (Lower_Case, Upper_Case);
@end{Example}

@Leading@;The format (which includes any trailing spaces) can be specified
by an optional field width parameter. The default field width and letter case
are defined by the following variables that are declared in the generic
package Enumeration_IO:

@begin{Example}
@Trailing@;Default_Width   : Field := 0;
Default_Setting : Type_Set := Upper_Case;
@end{Example}

@Leading@Keepnext@;The following procedures are provided:
@begin{DescribeCode}
@begin{Example}@Keepnext
@key[procedure] Get(File : @key[in] File_Type; Item : @key[out] Enum);
@key[procedure] Get(Item : @key[out] Enum);
@end{Example}
          After skipping any leading blanks, line terminators, or page
          terminators, reads an identifier according to the syntax of
          this lexical element (lower and upper case being considered
          equivalent), or a character literal according to the syntax of
          this lexical element (including the apostrophes). Returns, in
          the parameter Item, the value of type Enum that corresponds to
          the sequence input.

          @Trailing@;The exception Data_Error is propagated if the sequence input does
          not have the required syntax, or if the identifier or
          character literal does not correspond to a value of the
          subtype Enum.

@begin{Example}@Keepnext
@key[procedure] Put(File  : @key[in] File_Type;
              Item  : @key[in] Enum;
              Width : @key[in] Field := Default_Width;
              Set   : @key[in] Type_Set := Default_Setting);
@comment{Blank Line}
@key[procedure] Put(Item  : @key[in] Enum;
              Width : @key[in] Field := Default_Width;
              Set   : @key[in] Type_Set := Default_Setting);
@end{Example}
          @Trailing@;Outputs the value of the parameter Item as an enumeration
          literal (either an identifier or a character literal). The
          optional parameter Set indicates whether lower case or upper
          case is used for identifiers; it has no effect for character
          literals. If the sequence of characters produced has fewer
          than Width characters, then trailing spaces are finally output
          to make up the difference. If Enum is a character type,
          the sequence of characters produced is as for
          Enum'Image(Item), as modified by the Width
          and Set parameters.
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  For a character type, the literal might be a
  @Chg{Version=[3],New=[Wide_Wide_Character, ],Old=[]}Wide_Character@Chg{Version=[3],New=[,],Old=[]}
  or a control character.
  Whatever Image does for these things is appropriate here,
  too.

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0036-1]}
  @ChgAdded{Version=[3],Text=[The @ldquote@;characters produced@rdquote@;
  defines the @ldquote@;characters to be output@rdquote in the sense of
  @RefSecNum{Get and Put Procedures}, so a result that cannot fit on any bounded
  line will raise Layout_Error.]}
@end{Discussion}

@begin{Example}@Keepnext
@key[procedure] Get(From : @key[in] String; Item : @key[out] Enum; Last : @key[out] Positive);
@end{Example}
          Reads an enumeration value from the beginning of the given
          string, following the same rule as the Get procedure that
          reads an enumeration value from a file, but treating the end
          of the string as a file terminator. Returns, in the parameter
          Item, the value of type Enum that corresponds to the sequence
          input. Returns in Last the index value such that From(Last)
          is the last character read.

          @Trailing@;The exception Data_Error is propagated if the sequence input does
          not have the required syntax, or if the identifier or
          character literal does not correspond to a value of the
          subtype Enum.
@begin{Honest}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  For a character type, it is permissible for the implementation to make
  Get do the inverse of what Put does,
  in the case of wide@Chg{Version=[3],New=[ and wide_wide],Old=[]}
  @nt{character_literal}s and control characters.
@end{Honest}

@begin{Example}@Keepnext
@key[procedure] Put(To   : @key[out] String;
              Item : @key[in] Enum;
              Set  : @key[in] Type_Set := Default_Setting);
@end{Example}
          @Trailing@;Outputs the value of the parameter Item to the given string,
          following the same rule as for output to a file, using the
          length of the given string as the value for Width.
@end{DescribeCode}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0054],ARef=[AI95-00007-01]}
Although the specification of the generic package Enumeration_IO would allow
instantiation for an @Chg{New=[integer],Old=[float]} type, this is not the
intended purpose of this generic package, and the effect of such instantiations
is not defined by the language.
@end{StaticSem}

@begin{Notes}
There is a difference between Put defined for characters, and for
enumeration values. Thus
@begin{Example}
   Ada.Text_IO.Put('A');  --@RI{  outputs the character A}

   @key[package] Char_IO @key[is] @key[new] Ada.Text_IO.Enumeration_IO(Character);
   Char_IO.Put('A');  --@RI{  outputs the character 'A', between apostrophes}
@end{Example}

The type Boolean is an enumeration type, hence Enumeration_IO can be
instantiated for this type.
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0054],ARef=[AI95-00007-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the wording to
  say Enumeration_IO can be instantiated with an integer type, not a float
  type.]}
@end{DiffWord95}


@LabeledAddedSubClause{Version=[2],Name=[Input-Output for Bounded Strings]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[The package Text_IO.Bounded_IO provides
input-output in human-readable form for Bounded_Strings.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The generic library
package Text_IO.Bounded_IO has the following declaration:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.Strings.Bounded;
@key{generic}
   @key{with package} Bounded @key{is}
                     @key{new} Ada.Strings.Bounded.Generic_Bounded_Length (<>);
@key{package} Ada.Text_IO.Bounded_IO @key{is}@ChildUnit{Parent=[Ada.Text_IO],Child=[Bounded_IO]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put}
      (File : @key{in} File_Type;
       Item : @key{in} Bounded.Bounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put}
      (Item : @key{in} Bounded.Bounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put_Line}
      (File : @key{in} File_Type;
       Item : @key{in} Bounded.Bounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put_Line}
      (Item : @key{in} Bounded.Bounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Get_Line}
      (File : @key{in} File_Type)
      @key{return} Bounded.Bounded_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Get_Line}
      @key{return} Bounded.Bounded_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Get_Line}
      (File : @key{in} File_Type; Item : @key{out} Bounded.Bounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Get_Line}
      (Item : @key{out} Bounded.Bounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Text_IO.Bounded_IO;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For an item of type
Bounded_String, the following subprograms are provided:]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put
   (File : @key{in} File_Type;
    Item : @key{in} Bounded.Bounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put (File,
Bounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put
   (Item : @key{in} Bounded.Bounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put
(Bounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put_Line
   (File : @key{in} File_Type;
    Item : @key{in} Bounded.Bounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put_Line (File,
Bounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put_Line
   (Item : @key{in} Bounded.Bounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put_Line
(Bounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Get_Line
   (File : @key{in} File_Type)
   @key{return} Bounded.Bounded_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Returns
Bounded.To_Bounded_String(Text_IO.Get_Line(File));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Get_Line
   @key{return} Bounded.Bounded_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Returns
Bounded.To_Bounded_String(Text_IO.Get_Line);]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Get_Line
   (File : @key{in} File_Type; Item : @key{out} Bounded.Bounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Item := Get_Line (File);]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Get_Line
   (Item : @key{out} Bounded.Bounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Item := Get_Line;]}
@end{DescribeCode}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package Text_IO.Bounded_IO is new.]}
@end{Extend95}


@LabeledAddedSubClause{Version=[2],Name=[Input-Output for Unbounded Strings]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[The package Text_IO.Unbounded_IO provides
input-output in human-readable form for Unbounded_Strings.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[The library package
Text_IO.Unbounded_IO has the following declaration:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} Ada.Strings.Unbounded;
@key{package} Ada.Text_IO.Unbounded_IO @key{is}@ChildUnit{Parent=[Ada.Text_IO],Child=[Unbounded_IO]}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put}
      (File : @key{in} File_Type;
       Item : @key{in} Strings.Unbounded.Unbounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put}
      (Item : @key{in} Strings.Unbounded.Unbounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put_Line}
      (File : @key{in} File_Type;
       Item : @key{in} Strings.Unbounded.Unbounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Put_Line}
      (Item : @key{in} Strings.Unbounded.Unbounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Get_Line}
      (File : @key{in} File_Type)
      @key{return} Strings.Unbounded.Unbounded_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{function} @AdaSubDefn{Get_Line}
      @key{return} Strings.Unbounded.Unbounded_String;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Get_Line}
      (File : @key{in} File_Type; Item : @key{out} Strings.Unbounded.Unbounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} @AdaSubDefn{Get_Line}
      (Item : @key{out} Strings.Unbounded.Unbounded_String);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{end} Ada.Text_IO.Unbounded_IO;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For an item of type
Unbounded_String, the following subprograms are provided:]}

@begin{DescribeCode}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put
   (File : @key{in} File_Type;
    Item : @key{in} Strings.Unbounded.Unbounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put (File,
Strings.Unbounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put
   (Item : @key{in} Strings.Unbounded.Unbounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put
(Strings.Unbounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put_Line
   (File : @key{in} File_Type;
    Item : @key{in} Strings.Unbounded.Unbounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put_Line (File,
Strings.Unbounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Put_Line
   (Item : @key{in} Strings.Unbounded.Unbounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Text_IO.Put_Line
(Strings.Unbounded.To_String(Item));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Get_Line
   (File : @key{in} File_Type)
   @key{return} Strings.Unbounded.Unbounded_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Returns
Strings.Unbounded.To_Unbounded_String(Text_IO.Get_Line(File));]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{function} Get_Line
   @key{return} Strings.Unbounded.Unbounded_String;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Returns
Strings.Unbounded.To_Unbounded_String(Text_IO.Get_Line);]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Get_Line
   (File : @key{in} File_Type; Item : @key{out} Strings.Unbounded.Unbounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Item := Get_Line (File);]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],KeepNext=[T],Text=[@key{procedure} Get_Line
   (Item : @key{out} Strings.Unbounded.Unbounded_String);]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
@ChgAdded{Version=[2],Text=[Equivalent to Item := Get_Line;]}
@end{DescribeCode}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package Text_IO.Unbounded_IO is new.]}
@end{Extend95}


@LabeledRevisedClause{Version=[1],New=[Wide Text Input-Output and Wide Wide Text Input-Output],Old=[Wide Text Input-Output]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
The @Chg{Version=[2],New=[packages],Old=[package]} Wide_Text_IO
@Chg{Version=[2],New=[and Wide_Wide_Text_IO provide],Old=[provides]} facilities
for input and output in human-readable form. Each file is read or
written sequentially, as a sequence of wide characters
@Chg{Version=[2],New=[(or wide wide characters) ],Old=[]}grouped
into lines, and as a sequence of lines grouped into pages.
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00301-01]}
@ChildUnit{Parent=[Ada],Child=[Wide_@!Text_IO]}
The specification of package Wide_Text_IO is the same as that for
Text_IO, except that in each Get,
Look_Ahead, Get_Immediate,
Get_Line, Put, and Put_Line @Chg{Version=[2],New=[subprogram],Old=[procedure]},
any occurrence of Character is replaced by Wide_Character, and any
occurrence of String is replaced by Wide_String.@Chg{Version=[2],New=[
@ChildUnit{Parent=[Ada],Child=[Integer_@!Wide_@!Text_IO]}
@ChildUnit{Parent=[Ada],Child=[Float_@!Wide_@!Text_IO]}
Nongeneric equivalents of Wide_Text_IO.Integer_IO
and Wide_Text_IO.@!Float_IO are provided (as for Text_IO)
for each predefined numeric type,
with names such as Ada.Integer_@!Wide_Text_IO,
Ada.Long_@!Integer_@!Wide_Text_IO,
Ada.Float_@!Wide_Text_IO,
Ada.Long_@!Float_@!Wide_Text_IO.],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01],ARef=[AI95-00301-01]}
@Chg{Version=[2],New=[
@ChildUnit{Parent=[Ada],Child=[Wide_Wide_@!Text_IO]}
The specification of package Wide_Wide_Text_IO is the same as that for
Text_IO, except that in each Get,
Look_Ahead, Get_Immediate,
Get_Line, Put, and Put_Line subprogram,
any occurrence of Character is replaced by Wide_Wide_Character, and any
occurrence of String is replaced by Wide_Wide_String.
@ChildUnit{Parent=[Ada],Child=[Integer_@!Wide_Wide_@!Text_IO]}
@ChildUnit{Parent=[Ada],Child=[Float_@!Wide_Wide_@!Text_IO]}
Nongeneric equivalents of Wide_Wide_Text_IO.Integer_IO
and Wide_Wide_@!Text_IO.@!Float_IO are provided (as for Text_IO)
for each predefined numeric type,
with names such as Ada.Integer_@!Wide_Wide_@!Text_IO,
Ada.Long_@!Integer_@!Wide_Wide_@!Text_IO,
Ada.Float_@!Wide_Wide_@!Text_IO,
Ada.Long_@!Float_@!Wide_Wide_@!Text_IO.],
Old=[
@ChildUnit{Parent=[Ada],Child=[Integer_@!Wide_@!Text_IO]}
@ChildUnit{Parent=[Ada],Child=[Float_@!Wide_@!Text_IO]}
Nongeneric equivalents of Wide_Text_IO.Integer_IO
and Wide_Text_IO.@!Float_IO are provided (as for Text_IO)
for each predefined numeric type,
with names such as Ada.Integer_@!Wide_Text_IO,
Ada.Long_@!Integer_@!Wide_Text_IO,
Ada.Float_@!Wide_Text_IO,
Ada.Long_@!Float_@!Wide_Text_IO.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00428-01]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0004-1],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Text=[
@ChildUnit{Parent=[Ada.Wide_@!Text_IO],Child=[Bounded_IO]}
@ChildUnit{Parent=[Ada.Wide_Wide_@!Text_IO],Child=[Bounded_IO]}
The specification of package Wide_Text_IO.Wide_@!Bounded_IO is the same as that
for Text_IO.Bounded_IO, except that any occurrence of Bounded_String is
replaced by @Chg{Version=[3],New=[Bounded_@!Wide_@!String],Old=[Wide_@!Bounded_@!String]},
and any occurrence of package Bounded is
replaced by Wide_Bounded. The specification of package
@Chg{Version=[3],New=[Wide_Wide_@!Text_IO.Wide_Wide_@!Bounded_IO],
Old=[Wide_Wide_@!Text_IO.Wide_@!Bounded_IO]} is the same as that for
Text_IO.@!Bounded_IO, except that any occurrence of Bounded_@!String is
replaced by @Chg{Version=[3],New=[Bounded_@!Wide_Wide_@!String],
Old=[Wide_Wide_@!Bounded_@!String]}, and any occurrence of package Bounded
is replaced by Wide_Wide_@!Bounded.]}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[3],Text=[@ldquote@;package Bounded@rdquote refers
  to both the package Ada.Strings.Bounded and the formal package
  parameter named Bounded.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00301-01]}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Text=[
@ChildUnit{Parent=[Ada.Wide_@!Text_IO],Child=[Unbounded_IO]}
@ChildUnit{Parent=[Ada.Wide_Wide_@!Text_IO],Child=[Unbounded_IO]}
The specification of package Wide_Text_IO.@!Wide_Unbounded_IO is the same as that
for Text_IO.@!Unbounded_IO, except that any occurrence of Unbounded_@!String is
replaced by @Chg{Version=[3],New=[Unbounded_@!Wide_@!String],Old=[Wide_@!Unbounded_@!String]},
and any occurrence of package Unbounded is
replaced by Wide_@!Unbounded. The specification of package
Wide_Wide_Text_IO.Wide_Wide_@!Unbounded_IO is the same as that for
Text_IO.Unbounded_IO, except that any occurrence of Unbounded_String is
replaced by @Chg{Version=[3],New=[Unbounded_@!Wide_Wide_@!String],
Old=[Wide_Wide_@!Unbounded_@!String]}, and any occurrence of package Unbounded
is replaced by Wide_Wide_@!Unbounded.]}
@end{StaticSem}

@begin{Extend83}
@Defn{extensions to Ada 83}
Support for Wide_Character and Wide_String I/O is new in Ada 95.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package Wide_Wide_Text_IO is new. Be glad it wasn't called
  Double_Wide_Text_IO (for use in trailer parks) or Really_Wide_Text_IO.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00301-01]}
  @ChgAdded{Version=[2],Text=[Packages
  Wide_Text_IO.Wide_Unbounded_IO and
  Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO are also new.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00428-01]}
  @ChgAdded{Version=[2],Text=[Packages
  Wide_Text_IO.Wide_Bounded_IO and
  Wide_Wide_Text_IO.Wide_Wide_Bounded_IO are new as well.]}
@end{Extend95}


@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0092-1]}
  @ChgAdded{Version=[3],Text=[@B<Correction>: Corrected the names of
  various entities in the above description. Since the previously named
  entities don't exist and the intent is obvious, this is just considered
  a presentation change.]}
@end{DiffWord2005}


@LabeledClause{Stream Input-Output}
@begin{Intro}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
The packages Streams.Stream_IO, Text_IO.Text_Streams, @Chg{Version=[2],
New=[],Old=[and ]}Wide_Text_IO.Text_Streams@Chg{Version=[2],New=[, and
Wide_Wide_Text_IO.Text_Streams],Old=[]} provide stream-oriented operations
on files.
@end{Intro}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Included package Wide_Wide_Text_IO.Text_Streams
  in this description.]}
@end{Diffword95}


@LabeledSubClause{The Package Streams.Stream_IO}

@begin{Intro}
@Defn{heterogeneous input-output}
@Redundant[The subprograms in the child package Streams.Stream_IO provide control
over stream files. Access to a stream file is either sequential,
via a call on Read or Write to transfer an array of stream elements,
or positional (if supported by the implementation for the given file),
by specifying a relative index for an element. Since a stream
file can be converted to a Stream_Access value, calling stream-oriented
attribute subprograms of different element types with the same
Stream_Access value provides heterogeneous input-output.]
See @RefSecNum{Streams} for a general discussion of streams.
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[The elements of a stream file are stream elements.
If positioning is
supported for the specified external file, a current index and current size
are maintained for the file as described in @RefSecNum(Sequential and Direct Files).
If positioning is not supported, a current index is not maintained, and the
current size is implementation defined.@Defn2{Term=(Current index),
Sec=(of an open stream file)}@Defn2{Term=(Current size),Sec=(of a stream file)}]}
@ChgImpldef{Version=[1],Kind=[Added],Text=[@ChgAdded{Version=[1],
Text=[Current size for a stream file for which positioning is not supported.]}]}

@Leading@;The library package Streams.Stream_IO has the following declaration:
@begin(example)
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0283-1]}
@key(with) Ada.IO_Exceptions;@ChildUnit{Parent=[Ada.Streams],Child=[Stream_@!IO]}
@key(package) Ada.Streams.Stream_IO @key(is)@Chg{Version=[3],New=[
    @key(pragma) Preelaborate(Stream_IO);],Old=[]}

    @key[type] @AdaTypeDefn{Stream_Access} @key[is] @key[access] @key[all] Root_Stream_Type'Class;

    @key(type) @AdaTypeDefn{File_Type} @key(is) @key(limited) @key(private;)

    @key(type) @AdaTypeDefn{File_Mode} @key(is) (In_File, Out_File, Append_File);

    @key[type]    @AdaTypeDefn{Count}          @key[is] @key[range] 0 .. @RI[implementation-defined];
    @key[subtype] @AdaSubtypeDefn{Name=[Positive_Count],Of=[Count]} @key[is] Count @key[range] 1 .. Count'Last;
      -- @RI(Index into file, in stream elements.)

    @key(procedure) @AdaSubDefn{Create} (File : @key(in) @key(out) File_Type;
                      Mode : @key(in) File_Mode := Out_File;
                      Name : @key(in) String    := "";
                      Form : @key(in) String    := "");

    @key(procedure) @AdaSubDefn{Open} (File : @key(in) @key(out) File_Type;
                    Mode : @key(in) File_Mode;
                    Name : @key(in) String;
                    Form : @key(in) String := "");

    @key(procedure) @AdaSubDefn{Close}  (File : @key(in) @key(out) File_Type);
    @key(procedure) @AdaSubDefn{Delete} (File : @key(in) @key(out) File_Type);
    @key(procedure) @AdaSubDefn{Reset}  (File : @key(in) @key(out) File_Type; Mode : @key(in) File_Mode);
    @key(procedure) @AdaSubDefn{Reset}  (File : @key(in) @key(out) File_Type);

    @key(function) @AdaSubDefn{Mode} (File : @key(in) File_Type) @key(return) File_Mode;
    @key(function) @AdaSubDefn{Name} (File : @key(in) File_Type) @key(return) String;
    @key(function) @AdaSubDefn{Form} (File : @key(in) File_Type) @key(return) String;

    @key(function) @AdaSubDefn{Is_Open}     (File : @key(in) File_Type) @key(return) Boolean;
    @key(function) @AdaSubDefn{End_Of_File} (File : @key(in) File_Type) @key(return) Boolean;

    @key(function) @AdaSubDefn{Stream} (File : @key(in) File_Type) @key(return) Stream_Access;
        -- @RI(Return stream access for use with T'Input and T'Output)

@ChgRef{Version=[1], Kind=[Deleted]}
@ChgDeleted[Version=[1],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

    -- @RI(Read array of stream elements from file)
    @key(procedure) @AdaSubDefn{Read} (File : @key(in)  File_Type;
                    Item : @key(out) Stream_Element_Array;
                    Last : @key(out) Stream_Element_Offset;
                    From : @key(in)  Positive_Count);

    @key(procedure) @AdaSubDefn{Read} (File : @key(in)  File_Type;
                    Item : @key(out) Stream_Element_Array;
                    Last : @key(out) Stream_Element_Offset);

@ChgRef{Version=[1], Kind=[Deleted]}
@ChgDeleted[Version=[1],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

    -- @RI(Write array of stream elements into file)
    @key(procedure) @AdaSubDefn{Write} (File : @key(in) File_Type;
                     Item : @key(in) Stream_Element_Array;
                     To   : @key(in) Positive_Count);

    @key(procedure) @AdaSubDefn{Write} (File : @key(in) File_Type;
                           Item : @key(in) Stream_Element_Array);

@ChgRef{Version=[1], Kind=[Deleted]}
@ChgDeleted[Version=[1],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]

    -- @RI(Operations on position within file)

    @key[procedure] @AdaSubDefn{Set_Index}(File : @key[in] File_Type; To : @key[in] Positive_Count);

    @key[function] @AdaSubDefn{Index}(File : @key[in] File_Type) @key[return] Positive_Count;
    @key[function] @AdaSubDefn{Size} (File : @key[in] File_Type) @key[return] Count;

    @key(procedure) @AdaSubDefn{Set_Mode}(File : @key(in) @key(out) File_Type; Mode : @key(in) File_Mode);

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0051],ARef=[AI95-00057-01]}
    @key(procedure) @AdaSubDefn{Flush}(File : @key(in) @Chg{New=[],Old=[@key(out) ]}File_Type);


    -- @RI(exceptions)
    @AdaExcDefn{Status_Error} : @key(exception) @key(renames) IO_Exceptions.Status_Error;
    @AdaExcDefn{Mode_Error}   : @key(exception) @key(renames) IO_Exceptions.Mode_Error;
    @AdaExcDefn{Name_Error}   : @key(exception) @key(renames) IO_Exceptions.Name_Error;
    @AdaExcDefn{Use_Error}    : @key(exception) @key(renames) IO_Exceptions.Use_Error;
    @AdaExcDefn{Device_Error} : @key(exception) @key(renames) IO_Exceptions.Device_Error;
    @AdaExcDefn{End_Error}    : @key(exception) @key(renames) IO_Exceptions.End_Error;
    @AdaExcDefn{Data_Error}   : @key(exception) @key(renames) IO_Exceptions.Data_Error;

@key[private]
   ... -- @RI{not specified by the language}
@key(end) Ada.Streams.Stream_IO;
@end(example)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Text=[The type File_Type
needs finalization@PDefn2{Term=<needs finalization>,Sec=<language-defined type>}
(see @RefSecNum{Assignment and Finalization}).]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00283-01]}
The subprograms @Chg{Version=[2],New=[given in subclause
@RefSecNum(File Management) for the control of external files (],Old=[]}Create,
Open, Close, Delete, Reset, Mode, Name, Form,@Chg{Version=[2],New=[ and],Old=[]}
Is_Open@Chg{Version=[2],New=[) are available for stream files],
Old=[, and End_of_File have the same effect as the corresponding
subprograms in Sequential_IO (see @RefSecNum(File Management))]}.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00283-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The End_Of_File function:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Propagates Mode_Error if the mode of the file
is not In_File;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If positioning is supported for the given external
file, the function returns True if the current index exceeds the size of the
external file; otherwise@Chg{Version=[3],New=[,],Old=[]}
it returns False;]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
@ChgAdded{Version=[2],Text=[If positioning is not supported for the given
external file, the function returns True if no more elements can be read from
the given file; otherwise@Chg{Version=[3],New=[,],Old=[]}
it returns False.]}
@end{Itemize}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00085-01]}
@ChgAdded{Version=[1],Text=[The Set_Mode
procedure @Chg{Version=[2],New=[sets],Old=[changes]} the mode of the
file. If the new mode is Append_File, the file is positioned to its end;
otherwise, the position in the file is unchanged.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[The Flush procedure synchronizes the external file
with the internal file (by flushing any internal buffers) without closing the
file or changing the position. Mode_Error is propagated if the mode of the file
is In_File.]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0056],ARef=[AI95-00001-01]}
The Stream function returns a Stream_Access result from a File_Type
object, thus allowing the stream-oriented attributes Read, Write,
Input, and Output to be used on the same file for multiple types.
@Chg{New=[Stream propagates Status_Error if File is not open.],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
The procedures Read and Write are equivalent to the corresponding operations
in the package Streams. Read propagates Mode_Error if the mode of File is
not In_File. Write propagates Mode_Error if the mode of File is not
Out_File or Append_File. The Read procedure with a Positive_Count
parameter starts reading at the specified index.
The Write procedure with a Positive_Count
parameter starts writing at the specified index.@Chg{Version=[2],New=[ For
a file that supports positioning, Read without a Positive_Count parameter
starts reading at the current index, and Write without a Positive_Count
parameter starts writing at the current index.],Old=[]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[The Size function returns the current size of the
file.]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0055],ARef=[AI95-00026-01]}
The Index function returns the current @Chg{New=[],Old=[file ]}index@Chg{New=[],
Old=[, as a count (in stream elements) from the beginning of the file.
The position of the first element in the file is 1]}.
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Deleted]}
@ChgNote{This ramification is now part of the official wording.}
@ChgDeleted{Version=[1],Text=[The notion of Index for Stream_IO is analogous
to that of Index in Direct_IO, except that the former is measured in
Stream_Element units, whereas the latter is in terms of Element_Type values.]}
@end{Ramification}

The Set_Index procedure sets the current index to the
specified value.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Type=[Leading],Text=[If positioning is supported for the
external file, the current index is maintained as follows:]}

@begin{Itemize}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[For Open and Create, if the Mode parameter is
Append_File, the current index is set to the current size of the file plus one;
otherwise, the current index is set to one.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[For Reset, if the Mode parameter is Append_File, or
no Mode parameter is given and the current mode is Append_File, the current
index is set to the current size of the file plus one; otherwise, the current
index is set to one.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[For Set_Mode, if the new mode is Append_File, the
current index is set to current size plus one; otherwise, the current index is
unchanged.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[For Read and Write without a Positive_Count
parameter, the current index is incremented by the number of stream elements
read or written.]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgAdded{Version=[1],Text=[For Read and Write with a Positive_Count parameter,
the value of the current index is set to the value of the Positive_Count
parameter plus the number of stream elements read or written.]}
@end{Itemize}

If positioning is not supported for the given file, then a call
of Index or Set_Index propagates Use_Error. Similarly, a call of
Read or Write with a Positive_Count parameter propagates Use_Error.

@begin{ImplNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00085-01]}
@ChgAdded{Version=[2],Text=[It is permissible for an implementation to
implement mode Append_File using the Unix append mode (the
O_APPEND bit). Such an implementation does not support positioning when
the mode is Append_File, and therefore the operations listed above must
raise Use_Error. This is acceptable as there is no requirement that any
particular file support positioning; therefore it is acceptable that a
file support positioning when opened with mode Out_File, and the same file
not support positioning when opened with mode Append_File. But it is not
acceptable for a file to support positioning (by allowing the above
operations), but to do something other than the defined semantics (that is,
always write at the end, even when explicitly commanded to write somewhere
else).]}
@end{ImplNote}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 34 through 36
were deleted.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}
@ChgRef{Version=[1],Kind=[DeletedNoDelMsg],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgDeleted{Version=[1],Text=[The Size function returns the current size of
the file, in stream elements.]}

@ChgRef{Version=[1],Kind=[DeletedNoDelMsg],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgDeleted{Version=[1],Text=[The Set_Mode procedure changes the mode of the
file. If the new mode is Append_File, the file is positioned to its end;
otherwise, the position in the file is unchanged.]}

@ChgRef{Version=[1],Kind=[DeletedNoDelMsg],Ref=[8652/0055],ARef=[AI95-00026-01]}
@ChgDeleted{Version=[1],Text=[The Flush procedure synchronizes the external
file with the internal file (by flushing any internal buffers) without closing
the file or changing the position.
Mode_Error is propagated if the mode of the file is In_File.]}
@end{StaticSem}
@begin{Erron}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0056],ARef=[AI95-00001-01]}
@ChgAdded{Version=[1],Text=[@PDefn2{Term=(erroneous execution),Sec=(cause)}
If the File_Type object passed to the Stream function is later
closed or finalized, and the stream-oriented attributes are subsequently
called (explicitly or implicitly) on the Stream_Access value returned by
Stream, execution is erroneous. This rule applies even if the File_Type object
was opened again after it had been closed.]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[These rules are analogous to the rule for the result of the
Current_Input, Current_Output, and Current_Error functions. These rules make
it possible to represent a value of (some descendant of) Root_Stream_Type which
represents a file as an access value, with a null value corresponding to a
closed file.]}
@end{Reason}
@end{Erron}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00283-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  @b[Amendment Correction:] The description of the subprograms for
  managing files was corrected so that they do not require truncation of the
  external file @em a stream file is not a sequential file. An Ada 95
  program that expects truncation of the stream file
  @Chg{Version=[3],New=[might],Old=[may]} not work under Ada 2005.
  Note that the Ada 95 standard was ambiguous on this point (the normative
  wording seemed to require truncation, but didn't explain where; the
  AARM notes seemed to expect behavior like Direct_IO), and implementations
  varied widely. Therefore, as a practical matter, code that depends on
  stream truncation @Chg{Version=[3],New=[might],Old=[may]} not work
  even in Ada 95; deleting the file before
  opening it provides truncation that works in both Ada 95 and Ada 2005.]}
@end{Inconsistent95}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @B[Amendment Correction:] Stream_IO.File_Type is defined to need finalization. If the
  restriction No_Nested_Finalization (see @RefSecNum{Tasking Restrictions})
  applies to the partition, and File_Type does not have a controlled part, it
  will not be allowed in local objects in Ada 2005 whereas it would be allowed
  in original Ada 95. Such code is not portable, as another Ada compiler
  may have a controlled part in File_Type, and thus would be illegal.]}
@end{Incompatible95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0051],ARef=[AI95-00057-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the parameter mode
  of Flush; otherwise it could not be used on Standard_Output.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0055],ARef=[AI95-00026-01],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to describe the
  effects of the various operations on the current index. The Amendment adds
  an explanation of the use of current index for Read and Write.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0056],ARef=[AI95-00001-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that Stream can
  raise Status_Error, and clarified that using a Stream_Access whose file
  has been closed is erroneous.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00085-01]}
  @ChgAdded{Version=[2],Text=[Clarified that Set_Mode can
  be called with the current mode.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0283-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Package Ada.Streams.Stream_IO is now preelaborated, allowing it to be
  used in more contexts (including in distributed systems). Note that
  is @i<not> a remote types package; File_Type objects cannot be
  passed between partitions.]}
@end{Extend2005}


@LabeledSubClause{The Package Text_IO.Text_Streams}
@begin{Intro}
The package Text_IO.Text_Streams provides a function for treating
a text file as a stream.
@end{Intro}

@begin{StaticSem}
@Leading@;The library package Text_IO.Text_Streams has the following declaration:
@begin{example}
@key[with] Ada.Streams;@ChildUnit{Parent=[Ada.Text_IO],Child=[Text_@!Streams]}
@key[package] Ada.Text_IO.Text_Streams @key[is]
   @key[type] @AdaTypeDefn{Stream_Access} @key[is] @key[access] @key[all] Streams.Root_Stream_Type'Class;

   @key[function] @AdaSubDefn{Stream} (File : @key[in] File_Type) @key[return] Stream_Access;
@key[end] Ada.Text_IO.Text_Streams;
@end{example}

The Stream function has the same effect as the corresponding function
in Streams.Stream_IO.
@end{StaticSem}
@begin[Notes]
The ability to obtain a stream for a text file allows Current_Input,
Current_Output, and Current_Error to be processed with the functionality
of streams, including the mixing of text and binary input-output,
and the mixing of binary input-output for different types.

Performing operations on the stream associated with a text file does not
affect the column, line, or page counts.
@end[Notes]


@LabeledSubClause{The Package Wide_Text_IO.Text_Streams}
@begin{Intro}
The package Wide_Text_IO.Text_Streams provides a function for treating
a wide text file as a stream.
@end{Intro}

@begin{StaticSem}
@Leading@;The library package Wide_Text_IO.Text_Streams
has the following declaration:
@begin{example}
@key[with] Ada.Streams;@ChildUnit{Parent=[Ada.Wide_@!Text_IO],Child=[Text_@!Streams]}
@key[package] Ada.Wide_Text_IO.Text_Streams @key[is]
   @key[type] @AdaTypeDefn{Stream_Access} @key[is] @key[access] @key[all] Streams.Root_Stream_Type'Class;

   @key[function] @AdaSubDefn{Stream} (File : @key[in] File_Type) @key[return] Stream_Access;
@key[end] Ada.Wide_Text_IO.Text_Streams;
@end{example}

The Stream function has the same effect as the corresponding function
in Streams.Stream_IO.
@end{StaticSem}


@LabeledAddedSubClause{Version=[2],Name=[The Package Wide_Wide_Text_IO.Text_Streams]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[The package Wide_Wide_Text_IO.Text_Streams provides
a function for treating a wide wide text file as a stream.]}
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Leading@;The library package
Wide_Wide_Text_IO.Text_Streams has the following declaration:]}
@begin{example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[with] Ada.Streams;@ChildUnit{Parent=[Ada.Wide_Wide_@!Text_IO],Child=[Text_@!Streams]}
@key[package] Ada.Wide_Wide_Text_IO.Text_Streams @key[is]
   @key[type] @AdaTypeDefn{Stream_Access} @key[is] @key[access] @key[all] Streams.Root_Stream_Type'Class;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[function] @AdaSubDefn{Stream} (File : @key[in] File_Type) @key[return] Stream_Access;
@key[end] Ada.Wide_Wide_Text_IO.Text_Streams;]}
@end{example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
@ChgAdded{Version=[2],Text=[The Stream function has the same effect as the
corresponding function in Streams.Stream_IO.]}
@end{StaticSem}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Package Wide_Wide_Text_IO.Text_Streams is new.]}
@end{Extend95}


@LabeledClause{Exceptions in Input-Output}
@begin{Intro}
The package IO_Exceptions defines the exceptions needed by the
predefined input-output packages.
@end{Intro}

@begin{StaticSem}
@Leading@;The library package IO_Exceptions has the following declaration:
@begin{Example}
@key[package] Ada.IO_Exceptions @key[is]@ChildUnit{Parent=[Ada],Child=[IO_Exceptions]}
   @key[pragma] Pure(IO_Exceptions);

   @AdaExcDefn{Status_Error} : @key[exception];
   @AdaExcDefn{Mode_Error}   : @key[exception];
   @AdaExcDefn{Name_Error}   : @key[exception];
   @AdaExcDefn{Use_Error}    : @key[exception];
   @AdaExcDefn{Device_Error} : @key[exception];
   @AdaExcDefn{End_Error}    : @key[exception];
   @AdaExcDefn{Data_Error}   : @key[exception];
   @AdaExcDefn{Layout_Error} : @key[exception];

@key[end] Ada.IO_Exceptions;
@end{Example}

If more than one error condition exists, the corresponding exception
that appears earliest in the following list is the one that is propagated.

The exception Status_Error is propagated by an attempt to operate upon a
file that is not open, and by an attempt to open a file that is already
open.

The exception Mode_Error is propagated by an attempt to read from, or test
for the end of, a file whose current mode is Out_File or Append_File, and
also by an
attempt to write to a file whose current mode is In_File. In the case
of Text_IO, the exception Mode_Error is also propagated by specifying a file
whose current mode is Out_File or Append_File in a call of Set_Input, Skip_Line,
End_Of_Line, Skip_Page, or End_Of_Page; and by specifying a file whose
current mode is In_File in a call of Set_Output, Set_Line_Length,
Set_Page_Length, Line_Length, Page_Length, New_Line, or New_Page.

The exception Name_Error is propagated by a call of Create or Open if the
string given for the parameter Name does not allow the identification of
an external file. For example, this exception is propagated if the string
is improper, or, alternatively, if either none or more than one external
file corresponds to the string.

The exception Use_Error is propagated if an operation is attempted that is
not possible for reasons that depend on characteristics of the external
file. For example, this exception is propagated by the procedure Create,
among other circumstances, if the given mode is Out_File but the form
specifies an input only device, if the parameter Form specifies invalid
access rights, or if an external file with the given name already exists
and overwriting is not allowed.

The exception Device_Error is propagated if an input-output operation cannot
be completed because of a malfunction of the underlying system.

The exception End_Error is propagated by an attempt to skip (read past) the
end of a file.

The exception Data_Error can be propagated by the
procedure Read (or by the Read attribute) if the
element read cannot be interpreted as a value of the required subtype.
This exception is also propagated by a procedure Get (defined in the package
Text_IO) if the input character sequence fails to satisfy the required
syntax, or if the value input does not belong to the range of the
required subtype.

The exception Layout_Error is propagated (in text input-output) by Col,
Line, or Page if the value returned exceeds Count'Last. The exception
Layout_Error is also propagated on output by an attempt to set column or
line numbers in excess of specified maximum line or page lengths,
respectively (excluding the unbounded cases). It is also propagated by an
attempt to Put too many characters to a string.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[These exceptions are also propagated by various
other language-defined packages and operations, see the definition of those
entities for other reasons that these exceptions are propagated.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
  @ChgAdded{Version=[3],Text=[This @Chg{Version=[3],New=[subclause],Old=[clause]}
  is based in Ada 95. Later versions of
  Ada (starting with Technical Corrigendum 1) have added a number of additional
  places and reasons that cause these exceptions. In particular, TC1 says that
  stream attributes need to raise End_Error in some circumstances; Amendment 1
  adds Ada.Directories and a number of new places and reasons that Name_Error and
  Use_Error are raised. There are more. We don't want to try to update this text
  (or even this note!) for every possible reason and place that might raise one
  of these exceptions, so we add this blanket statement.]}
@end{Reason}

@end{StaticSem}


@begin{DocReq}
The implementation shall document the conditions under which
Name_Error, Use_Error and Device_Error are propagated.
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[
The conditions under which Io_Exceptions.Name_Error, Io_Exceptions.Use_Error,
and Io_Exceptions.Device_Error are propagated.]}]}
@end{DocReq}


@begin{ImplPerm}
If the associated check is too complex, an implementation need
not propagate Data_Error as part of a procedure Read
(or the Read attribute) if the value read cannot be interpreted as a value
of the required subtype.
@begin{Ramification}
An example where the implementation may choose not to
perform the check is an enumeration type with a representation clause
with @lquotes@;holes@rquotes@; in the range of internal codes.@end{ramification}
@end{ImplPerm}

@begin{Erron}
@PDefn2{Term=(erroneous execution),Sec=(cause)}@Redundant[If the element read
by the procedure Read (or by the Read attribute)
cannot be interpreted as a value of the required subtype,
but this is not detected and Data_Error is not propagated,
then the resulting value can be abnormal,
and subsequent references to the value
can lead to erroneous execution,
as explained in @RefSecNum{Data Validity}.
@PDefn{normal state of an object}
@PDefn{abnormal state of an object}]
@end{Erron}

@LabeledClause{File Sharing}

@begin{RunTime}
@PDefn{unspecified}
@Leading@;It is not specified by the language whether the same external file
can be associated with more than one file object. If such sharing
is supported by the implementation, the following effects are defined:
@begin{itemize}
Operations on one text file object do not affect the column,
line, and page numbers of any other file object.

@ChgRef{Version=[1],Kind=[Deleted],Ref=[8652/0057],ARef=[AI95-00050-01]}
@ChgDeleted{Version=[1],Text=[Standard_Input and Standard_Output are associated
with distinct external files, so operations on one of these files cannot affect
operations on the other file. In particular, reading from
Standard_Input does not affect the current page, line, and column
numbers for Standard_Output, nor does writing to Standard_Output
affect the current page, line, and column numbers for Standard_Input.]}

For direct and stream files, the current index is a
property of each file object;
an operation on one file object does not affect the current
index of any other file object.

For direct and stream files, the current size of the file is a property of
the external file.
@end{itemize}

All other effects are identical.
@end{RunTime}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0057],ARef=[AI95-00050-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Removed the incorrect statement
  that the external files associated with the standard input, standard output,
  and standard error files are distinct.]}
@end{DiffWord95}
