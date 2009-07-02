@Part(io-idl, Root="asis.msm")

@Comment{$Date: 2009/07/02 04:50:54 $}

@comment{$Source: e:\\cvsroot/ARM/ASIS/io-idl.mss,v $}
@comment{$Revision: 1.4 $}

@LabeledInformativeAnnex{Miscellaneous ASIS I/O and IDL approaches}

This Annex contains examples of miscellaneous approaches to deal with I/O and
IDL issues. The first two portions contain packages for application I/O of ASIS
types: Portable_Data and Id. The third portion contains an approach to
providing an ASIS IDL. Annex C consists of:

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[@RefSec{package Portable_Data_Io} @em I/O for type
Portable_Data from Asis.Data_Decomposition]}

@RefSec{package Asis.Ids.Id_Io} @em I/O for type Id from Asis.Ids

@RefSec{Using ASIS with CORBA and IDL}
@end{Itemize}


@ChgNote{Removed SI-47}
@LabeledDeletedClause{Version=[2],Name=[package Portable_Data_Io]}


@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0047-1]}
@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
----------------------------------------------------------------------------
Annex C.1  package Portable_Data_Io
----------------------------------------------------------------------------
----------------------------------------------------------------------------
@key[with] Asis;
@key[with] Asis.Data_Decomposition.Portable_Transfer;
@key[with] Ada.Io_Exceptions;
@key[with] Ada.Sequential_Io;
----------------------------------------------------------------------------
@key[package] Portable_Data_Io @key[is]
----------------------------------------------------------------------------
Portable_Data_Io]}

@ChgDeleted{Version=[2],Text=[A package for reading/writing sequential portable data streams.
----------------------------------------------------------------------------
It provides support for logging and delogging of application data using
ASIS.]}

@ChgDeleted{Version=[2],Text=[These interfaces do not make use of the normal ASIS implementation.
This is a standalone package. It is not necessary to initialize ASIS
before using this interface.]}

@ChgDeleted{Version=[2],Text=[This interface is intended to be separate from the normal ASIS
implementation. That is, linking with this interface does not cause the
full ASIS implementation to be linked with an application. This keeps the
memory cost of this facility to a minimum. However, it also means that the
normal ASIS exceptions, Status, and Diagnosis facilities are not available.
This interface can propagate exceptions other than those defined in
Asis.Errors. In particular, the I/O facilities can raise any of the
normal Ada I/O exceptions from package Ada.Io_Exceptions.]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.1  type File_Type
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[type] File_Type @key[is limited private];]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.2  type File_Mode
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[type] File_Mode @key[is] (In_File,
                       Out_File);]}

@ChgDeleted{Version=[2],Text=[------------------------------------------------------------------------------
Annex C.1.3  procedure Create
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[procedure] Create (File           : @key[in out] File_Type;
                      Mode           : @key[in]     File_Mode := Out_File;
                      Name           : @key[in]     Wide_String    := "";
                      Form           : @key[in]     Wide_String    := "");]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.4  procedure Open
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[procedure] Open   (File           : @key[in out] File_Type;
                      Mode           : @key[in]     File_Mode;
                      Name           : @key[in]     Wide_String;
                      Form           : @key[in]     Wide_String := "");]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.5  procedure Close
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[procedure] Close  (File           : @key[in out] File_Type);]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.6  procedure Delete
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[procedure] Delete (File           : @key[in out] File_Type);]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.7  procedure Reset
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[procedure] Reset  (File           : @key[in out] File_Type;
                      Mode           : @key[in]     File_Mode);
    @key[procedure] Reset  (File           : @key[in out] File_Type);]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.8  function Mode
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[function] Mode (File              : @key[in] File_Type) @key[return] File_Mode;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.9  function Name
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[function] Name (File              : @key[in] File_Type) @key[return] Wide_String;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.10  function Form
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[function] Form (File              : @key[in] File_Type) @key[return] Wide_String;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.11  function Is_Open
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[function] Is_Open (File           : @key[in] File_Type) @key[return] Boolean;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.12  function Read
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    -- @examcom{ Read operations for single value and array values:}]}

@ChgDeleted{Version=[2],Text=[    @key[function] Read (File              : @key[in] File_Type)
        @key[return] Asis.Data_Decomposition.Portable_Value;]}

@ChgDeleted{Version=[2],Text=[    @key[function] Read (File              : @key[in] File_Type)
        @key[return] Asis.Data_Decomposition.Portable_Data;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.13  procedure Write
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[ Write operations for single value and array values:]}

@ChgDeleted{Version=[2],Text=[   @key[procedure] Write (File             : @key[in] File_Type;
                    Data_Stream      : @key[in] Asis.Data_Decomposition.Portable_Value);]}

@ChgDeleted{Version=[2],Text=[   @key[procedure] Write (File             : @key[in] File_Type;
                    Data_Stream      : @key[in] Asis.Data_Decomposition.Portable_Data);]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.14  generic package Write_Data_Type
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[generic]
        @key[type] Data_Type @key[is private];
        @key[with function] Convert (Value : @key[in] Data_Type)
            @key[return] Asis.Data_Decomposition.Portable_Data;
        -- @examcom{Instantiated from Asis.Data_Decomposition.Portable_Transfer.Convert}
    @key[package] Write_Data_Type @key[is]
        @key[procedure] Write (File        : @key[in] File_Type;
                         Value       : @key[in] Data_Type);]}

@ChgDeleted{Version=[2],Text=[        -- @examcom{ Convenience interface. Can be more efficient, but is}
        -- @examcom{ exactly equivalent to:}
        --
        -- @examcom{     Write (File, Convert(Value));}
        --
    end Write_Data_Type;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.15  function End_Of_File
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    @key[function] End_Of_File (File       : @key[in] File_Type) @key[return] Boolean;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------
Annex C.1.16  Exceptions
----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[    Status_Error : @key[exception renames] Ada.Io_Exceptions.Status_Error;
    Mode_Error   : @key[exception renames] Ada.Io_Exceptions.Mode_Error;
    Name_Error   : @key[exception renames] Ada.Io_Exceptions.Name_Error;
    Use_Error    : @key[exception renames] Ada.Io_Exceptions.Use_Error;
    Device_Error : @key[exception renames] Ada.Io_Exceptions.Device_Error;
    End_Error    : @key[exception renames] Ada.Io_Exceptions.End_Error;
    Data_Error   : @key[exception renames] Ada.Io_Exceptions.Data_Error;]}

@ChgDeleted{Version=[2],Text=[@key[private]
    @key[package] Seq_Io @key[is new] Ada.Sequential_Io
        (Asis.Data_Decomposition.Portable_Value);
    @key[type] File_Type @key[is new] Seq_Io.File_Type;]}

@ChgDeleted{Version=[2],Text=[----------------------------------------------------------------------------]}

@ChgDeleted{Version=[2],Text=[@key[end] Portable_Data_Io;]}
@end{Example}


@LabeledClause{package Asis.Ids.Id_Io }


@begin{Example}
@key[with] Asis;
@key[with] Asis.Ids;
@key[with] Ada.Direct_Io;
@key[with] Ada.Io_Exceptions;
@key[package] Id_Io @key[is]
----------------------------------------------------------------------------
Asis.Ids.Id_Io provides Id I/O Facilities.
----------------------------------------------------------------------------
This interface is a copy of the Ada.Direct_Io interface.
The internals of this package are implementation dependent and the
amount of space taken, in an Id file, by an ASIS Id value is variable.
An ASIS Id value has a fixed 'Size, but, the size of the data represented
by that Id value can be arbitrarily large. It can contain access values.
----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.1  type File_Type
----------------------------------------------------------------------------

    @key[type] File_Type @key[is] limited private;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.2  type File_Mode
----------------------------------------------------------------------------

    @key[type] File_Mode @key[is] (In_File,
                       Inout_File,
                       Out_File);

    -- @examcom{ Count has an ASIS implementation-defined upper bound.}
    -- @examcom{ Note: An Id can take up more than one "slot" in an Id file.}
    -- @examcom{ The 1st Id in a file will probably be at From => 1, but, the 2nd}
    -- @examcom{ Id in a file is not necessarily at From => 2.}

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.3  type Count
----------------------------------------------------------------------------

    @key[type] Count @key[is new] Integer range 0 ..
        Asis.Implementation_Defined_Integer_Constant;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.4  type Positive_Count
----------------------------------------------------------------------------

    @key[subtype] Positive_Count @key[is] Count range 1 .. Count'Last;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.5  procedure Create
----------------------------------------------------------------------------

    @key[procedure] Create (File      : @key[in out] File_Type;
                      Mode      : @key[in]     File_Mode := Inout_File;
                      Name      : @key[in]     Wide_String    := "";
                      Form      : @key[in]     Wide_String    := "");

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.6  procedure Open
----------------------------------------------------------------------------

    @key[procedure] Open (File        : @key[in out] File_Type;
                    Mode        : @key[in]     File_Mode;
                    Name        : @key[in]     Wide_String;
                    Form        : @key[in]     Wide_String := "");

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.7  procedure Close
----------------------------------------------------------------------------

    @key[procedure] Close (File       : @key[in out] File_Type);

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.8  procedure Delete
----------------------------------------------------------------------------

    @key[procedure] Delete (File      : @key[in out] File_Type);

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.9  procedure Reset
----------------------------------------------------------------------------

    @key[procedure] Reset (File       : @key[in out] File_Type; Mode : File_Mode);
    @key[procedure] Reset (File       : @key[in out] File_Type);

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.10 function Mode
----------------------------------------------------------------------------

    @key[function] Mode (File         : @key[in] File_Type) @key[return] File_Mode;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.11 function Name
----------------------------------------------------------------------------

    @key[function] Name (File         : @key[in] File_Type) @key[return] Wide_String;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.12 function Form
----------------------------------------------------------------------------

    @key[function] Form (File         : @key[in] File_Type) @key[return] Wide_String;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.13 function Is_Open
----------------------------------------------------------------------------

    @key[function] Is_Open (File      : @key[in] File_Type) @key[return] Boolean;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.14 procedure Read
----------------------------------------------------------------------------
Read operations

    @key[procedure] Read (File        : @key[in]  File_Type;
                    Item        : @key[out] Asis.Ids.Id;
                    From        : @key[in]  Positive_Count);
    @key[procedure] Read (File        : @key[in]  File_Type;
                    Item        : @key[out] Asis.Ids.Id);
    @key[procedure] Read (File        : @key[in]  File_Type;
                    Item        : @key[out] Asis.Element;
                    From        : @key[in]  Positive_Count;
                    Context     : @key[in]  Asis.Context);
    @key[procedure] Read (File        : @key[in]  File_Type;
                    Item        : @key[out] Asis.Element;
                    Context     : @key[in]  Asis.Context);

------------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.15 procedure Write
----------------------------------------------------------------------------
Write operations

   @key[procedure] Write (File        : @key[in] File_Type;
                     Item       : @key[in] Asis.Ids.Id;
                     To         : @key[in] Positive_Count);
    @key[procedure] Write (File       : @key[in] File_Type;
                     Item       : @key[in] Asis.Ids.Id);
    @key[procedure] Write (File       : @key[in] File_Type;
                     Item       : @key[in] Asis.Element;
                     To         : @key[in] Positive_Count);
    @key[procedure] Write (File       : @key[in] File_Type;
                     Item       : @key[in] Asis.Element);

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.16 procedure Set_Index
----------------------------------------------------------------------------

    @key[procedure] Set_Index (File   : @key[in] File_Type;
                         To     : @key[in] Positive_Count);

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.17 function Index
----------------------------------------------------------------------------

    @key[function] Index (File        : @key[in] File_Type) @key[return] Positive_Count;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.18 function Size
----------------------------------------------------------------------------

    @key[function] Size (File         : @key[in] File_Type) @key[return] Count;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.19 function End_Of_File
----------------------------------------------------------------------------

    @key[function] End_Of_File (File  : File_Type) @key[return] Boolean;

----------------------------------------------------------------------------
Annex C.@Chg{Version=[2],New=[1],Old=[2]}.20 Exceptions
----------------------------------------------------------------------------

    -- @examcom{Exceptions}

    Status_Error : @key[exception renames] Ada.Io_Exceptions.Status_Error;
    Mode_Error   : @key[exception renames] Ada.Io_Exceptions.Mode_Error;
    Name_Error   : @key[exception renames] Ada.Io_Exceptions.Name_Error;
    Use_Error    : @key[exception renames] Ada.Io_Exceptions.Use_Error;
    Device_Error : @key[exception renames] Ada.Io_Exceptions.Device_Error;
    End_Error    : @key[exception renames] Ada.Io_Exceptions.End_Error;
    Data_Error   : @key[exception renames] Ada.Io_Exceptions.Data_Error;

@key[private]
    -- @examcom{ ASIS implementation dependent.}
    @key[package] Ids_Id_Io @key[is new] Ada.Direct_Io (Asis.Ids.Id);
    @key[type] File_Type @key[is new] Ids_Id_Io.File_Type;

----------------------------------------------------------------------------

@key[end] Id_Io;
@end{Example}


@ChgNote{Improve title - SI-47}
@LabeledRevisedClause{Version=[2],New=[Using ASIS with CORBA and IDL],Old=[Implementation approach for IDL]}

An ASIS application can use the required ASIS interfaces within a Common Object
Request Broker Architecture (CORBA)@Defn{CORBA} networked environment using the
Interface Definition Language (IDL).@Defn{IDL}

Figure C-1 depicts how the ASIS CORBA clients and servers can be created from
the ASIS API. ASIS in IDL can be obtained by reverse engineering the ASIS API
expressed in this International Standard. This can be done either manually or
through automated tools. Once the ASIS in IDL is created, CORBA clients and
servers can be created via the automatic generation of IDL described in the
remaining portion of this annex.

@PictureAlone(Alignment=[Center], Border=[None],
         Height=[434], Width=[492],
         Name=[asis_art.png],
         Descr=[ASIS CORBA usage])
@Comment{Image dimensions: Height=[490], Width=[556]}

Figure C.1 @en Generation of client/server ASIS artifacts


@LabeledSubClause{ASIS API server}

When compiled on the server side using the Ada option, an Ada specification is
created with the necessary objects to register the ASIS IDL with the ORB. This
specification should be nearly identical to the original ASIS specification.
The ASIS specification created by the compilation of the ASIS IDL should map to
the original ASIS specification supported by the implementor. In this way, an
implementor's ASIS interface can be used locally on the host node or through
the CORBA client / server mechanism as desired by the tool implementor. ASIS
implementors should be able to easily take advantage of an IDL client / server
capability with little additional work.


@LabeledSubClause{ASIS API client tool}

The ASIS IDL can be compiled on the client side producing an Ada package
specification, C++ code, or Smalltalk code. This allows tools in any language
to access the ASIS interface. Assuming the Ada compilation option, a logical
Ada package body executable is created having hooks to the ASIS server via the
ORB. If the Ada compilation option is used, then tools developed using Ada
could access this Ada specification as if the ASIS package body were locally
available. Any client tool can now use the ASIS CORBA client as an Ada
specification through an Ada with clause. Client tools in Java, in C++, or in
Smalltalk can also be easily built.


@ChgNote{Improve title - SI-47}
@LabeledRevisedSubClause{Version=[2],New=[Implementing generic subprogram
Traverse_Element in a CORBA environment],Old=[Approach to implement the
Traverse_Element generic]}

@leading@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1],ARef=[SI99-0055-1]}
@Chg{Version=[2],New=[To use ASIS in a CORBA environment, an],Old=[An]}
IDL @Chg{Version=[2],New=[must],Old=[should]} first be created from the ASIS
specification (this International Standard). Automatic IDL generators can create
IDL from Ada. Language features such as generics are problematic for this
automatic conversion as generics cannot be automatically translated into IDL.
The package Asis.Iterator is the only required ASIS package containing a
generic. The following steps should be performed to create an IDL to support the
required ASIS interfaces:

@begin{Enumerate}
Remove child package Asis.Iterator.

Remove the optional packages (Asis.Data_Decomposition and
Asis.Data_Decomposition.Portable_Transfer).

Perform a global replace of "Asis." with "ASIS_CORBA."

Rename package Asis as ASIS_CORBA.

Submit package ASIS_CORBA to an automatic IDL generation process.

Insert the attached IDL segments for Interface Iterator (see
@RefSecNum{IDL to implement generic subprogram Traverse_Element}) and
Interface Generics (see @RefSecNum{IDL to implement generic subprogram Traverse_Element}) to restore the
effects of Asis.Iterator into the automatically generated IDL. [Note: the
Interface Element and enum Traverse_Control could be automatically generated;
they are shown here as they are supporters of Interface Generics and Interface
Iterator.]

After the IDL is compiled with Ada as the target language, the ASIS_CORBA
package will appear as shown in the attached Ada code.

@leading@;On the Client side, a client can use package Asis directly when the
specification has been mapped into ASIS_CORBA. The type Asis.Element will map
into the package ASIS_CORBA.Element (there will also be the package
Asis.Elements mapping into the package ASIS_CORBA.Elements). The package
Asis.Iterator will map into ASIS_CORBA.Iterator. The generics are placed into a
separate IDL module called Interface Generics which will create the package
Asis.Generics. This will allow clients to map My_Pre_Operation and
My_Post_Operation procedures into the Pre_Operation and    Post_Operation
procedures of Asis.Generics. This mapping can be achieved by:

@begin{InnerItemize}
Implementing the ASIS_CORBA.Generics in a separate server and pass the
reference to the Generics object from the ASIS client to the ASIS server.

Using a multithreaded CORBA implementation with an additional server thread in
the client.

Implementing the client mapping to accept callbacks in the client (not
portable).

@end{InnerItemize}
@end{Enumerate}


@ChgNote{Improve title - SI-47}
@LabeledRevisedSubClause{Version=[2],New=[IDL to implement generic subprogram Traverse_Element],
Old=[IDL to implement the Traverse_Element generic]}

It is possible to automatically generate IDL from the ASIS specification. The
following interfaces for Interface Generics and Interface Iterator should be
manually inserted into the automatically generated IDL prior to compilation.
Compilation of the resulting IDL will produce the necessary artifacts to
register the IDL with the Object Request Broker (ORB) and the output Ada
depicted in @RefSecNum{Ada code output by the IDL compiler}.

@begin{Example}
module ASIS_CORBA
{
    // ...
    interface Element { };        // This is the private type Asis.Element
     enum Traverse_Control {      // This type is found in package Asis

          Continue,               // Continues the normal depth-first traversal.
          Abandon_Children,       // Prevents traversal of the current element's
                                  //   children.
          Abandon_Siblings,       // Prevents traversal of the current element's
                                  // children and remaining siblings.
          Terminate_Immediately}; // Does exactly that.
    interface Generics            // abstract
    {                             // Generic portion of Asis.Iterator.Traverse_Control

            void Pre_Operation
                (in    Element           This_Element,
                 inout Traverse_Control  What_Next);

            void Post_Operation
                (in    Element            This_Element,
                 inout Traverse_Control   What_Next);

                                  // no state info needed, children will add state

       };

    interface Iterator            // Abstract
    {                             // This is Asis.Iterator.Traverse_Element

            void Traverse_Element
                 (in    Element            Starting_From,
                  inout Traverse_Control   What_Next,
                  in    Traversal          Using);
      };

      // ...
};
@end{Example}


@LabeledSubClause{Ada code output by the IDL compiler}

The following code segment represents the Ada specification generated by an IDL
compiler for the IDL code presented in Annex C.3.4 when targeted for an Ada
output.

@begin{Example}
@key[with] CORBA.Object;
@key[package] ASIS_CORBA is
    -- @examcom{...}
    @key[type] Traverse_Control @key[is]      -- @examcom{This type is found in package Asis}
        (Continue,                -- @examcom{Continues the normal depth-first traversal.}
          Abandon_Children,       -- @examcom{Prevents traversal of the current element's}
                                  -- @examcom{children.}
          Abandon_Siblings,       -- @examcom{Prevents traversal of the current element's}
                                  -- @examcom{children and remaining siblings.}
          Terminate_Immediately); -- @examcom{Does exactly that.}
    -- @examcom{...}
    @key[package] Element @key[is]            -- @examcom{This is the private type Asis.Element}

        @key[type] Ref @key[is new] CORBA.Object.Ref @key[with null record];
        -- @examcom{Narrow/Widen functions}
        @key[function] To_Ref (From : CORBA.Any) @key[return] Ref;
        @key[function] To_Ref (From : CORBA.Object.Ref'CLASS) @key[return] Ref;

    @key[end]Element;
    -- @examcom{...}
@key[end ]ASIS_CORBA;

@key[with] CORBA.Object; @key[with] ASIS_CORBA.Element;
@key[package] ASIS_CORBA.Generics @key[is] -- @examcom{generic portion of Asis.Iterator.Traverse_Control}

    @key[type] Ref @key[is new] CORBA.Object.Ref @key[with null record];	-- @examcom{abstract}
    -- @examcom{Narrow/Widen functions}
    @key[function] To_Ref (From : CORBA.Any) @key[return] Ref;
    @key[function] To_Ref (From : CORBA.Object.Ref'CLASS) @key[return] Ref;

    @key[procedure] Pre_Operation
        (Self           : Ref;
         This_Element   : @key[in]     ASIS_CORBA.Element.Ref;
         What_Next      : @key[in out] ASIS_CORBA.Traverse_Control);

    @key[procedure] Post_Operation
         (Self          : Ref;
          This_Element  : @key[in]     ASIS_CORBA.Element.Ref;
          What_Next     : @key[in out] ASIS_CORBA.Traverse_Control);
          -- @examcom{no state info needed, children will add state}
@key[end] ASIS_CORBA.Generics

@key[with] CORBA.Object; @key[with] ASIS_CORBA.Element; @key[with] ASIS_CORBA.Generics;
@key[package] ASIS_CORBA.Iterator @key[is]

    @key[type] Ref @key[is new] CORBA.Object.Ref @key[with null record];
    -- @examcom{Narrow/Widen functions}
    @key[function] To_Ref (From : CORBA.Any) @key[return] Ref;
    @key[function] To_Ref (From : CORBA.Object.Ref'CLASS) @key[return] Ref;

    @key[procedure] Traverse_Element
       (Self          : Ref;
        Starting_From : @key[in]     ASIS_CORBA.Element.Ref;
        What_Next     : @key[in out] ASIS_CORBA.Traverse_Control;
        Using         : @key[in]     ASIS_CORBA.Generics.Ref);
        -- @examcom{Using contains state and implementations of My_Pre_Operation and}
        -- @examcom{My_Post_Operation.}
@key[end] ASIS_CORBA.Iterator;
@end{Example}

