@Part(frontmatter, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/impl.mss,v $}
@comment{$Revision: 1.3 $ $Date: 2006/09/22 04:39:59 $}

@LabeledSection{package Asis.Implementation}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Implementation]}Asis.Implementation
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[with] Asis.Errors;@*
@key[package] @ChildUnit{Parent=[Asis],Child=[Implementation]}Asis.Implementation @key[is]}]}

Asis.Implementation provides queries to initialize, finalize, and query the
error status of the ASIS Implementation.


@LabeledClause{function ASIS_Version}

@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{ASIS_Version} @key[return] Wide_String;
@end{Example}

@Comment{Moved from below}
@ChgAdded{Version=[1],Text=[Returns a value which identifies the version of the ASIS interface, e.g., "2.1"]}

@end{DescribeCode}


@LabeledClause{function ASIS_Implementor}

@begin{DescribeCode}
@begin{Example}
@Key[function] @AdaSubDefn{ASIS_Implementor} @key[return] Wide_String;
@end{Example}

@Comment{Moved from below}
@ChgAdded{Version=[1],Text=[Returns a value which identifies the name of the implementor, e.g., "Ada Inc."]}
@end{DescribeCode}


@LabeledClause{function ASIS_Implementor_Version}

@begin{DescribeCode}
@begin{Example}
@Key[function] @AdaSubDefn{ASIS_Implementor_Version} @Key[return] Wide_String;
@end{Example}

@Comment{Moved from below}
@ChgAdded{Version=[1],Text=[Returns a value which identifies the implementation's version, e.g., "5.2a"]}
@end{DescribeCode}


@LabeledClause{function ASIS_Implementor_Information}

@begin{DescribeCode}
@begin{Example}
@Key[function] @AdaSubDefn{ASIS_Implementor_Information} @Key[return] Wide_String;
@end{Example}

@Comment{Moved these parts where they belong}
@Chg{Version=[1],New=[Returns a value which identifies additional ],
Old=[Returns values which identify:@*
ASIS_Version - the version of the ASIS interface, e.g., "2.1"@*
ASIS_Implementor - the name of the implementor, e.g., "Ada Inc."@*
ASIS_Implementor_Version - the implementation's version, e.g., "5.2a"@*
ASIS_Implementor_Information - ]}implementation information,
e.g., "Copyright ..."
@end{DescribeCode}


@LabeledClause{function Is_Initialized}

@begin{DescribeCode}
@begin{Example}
@Key[function] @AdaSubDefn{Is_Initialized} @Key[return] Boolean;
@end{Example}

Returns True if ASIS is currently initialized.
@end{DescribeCode}


@LabeledClause{procedure Initialize}

@begin{DescribeCode}
@begin{Example}
@Key[procedure] @AdaSubDefn{Initialize} (Parameters : @key[in] Wide_String := "");
@end{Example}

Parameters  - Specifies implementation specific parameters.

Performs any necessary initialization activities. This shall be invoked
at least once before any other ASIS services are used. Parameter values
are implementation dependent. The call is ignored if ASIS is already
initialized. All ASIS queries and services are ready for use once this
call completes.

Raises ASIS_Failed if ASIS failed to initialize or if the Parameters
argument is invalid. Status is Environment_Error or Parameter_Error.
@end{DescribeCode}

@begin{Notes}
The ASIS implementation may be Initialized and Finalized any number of
times during the operation of an ASIS program.  However, all existing
Context, Compilation_Unit and Element values become invalid when
ASIS Is_Finalized. Subsequent calls to ASIS queries or services using
such invalid Compilation_Unit or Element values will cause
ASIS_Inappropriate_Context to be raised.
@end{Notes}

@LabeledClause{function Is_Finalized}

@begin{DescribeCode}
@begin{Example}
@Key[function] @AdaSubDefn{Is_Finalized} @Key[return] Boolean;
@end{Example}

Returns True if ASIS is currently finalized or if ASIS has never been
initialized.
@end{DescribeCode}

@LabeledClause{procedure Finalize}

@begin{DescribeCode}
@begin{Example}
@Key[procedure] @AdaSubDefn{Finalize} (Parameters : @Key[in] Wide_String := "");
@end{Example}

Parameters  - Specifies any implementation required parameter values.

Performs any necessary ASIS termination activities. This should be invoked
once following the last use of other ASIS queries. Parameter values are
implementation dependent. The call is ignored if ASIS is already finalized.
Subsequent calls to ASIS Environment, Compilation_Unit, and Element queries,
are erroneous while the environment Is_Finalized.

Raises ASIS_Failed if the ASIS implementation failed to finalize. Status
is likely to be Internal_Error and will not be Not_An_Error.
Whenever an error condition is detected, and any ASIS exception is raised,
an Asis.Errors.Error_Kinds value and a Diagnosis string is stored. These
values can be retrieved by the Status and Diagnosis functions. The
Diagnosis function will retrieve the diagnostic message describing the error.

Error information always refers to the most recently recorded error.

Note that Diagnosis values are implementation dependent and may vary
greatly among ASIS implementations.
@end{DescribeCode}

@LabeledClause{function Status}

@begin{DescribeCode}
@begin{Example}
@Key[function] @AdaSubDefn{Status} @Key[return] Asis.Errors.Error_Kinds;
@end{Example}

Returns the Error_Kinds value for the most recent error.
@end{DescribeCode}

@LabeledClause{function Diagnosis}

@begin{DescribeCode}
@begin{Example}
@Key[function] @AdaSubDefn{Diagnosis} @Key[return] Wide_String;
@end{Example}

Returns a string value describing the most recent error.

Will typically return a null string if Status = Not_An_Error.
@end{DescribeCode}

@LabeledClause{procedure Set_Status}

@begin{DescribeCode}
@begin{Example}
@Key[procedure] @AdaSubDefn{Set_Status}
    (Status    : @Key[in] Asis.Errors.Error_Kinds := Asis.Errors.Not_An_Error;
     Diagnosis : @Key[in] Wide_String        := "");
@end{Example}

Status      - Specifies the new status to be recorded
Diagnosis   - Specifies the new diagnosis to be recorded

Sets (clears, if the defaults are used) the Status and Diagnosis
information. Future calls to Status will return this Status (Not_An_Error)
and this Diagnosis (a null string).

Raises ASIS_Failed, with a Status of Internal_Error and a Diagnosis of
a null string, if the Status parameter is Not_An_Error and the Diagnosis
parameter is not a null string.

@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Implementation;]}
@end{Example}
