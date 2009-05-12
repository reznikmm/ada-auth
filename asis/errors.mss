@Part(frontmatter, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/errors.mss,v $}
@comment{$Revision: 1.3 $ $Date: 2009/05/09 06:28:46 $}

@LabeledSection{package Asis.Errors}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Errors]}Asis.Errors
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Errors]}ASIS.Errors @key[is]}]}

ASIS reports all operational errors by raising an exception. Whenever an
ASIS implementation raises one of the exceptions declared in package
Asis.Exceptions, it will previously have set the values
returned by the
Status and Diagnosis queries to indicate the cause of the error. The
possible values for Status are indicated in the definition of Error_Kinds
below, with suggestions for the associated contents of the Diagnosis
string as a comment.

The Diagnosis and Status queries are provided in the Asis.Implementation
package to supply more information about the reasons for raising any
exception.

ASIS applications are encouraged to follow this same convention whenever
they explicitly raise any ASIS exception--always record a Status and
Diagnosis prior to raising the exception.

@LabeledClause{type Error_Kinds}

This enumeration type describes the various kinds of errors.

@begin{Example}
@tabset{P49}
@key[type] @AdaTypeDefn{Error_Kinds} @key[is] (

    @AdaObjDefn{Not_An_Error},@\-- @examcom{No error is presently recorded}

    @AdaObjDefn{Value_Error},@\-- @examcom{Routine argument value invalid}
    @AdaObjDefn{Initialization_Error},@\-- @examcom{ASIS is uninitialized}
    @AdaObjDefn{Environment_Error},@\-- @examcom{ASIS could not initialize}
    @AdaObjDefn{Parameter_Error},@\-- @examcom{Bad Parameter given to Initialize}
    @AdaObjDefn{Capacity_Error},@\-- @examcom{Implementation overloaded}
    @AdaObjDefn{Name_Error},@\-- @examcom{Context/unit not found}
    @AdaObjDefn{Use_Error},@\-- @examcom{Context/unit not use/open-able}
    @AdaObjDefn{Data_Error},@\-- @examcom{Context/unit bad/invalid/corrupt}
    @AdaObjDefn{Text_Error},@\-- @examcom{The program text cannot be located}
    @AdaObjDefn{Storage_Error},@\-- @examcom{Storage_Error suppressed}
    @AdaObjDefn{Obsolete_Reference_Error},@\-- @examcom{Argument or result is invalid due to}
@\-- @examcom{and inconsistent compilation unit}
    @AdaObjDefn{Unhandled_Exception_Error},@\-- @examcom{Unexpected exception suppressed}
    @AdaObjDefn{Not_Implemented_Error},@\-- @examcom{Functionality not implemented}
    @AdaObjDefn{Internal_Error});@\-- @examcom{Implementation internal failure}
@end{Example}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Errors;]}
@end{Example}
