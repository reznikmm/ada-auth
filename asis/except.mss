@Part(frontmatter, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/except.mss,v $}
@comment{$Revision: 1.10 $ $Date: 2010/04/09 07:02:40 $}

@LabeledSection{package Asis.Exceptions}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Exceptions]}Asis.Exceptions
shall exist. The package
shall provide interfaces equivalent to those described in this clause.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Exceptions]}ASIS.Exceptions @key[is]}]}

@leading@keepnext@;ASIS exceptions are:

@begin{DescribeCode}
@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Context} : @key[exception];
@end{Example}

Raised when ASIS is passed a Context value that is not appropriate for the
operation. This exception will typically indicate that a user error
has occurred within the application.

@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0058-1]}
@ChgDeleted{Version=[2],Text=[@AdaExcDefn{ASIS_Inappropriate_Container} : @key[exception];]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[SI99-0058-1]}
@ChgDeleted{Version=[2],Text=[Raised when ASIS is passed a Container value that
is not appropriate for the operation. This exception will typically indicate
that a user error has occurred within the application.]}

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Compilation_Unit} : @key[exception];
@end{Example}

Raised when ASIS is passed a Compilation_Unit value that is not
appropriate. This exception will typically indicate that a user
error has occurred within the application.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Element} : @key[exception];
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0055-1]}
Raised when ASIS is @Chg{Version=[2],New=[passed],Old=[given]}
an Element value that is not appropriate. This
exception will typically indicate that a user error has occurred within
the application.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Line} : @key[exception];
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0055-1]}
Raised when ASIS is @Chg{Version=[2],New=[passed],Old=[given]} a Line
value that is not appropriate.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Line_Number} : @key[exception];
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0055-1]}
Raised when ASIS is @Chg{Version=[2],New=[passed],Old=[given]}
a Line_Number value that is not appropriate.
This exception will typically indicate that a user error has occurred
within the application.

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0024-1]}
@ChgAdded{Version=[2],Text=[@AdaExcDefn{ASIS_Inappropriate_View} : @key[exception];]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0024-1],ARef=[SI99-0055-1]}
@ChgAdded{Version=[2],Text=[Raised when ASIS is passed an inappropriate view or profile.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0024-1]}
@ChgAdded{Version=[2],Text=[@AdaExcDefn{ASIS_Not_In_Context} : @key[exception];]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0024-1],ARef=[SI99-0054-1]}
@ChgAdded{Version=[2],Text=[Raised for any operation defined in Asis.Views,
Asis.Subtype_Views, Asis.Object_Views, Asis.Program_Units, Asis.Profiles,
Asis.Callable_Views, Asis.Package_Views, Asis.Generic_Views,
Asis.Exception_Views, Asis.Statement_Views, and their subpackages and children,
if the result is an entity that is not part of the current context.]}

@begin{Example}
@AdaExcDefn{ASIS_Failed} : @key[exception];
@end{Example}

This is a catch-all exception that may be raised for different reasons
in different ASIS implementations. All ASIS routines may raise ASIS_Failed
whenever they cannot normally complete their operation. This exception
will typically indicate a failure of the underlying ASIS implementation.

@begin{SingleNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0024-1]}
@ChgAdded{Version=[2],Text=[ASIS_Not_In_Context can happen, for instance, if the
View is of an incomplete view, and the full view is not part of the context. It
also can happen if the context includes only a portion of the semantic
dependencies of the queried unit.]}
@end{SingleNote}


@end{DescribeCode}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Exceptions;]}
@end{Example}
