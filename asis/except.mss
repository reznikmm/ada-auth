@Part(frontmatter, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/except.mss,v $}
@comment{$Revision: 1.1 $ $Date: 2006/07/25 23:43:19 $}

@LabeledSection{package Asis.Exceptions}

Package @ChildUnit{Parent=[Asis],Child=[Exceptions]}Asis.Exceptions is defined
with the contents described below.

@leading@;ASIS exceptions are:

@begin{DescribeCode}
@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Context} : @key[exception];
@end{Example}

Raised when ASIS is passed a Context value that is not appropriate for the
operation. This exception will typically indicate that a user error
has occurred within the application.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Container} : @key[exception];
@end{Example}

Raised when ASIS is passed a Container value that is not appropriate for
the operation. This exception will typically indicate that a user error
has occurred within the application.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Compilation_Unit} : @key[exception];
@end{Example}

Raised when ASIS is passed a Compilation_Unit value that is not
appropriate. This exception will typically indicate that a user
error has occurred within the application.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Element} : @key[exception];
@end{Example}

Raised when ASIS is given an Element value that is not appropriate. This
exception will typically indicate that a user error has occurred within
the application.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Line} : @key[exception];
@end{Example}

Raised when ASIS is given a Line value that is not appropriate.

@begin{Example}
@AdaExcDefn{ASIS_Inappropriate_Line_Number} : @key[exception];
@end{Example}

Raised when ASIS is given a Line_Number value that is not appropriate.
This exception will typically indicate that a user error has occurred
within the application.

@begin{Example}
@AdaExcDefn{ASIS_Failed} : @key[exception];
@end{Example}

This is a catch-all exception that may be raised for different reasons
in different ASIS implementations. All ASIS routines may raise ASIS_Failed
whenever they cannot normally complete their operation. This exception
will typically indicate a failure of the underlying ASIS implementation.
@end{DescribeCode}

