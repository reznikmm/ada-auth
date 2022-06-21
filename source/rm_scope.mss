@Part(01, Root="ada.mss")


@Comment{$Date: 2022/05/14 04:06:53 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/rm_scope.mss,v $}
@Comment{$Revision: 1.1 $}

@LabeledClause{Scope}


@begin{Intro}
@Chgref{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
This @IntlStdTitle specifies the form and meaning of programs written in Ada.
Its purpose is to promote the portability of Ada programs to a variety
of @Chg{Version=[3],New=[computing],Old=[data processing]} systems.

@Chgref{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
@Chgref{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0441-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[Ada is a programming language
designed to support
the construction of long-lived, highly reliable software systems. The language
includes facilities to define packages of related types, objects, and
operations. The packages may be parameterized and the types may be extended to
support the construction of libraries of reusable, adaptable software
components. The operations may be implemented as subprograms using conventional
sequential control structures, or as entries that include synchronization of
concurrent threads of control as part of their invocation. Ada supports
object-oriented programming by providing classes and interfaces, inheritance,
polymorphism of variables and methods, and generic units. The language treats
modularity in the physical sense as well, with a facility to support separate
compilation.],Old=[]}]}

@Chgref{Version=[3],Kind=[AddedNormal],ARef=[AI05-0269-1],ARef=[AI05-0299-1]}
@Chgref{Version=[4],Kind=[Revised],ARef=[AI12-0056-1]}
@Chgref{Version=[5],Kind=[DeletedNoDelMsg],ARef=[AI12-0441-1]}
@ChgDeleted{Version=[5],Text=[@Chg{Version=[3],New=[The language provides rich 
support for real-time, concurrent programming, and
includes facilities for multicore and multiprocessor programming. Errors can be
signaled as exceptions and handled explicitly. The language also covers systems
programming; this requires precise control over the representation of data and
access to system-dependent properties. Finally, a predefined environment of
standard packages is provided, including facilities for, among others,
input-output, string manipulation, numeric elementary functions,
@Chg{Version=[4],New=[],Old=[and ]}random number generation, and definition and
use of containers.],Old=[]}]}

@end{Intro}

@Comment{The header for the next subclause is here, as the ISO version does not
have the following subclause (the content is part of the scope). The contents of
this clause are found in Extent.Mss.}
@LabeledSubClause{Extent}

