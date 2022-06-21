@Part(01, Root="ada.mss")

@Comment{$Date: 2022/05/14 04:06:51 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/extent.mss,v $}
@Comment{$Revision: 1.1 $}

@Comment{The subclause header, if any, is found in ISO_Scope.Mss and RM_Scope.Mss.}

@begin{Intro}
@Leading@keepnext@;This @IntlStdTitle specifies:
@begin(Itemize)
     The form of a program written in Ada;

     The effect of translating and executing such a program;

     The manner in which program units may be combined to form Ada
     programs;

     The language-defined library units that a conforming implementation
     is required to supply;

@Chgref{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
     The permissible variations @Chg{Version=[5],New=[in conformance to the
     rules of this @intlstdname],Old=[within the standard]}, and the manner in
     which they are to be documented;

@Chgref{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
     Those violations of the @Chg{Version=[5],New=[requirements of this
     @intlstdname],Old=[standard]} that a conforming implementation
     is required to detect, and the effect of attempting to translate or
     execute a program containing such violations;

@Chgref{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
     Those violations of the @Chg{Version=[5],New=[requirements of this
     @intlstdname],Old=[standard]} that a conforming implementation
     is not required to detect.
@end(Itemize)

@begin{WideAbove}
@Leading@keepnext@;This @IntlStdTitle does not specify:
@end{WideAbove}
@begin(Itemize)
     The means whereby a program written in Ada is transformed into
     object code executable by a processor;

     The means whereby translation or execution of programs is invoked
     and the executing units are controlled;

     The size or speed of the object code, or the relative execution
     speed of different language constructs;

     The form or contents of any listings produced by implementations;
     in particular, the form or contents of error or warning messages;

@Chgref{Version=[5],Kind=[Revised],ARef=[AI12-0425-1]}
     The effect of unspecified execution@Chg{Version=[5],New=[;],Old=[.]}

     The size of a program or program unit that will exceed the capacity
     of a particular conforming implementation.
@end(Itemize)
@end{Intro}

