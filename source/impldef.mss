@Part(impldef, Root="ada.mss")
@Modify(Appendix, Numbered <@A.>, Referenced <@A>)

@Comment{$Date: 2000/04/30 02:44:41 $}
@LabeledInformativeAnnex{Implementation-Defined Characteristics}

@comment{$Source: e:\\cvsroot/ARM/Source/impldef.mss,v $}
@comment{$Revision: 1.9 $}

@begin{Intro}


@Defn2{Term=[implementation defined], Sec=(summary of characteristics)}
The Ada language allows for certain machine dependences in a controlled
manner.
@Defn{documentation (required of an implementation)}
Each Ada implementation must document all implementation-defined
characteristics:
@begin{Ramification}
@Defn{unspecified}
@Defn{specified (not!)}
It need not document unspecified characteristics.

Some of the items in this list require documentation only for
implementations that conform to Specialized Needs Annexes.
@end{Ramification}
@begin{Itemize}
@BackPlace{ImplDefList}
@end{Itemize}
@end{Intro}
