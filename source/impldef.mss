@Part(impldef, Root="ada.mss")

@Comment{$Date: 2000/05/16 04:48:24 $}
@LabeledInformativeAnnex{Implementation-Defined Characteristics}

@comment{$Source: e:\\cvsroot/ARM/Source/impldef.mss,v $}
@comment{$Revision: 1.10 $}

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
@ImplDefList
@end{Intro}
