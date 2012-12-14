@Part(impldef, Root="ada.mss")

@Comment{$Date: 2012/11/28 23:53:05 $}
@LabeledRevisedInformativeAnnex{Version=[2],New=[Summary of Documentation Requirements],Old=[Implementation-Defined Characteristics]}

@comment{$Source: e:\\cvsroot/ARM/Source/impldef.mss,v $}
@comment{$Revision: 1.17 $}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@ChgAdded{Version=[2],Text=[@Defn{documentation requirements}
The Ada language allows for certain target machine dependences in a controlled
manner. Each Ada implementation must document many characteristics and
properties of the target system. This International Standard contains
specific documentation requirements. In addition, many characteristics
that require documentation
are identified throughout this International Standard as being
implementation defined. Finally, this International Standard requires
documentation of
whether implementation advice is followed. The following
@Chg{Version=[3],New=[subclauses],Old=[clauses]} provide
summaries of these documentation requirements.]}
@end{Intro}

@LabeledAddedClause{Version=[2],Name=[Specific Documentation Requirements]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[documentation requirements], Sec=(summary of requirements)}
@Defn{documentation (required of an implementation)}
In addition to implementation-defined characteristics, each Ada implementation
must document various properties of the implementation:]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Most of the items in this list require
documentation only for implementations that conform to Specialized Needs
Annexes.]}
@end{Ramification}

@AddedDocReqList{Version=[2]}

@end{Intro}

@LabeledAddedClause{Version=[2],Name=[Implementation-Defined Characteristics]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised]}
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

@LabeledAddedClause{Version=[2],Name=[Implementation Advice]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[implementation advice], Sec=(summary of advice)}
@Defn{documentation (required of an implementation)}
This International Standard sometimes gives advice about handling certain
target machine dependences. Each Ada implementation must document whether
that advice is followed:]}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Some of the items in this list require documentation only for
implementations that conform to Specialized Needs Annexes.]}
@end{Ramification}

@AddedImplAdviceList{Version=[2]}

@end{Intro}
