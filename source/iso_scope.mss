@Part(01, Root="ada.mss")

@PrefaceSection{} @Comment{Go to odd page.}

@begin{ISOOnly}
@begin{Comment}
The following stuff is to get the "INTERNATIONAL STANDARD" title between
two horizontal rules at the top of page 1. (RLB-The following is a hack:
I probably ought to add a style just for this purpose)
@end{Comment}

@ChgDeleted{Version=[5],Text=[@thickline]}

@begin{WideAbove}
@ChgDeleted{Version=[5],Noparanum=[T],Type=[Leading],Text=[@tabclear()@tabset(P45)
@Swiss<@Grow[@B{INTERNATIONAL STANDARD@\ISO/IEC 8652:@Chg{Version=[2],New=[@Chg{Version=[5],New=[202x],Old=[@Chg{Version=[3],New=[2012(E)@Chg{Version=[4],New=[ with Cor 1:2016],Old=[]}],Old=[2007(E), Ed. 3]}]}],Old=[@Chg{Version=[1], New=[1995(E) with COR.1:2001], Old=[1995(E)]}]}}]>]}
@end{WideAbove}

@ChgDeleted{Version=[5],Text=[@thickline]}

@begin{Title}
@noparanum@ @*@comment{Some vertical whitespace}

@noparanum@Swiss{@b{Information technology @Em Programming@Chg{Version=[5],New=[ Languages @Em Ada],Old=[]}}}@Chg{Version=[5],New=[],Old=[@*
@noparanum@Swiss{@b{Languages @Em Ada}}]}

@noparanum@ @*@comment{Some vertical whitespace}
@end{Title}
@end{ISOOnly}

@LabeledSectionNoBreak{Scope}
@Comment{$Date: 2023/01/05 05:49:12 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/iso_scope.mss,v $}
@Comment{$Revision: 1.2 $}

@begin{Intro}
This @IntlStdTitle specifies the form and meaning of programs written in Ada.
Its purpose is to promote the portability of Ada programs to a variety
of @Chg{Version=[3],New=[computing],Old=[data processing]} systems.
@end{Intro}

@Comment{The contents of Extent.Mss follow directly here, no header is needed.}
