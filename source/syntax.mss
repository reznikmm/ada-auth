@Part(syntax, Root="ada.mss")

@Comment{$Date: 2023/10/04 05:40:21 $}
@LabeledInformativeAnnex{Syntax Summary}

@comment{$Source: e:\\cvsroot/ARM/Source/syntax.mss,v $}
@comment{$Revision: 1.24 $}

@NoParaNum@;This Annex summarizes the complete syntax of the language.

@LabeledClause{Syntax Rules}@ChgNote{This clause heading is new
in Ada 2022, so we don't have a single clause}

@begin{Intro}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0426-1]}
@Defn2{Term=[syntax], Sec=(complete listing)}
@Defn2{Term=[grammar], Sec=(complete listing)}
@Defn2{Term=[context free grammar], Sec=(complete listing)}
@Defn2{Term=[BNF (Backus-Naur Form)], Sec=(complete listing)}
@Defn2{Term=[Backus-Naur Form (BNF)], Sec=(complete listing)}
@Chg{Version=[5],New=[This subclause lists the complete syntax of
the language in the order it appears in this 
@IntlStdTitle. ],Old=[]}See 
@RefSecNum{Method of Description and Syntax Notation} for a 
description of the notation used.
@end{Intro}

@SyntaxSummary

@NotIsoRMNewPageVer{Version=[5]}@Comment{For printed Ada 2022 RM only}
@LabeledClause{Syntax Cross Reference}@ChgNote{This was not a 
clause until Ada 2022, but it probably always should have been 
in the TOC and a separate HTML file}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@ChgAdded{Version=[2],Text=[In the following syntax cross reference, each
syntactic category is followed by the
@Chg{Version=[3],New=[subclause],Old=[clause]} number where it is defined.
In addition, each syntactic category @i{S} is followed by a list of the
categories that use @i{S} in their definitions. For example, the first
listing below shows that @nt{abort_statement} appears in the definition of
@nt{simple_statement}.]}
@Comment{This explanation is a simplified version of the Ada 83 one.}
@Defn2{Term=[syntax], Sec=(cross reference)}
@Defn2{Term=[grammar], Sec=(cross reference)}
@Defn2{Term=[context free grammar], Sec=(cross reference)}
@Defn2{Term=[BNF (Backus-Naur Form)], Sec=(cross reference)}
@Defn2{Term=[Backus-Naur Form (BNF)], Sec=(cross reference)}
@end{Intro}

@SyntaxXRef

