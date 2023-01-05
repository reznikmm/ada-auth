@Part(RM_Term, Root="ada.mss")

@Comment{$Date: 2023/01/05 05:49:12 $}
@Comment{$Source: e:\\cvsroot/ARM/Source/rm_term.mss,v $}
@Comment{$Revision: 1.4 $}

@Comment{The RM version of "Terms and Definitions". Note that the ISO version 
only shares generated content, so we don't try to share any part of this
clause.}

@LabeledRevisedClause{Version=[3],New=[Terms and Definitions],Old=[Definitions]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00415-01]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0443-1]}
@Defn2{Term=[italics],Sec=(terms introduced or defined)}
Terms are defined throughout this @IntlStdName,
indicated by @i(italic) type.
Terms explicitly defined in this @IntlStdName are not to be presumed to
refer implicitly to similar terms defined elsewhere.
@Chg{Version=[2],New=[Mathematical terms not defined in this
@IntlStdName are to be interpreted according to the 
@i<CRC Concise Encyclopedia of Mathematics, Second Edition>. Other 
terms],Old=[Terms]} not defined in this
@IntlStdName are to be interpreted according to
the @i(Webster's Third New International Dictionary of the
English Language).
Informal descriptions of some terms are also given @Chg{Version=[5],New=[below],Old=[in
@RefSec{Glossary}]}.
@Comment{These are here to avoid a blank paragraph at the end, and because
they have to be somewhere.}
@SeeAlso{Primary=[library unit], Other=(language-defined library units)}
@SeeOther{Primary=[predefined library unit], Other=(language-defined library units)}
@SeeAlso{Primary=[type], Other=(language-defined types)}
@SeeOther{Primary=[predefined type], Other=(language-defined types)}
@begin{Discussion}
The index contains an entry for every defined term.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00415-01]}
@ChgAdded{Version=[2],Text=[The contents of the @i<CRC Concise Encyclopedia of
Mathematics, Second Edition> can be accessed on
@URLLink{URL=[http://www.mathworld.com],Text=[http://www.mathworld.com]}.
The ISBN number of the book is ISBN 1584883472.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0443-1]}
@ChgAdded{Version=[5],Text=[The definitions found in this subclause are
short, descriptive definitions. These are most useful for comprehension when
an exact meaning is not needed. More formal, detailed definitions are found in
the body of the @IntlStdTitle; these are needed to determine the exact
application of rules to constructs and entities.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0443-1]}
@ChgAdded{Version=[5],Text=[To see the difference, consider
@ldquote@;aliased view@rdquote. The descriptive definition is
@ldquote@;a view of an object that can be designated by an access value@rdquote.
The formal definition is a lengthy paragraph in @RefSecNum{Access Types} that
defines 10 separate constructs as aliased views. In order to reason about
a language rule that requires an aliased view in a particular context, one
needs to look at the list of constructs in order to determine if the object
in hand is actually an aliased view. The descriptive definition is no help
in this case.]}
@end{Discussion}
@begin{GlossaryMarker}
Each term defined in @RefSecNum{Glossary} is marked like this.
@end{GlossaryMarker}
@begin{TermMarker}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0443-1]}
@ChgAdded{Version=[5],Text=[Each term defined in this subclause is marked
like this in the body of the AARM near its detailed formal definition.]}
@end{TermMarker}
@end{Intro}

@begin{Discussion}
@ChgRef{Version=[1],Kind=[Revised]}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
Here are some AARM-only definitions:
@Defn{Ada Rapporteur Group (ARG)}
@Defn(ARG)
The Ada Rapporteur Group (ARG) interprets the @Chg{Version=[1],New=<Ada Reference Manual>,Old=<RM83>}.
@Defn{Ada Issue (AI)}
@Defn(AI)
An Ada Issue (AI) is a numbered ruling from the ARG.@Chg{Version=[1],New=< Ada Issues
created for Ada 83 are denoted as "AI83", while Ada Issues created for Ada 95
are denoted as "AI95" in this document.@Chg{Version=[3],New=< Similarly,
Ada Issues created for Ada 2005 are denoted as "AI05"@Chg{Version=[5],New=[,
Ada Issues created for Ada 2012 are denoted as "AI12", and Ada Issues created
for Ada 2022 are denoted as "AI22"],Old=[]}.>,Old=<>}>,Old=<>}
@Defn{Ada Commentary Integration Document (ACID)}
@Defn(ACID)
The Ada Commentary Integration Document (ACID)
is an edition of @Chg{Version=[2],New=[the Ada 83 RM],Old=[RM83]}
in which clearly marked insertions
and deletions indicate the effect of integrating the approved AIs.
@Defn{Uniformity Rapporteur Group (URG)}
@Defn(URG)
The Uniformity Rapporteur Group (URG) @Chg{Version=[1],New=<issued>,Old=<issues>}
recommendations intended to increase uniformity across Ada implementations.
@Chg{Version=[1],New=<The functions of the URG have been assumed by the ARG.>,Old=<>}
@Defn{Uniformity Issue (UI)}
@Defn(UI)
A Uniformity Issue (UI) @Chg{Version=[1],New=<was>,Old=<is>} a numbered recommendation from the URG.
@Chg{Version=[1],New=<A Defect Report and Response is an official query to WG9 about an
error in the standard. Defect Reports are processed by the ARG, and are
referenced here by their ISO numbers: 8652/nnnn. Most changes to the Ada 95
standard include reference(s) to the Defect Report(s) that prompted the change.>,Old=<>}
@Chg{Version=[1],New=[@PDefn2{Term=[ACVC],Sec=(Ada Compiler Validation Capability)}
@PDefn2{Term=[Ada Compiler Validation Capability],Sec=(ACVC)}
@PDefn2{Term=[ACATS],Sec=(Ada Conformity Assessment Test Suite)}
@PDefn2{Term=[Ada Conformity Assessment Test Suite],Sec=(ACATS)}
The @i<Ada Conformity Assessment Test Suite (ACATS)> is a set of tests intended
to check the conformity of Ada implementations to this standard. This set of
tests was previously known as the Ada Compiler Validation Capability (ACVC).],Old=[]}
@end{Discussion}

@Comment{The automatically generated terms and definitions (used to be the Glossary) follow.}

@LabeledAddedSubClause{Version=[5],Name=[Types, Objects, and their Properties]}

@AddedTermList{Version=[5],Group=[T]}

@LabeledAddedSubClause{Version=[5],Name=[Subprograms and their Properties]}

@AddedTermList{Version=[5],Group=[S]}

@LabeledAddedSubClause{Version=[5],Name=[Other Syntactic Constructs]}

@AddedTermList{Version=[5],Group=[C]}

@LabeledAddedSubClause{Version=[5],Name=[Runtime Actions]}

@AddedTermList{Version=[5],Group=[R]}

@LabeledAddedSubClause{Version=[5],Name=[Exceptional Situations]}

@AddedTermList{Version=[5],Group=[E]}

