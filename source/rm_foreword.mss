@Part(rm_foreword, root="ada.mss")
@comment{$Source: e:\\cvsroot/ARM/Source/rm_foreword.mss,v $}
@comment{$Revision: 1.3 $ $Date: 2023/10/04 05:40:22 $}

@Comment{This version of the Foreword file is only used in non-ISO documents.}

@UnNumberedSection(Foreword)

@ChgNote{We needed boilerplate like this to avoid objections from various
outside parties. It seems unlikely that the same objections won't be raised
again.}
@begin{Intro}
@begin{NotISO}
@begin{AARMOnly}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Text=[This document is the Annotated Ada Reference Manual. The
International Standard for the programming language Ada is
ISO/IEC 8652:@Chg{Version=[4],New=[@Chg{Version=[5],New=[2023],Old=[2012]}],Old=[1995]}(E).
The International Standard is derived from the Ada Reference Manual, with various 
non-normative changes. In particular, the International Standard @Chg{Version=[5],New=[numbers
clauses differently, ],Old=[]}omits the 
annotations and paragraph numbers, eliminates the Acknowledgements, and modifies
various front matter such as the Title page and the Foreword.]}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[New ISO requirements necessitated changing the numbers of
  all of the numbered clauses and subclauses in the International Standard. The
  Ada Reference Manual retains its traditional numbering of clauses and subclauses
  as it is common for existing material (both printed and electronic) to refer to
  subclauses just by number.]}
@end{Discussion}
@end{AARMOnly}
@begin{RMOnly}
@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Text=[This document is the Ada Reference Manual. The
International Standard for the programming language Ada is
ISO/IEC 8652:@Chg{Version=[4],New=[@Chg{Version=[5],New=[2023],Old=[2012]}],Old=[1995]}(E).
The International Standard is derived from the Ada Reference Manual, with various 
non-normative changes. In particular, the International Standard @Chg{Version=[5],New=[numbers
clauses differently, ],Old=[]}omits paragraph
numbers, eliminates the Acknowledgements, and modifies various front matter
such as the Title page and the Foreword.]}
@end{RMOnly}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded]}
@ChgAdded{Version=[1],Text=[The Ada Working Group ISO/IEC JTC 1/SC 22/WG 9 is tasked by ISO with
the work item to interpret and maintain the International Standard and to
produce Technical Corrigenda, as appropriate. The technical work on the
International Standard is performed by the Ada Rapporteur Group (ARG) of WG 9.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[AXE Consultants produces the Ada Reference Manual
in consultation with the ARG, along with drafts of other documents as needed. 
ISO/IEC documents often list the individual changes that need to be made to 
the text of a Standard, rather than simply updating the document. As such, 
an International Standard is often found in several parts, while the Ada 
Reference Manual is always a single document.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded]}
@ChgRef{Version=[5],Kind=[DeletedAddedNoDelMsg]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[5],New=[],Old=[In @Chg{Version=[4],New=[June 2015],Old=[September 2000]}, 
WG 9 approved and forwarded Technical Corrigendum 1 to SC 22 for ISO approval, which was
granted in @Chg{Version=[4],New=[December 2015],Old=[February 2001]}. Technical
Corrigendum 1 was published in @Chg{Version=[4],New=[February 2016],Old=[June 2001]}.]}]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgRef{Version=[4],Kind=[DeletedAddedNoDelMsg]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Text=[@Chg{Version=[4],New=[@Chg{Version=[5],New=[In
June 2016, WG 9 approved a tentative schedule for the preparation of
an Amendment or Revision to the International Standard, with a delivery
no earlier than 2018. In July 2019, WG 9 approved an additional review and
prototyping period for this revision, extending the delivery to no earlier
than late 2020. The draft standard was delivered to WG 9 in July 2021, and the
Standard was published as ISO/IEC 8652:2023 in May 2023.],Old=[]}],Old=[In
October 2002, WG 9 approved a schedule and
guidelines for the preparation of an Amendment to the International Standard.
WG 9 approved the scope of the Amendment in June 2004. In April 2006, WG 9
approved and forwarded the Amendment to SC 22 for approval, which was granted
in August 2006. Final ISO/IEC approval @Chg{Version=[3],New=[came in January 2007,
and the Amendment was published as ISO/IEC 8652:1995/Amd 1:2007(E) in March],Old=[is expected
by early]} 2007.]}]}

@ChgRef{Version=[6],Kind=[Added]}
@ChgAdded{Version=[6],Text=[Since the
completion of Ada 2022, work has continued on Ada, both to correct errors and to add
small enhancements. Currently, there has been no determination of when or in what
form these changes will be issued. However, it is valuable to have all of the
corrections available in one place. Thus, we've created this draft update, which
for the purposes of this document, we'll Ada 202y.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0313-1],ARef=[AI12-0441-1]}
@ChgAdded{Version=[5],Text=[This Ada Reference Manual
replaces the edition of 2012. It modifies the previous edition by
making changes and additions that improve the capability of the language and
the reliability of programs written in the
language.]}

@Comment<Alternative to use for the above in draft versions:
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgRef{Version=[6],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[This version of the Ada Reference Manual shows the current
state of Ada@Chg{Version=[5],New=[@Chg{Version=[6],New=[ 202y. As Ada 202y is still under development, features
are still subject to change],Old=[ 2022.]}],Old=[@Chg{Version=[3],New=[ 2012@Chg{Version=[4],New=[ 
including the changes included in Technical Corrigendum 1],Old=[]}],Old=[, 
including the changes of Technical Corrigendum 1 and Amendment 1]}]}.]} End Comment>




@end{NotISO}
@end{Intro}
@Comment{(*End of Non-ISO Foreword (except for list of changes)*)}


@begin{Comment} (*Was Syntax9XOnly - We don't generate this document anymore*)
This document lists the syntax rules of Ada 95.
@end{Comment}
@begin{Comment} (*Was Chg839XOnly - We don't generate this document anymore*)

This document lists in detail the changes introduced in the second
(Ada 95) edition of the Ada standard (ISO/IEC 8652:1995)
with respect to the first (Ada 83) edition (ISO 8652:1987).

@end{Comment}

