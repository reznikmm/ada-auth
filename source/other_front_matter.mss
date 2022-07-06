@Part(otherfrontmatter, root="ada.mss")
@comment{$Source: e:\\cvsroot/ARM/Source/other_front_matter.mss,v $}
@comment{$Revision: 1.2 $ $Date: 2022/06/21 06:08:04 $}

@comment{These parts appear after the Introduction in RM and AARM versions only.}

@begin{NotISO}
@NewPage
@SubHeading(Instructions for Comment Submission)

@begin{Intro}


@ChgRef{Version=[1],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
@Defn{instructions for comment submission}
@Defn{comments, instructions for submission}
Informal comments on this @IntlStdTitle @Chg{Version=[5],New=[can],Old=[may]} be sent via
e-mail to @Chg{New=[@b(ada-comment@@ada-auth.org)],
Old=[@b(ada-comment@@sw-eng.falls-church.va.us)]}.
If appropriate, the Project Editor will initiate
the defect correction procedure.


Comments should use the following format:
@begin(display)
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
@TabSet(L6)@\@b(!topic) @i[Title summarizing comment]
@\@b(!reference) @Chg{Version=[2],New=[Ada @Chg{Version=[3],New=[@Chg{Version=[5],New=[2022],Old=[2012]}],Old=[2005]} RM],Old=[RM95-]}@i{ss.ss(pp)}
@\@b(!from) @i{Author Name yy-mm-dd}
@\@b(!keywords) @i{keywords related to topic}
@\@b(!discussion)
@comment{Blank line}
@\@i{text of discussion}
@end(display)

@ChgRef{Version=[3],Kind=[Revised]}
where @i(ss.ss) is the @Chg{Version=[3],New=[],Old=[section, ]}clause or
subclause number, @i(pp) is the paragraph number where applicable,
and @i(yy-mm-dd) is the date the comment was sent.
The date is optional, as is the @b(!keywords) line.

@ChgRef{Version=[1],Kind=[Revised]}
@Chg{New=[], Old=[Multiple comments per e-mail message are acceptable.]}
Please use a descriptive @lquotes@;Subject@rquotes@; in your e-mail
message@Chg{New=[, and limit each message to a single comment.], Old=[.]}

When correcting typographical errors or making minor wording
suggestions, please put the correction directly as the topic of the
comment; use square brackets [ ] to indicate text to be omitted and
curly braces { } to indicate text to be added, and provide enough
context to make the nature of the suggestion self-evident or put
additional information in the body of the comment, for example:
@begin{Display}
@TabSet(L6)@\@b(!topic) [c]{C}haracter
@\@b(!topic) it[']s meaning is not defined
@end{Display}


Formal requests for interpretations and for reporting defects in the
International Standard may be made in accordance with the ISO/IEC
JTC 1 Directives and the ISO/IEC JTC 1/SC 22 policy for interpretations.
National Bodies may submit a Defect Report to ISO/IEC JTC 1/SC 22 for resolution
under the JTC 1 procedures.
A response will be provided and, if appropriate,
a Technical Corrigendum will be issued in accordance with the procedures.

@end{Intro}
@end{NotISO}


@begin{NotISO}
@NewPage
@AddedSubHeading{Version=[3],Acknowledgements for the Ada 83 edition}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Ada is the result of a collective effort to design a
common language for programming large scale and real-time systems.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The common high order language program began in
1974. The requirements of the United States Department of Defense were
formalized in a series of documents which were extensively reviewed by the
Services, industrial organizations, universities, and foreign military
departments. The Ada language was designed in accordance with the final (1978)
form of these requirements, embodied in the Steelman specification.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The Ada design team was led by Jean D. Ichbiah and
has included Bernd Krieg-Brueckner, Brian A. Wichmann, Henry F. Ledgard,
Jean-Claude Heliard, Jean-Loup Gailly, Jean-Raymond Abrial, John G.P. Barnes,
Mike Woodger, Olivier Roubine, Paul N. Hilfinger, and Robert Firth.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[At various stages of the project, several people
closely associated with the design team made major contributions. They include
J.B. Goodenough, R.F. Brender, M.W. Davis, G. Ferran, K. Lester, L. MacLaren, E.
Morel, I.R. Nassi, I.C. Pyle, S.A. Schuman, and S.C. Vestal.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Two parallel efforts that were started in the second
phase of this design had a deep influence on the language. One was the
development of a formal definition using denotational semantics, with the
participation of V. Donzeau-Gouge, G. Kahn, and B. Lang. The other was the
design of a test translator with the participation of K. Ripken, P. Boullier, P.
Cadiou, J. Holden, J.F. Hueras, R.G. Lange, and D.T. Cornhill. The entire effort
benefitted from the dedicated assistance of Lyn Churchill and Marion Myers, and
the effective technical support of B. Gravem, W.L. Heimerdinger, and P. Cleve.
H.G. Schmitz served as program manager.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Over the five years spent on this project, several
intense week-long design reviews were conducted, with the participation of P.
Belmont, B. Brosgol, P. Cohen, R. Dewar, A. Evans, G. Fisher, H. Harte, A.L.
Hisgen, P. Knueven, M. Kronental, N. Lomuto, E. Ploedereder, G. Seegmueller, V.
Stenning, D. Taffs, and also F. Belz, R. Converse, K. Correll, A.N. Habermann,
J. Sammet, S. Squires, J. Teller, P. Wegner, and P.R. Wetherall.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Several persons had a constructive influence with
their comments, criticisms and suggestions. They include P. Brinch Hansen, G.
Goos, C.A.R. Hoare, Mark Rain, W.A. Wulf, and also E. Boebert, P. Bonnard, H.
Clausen, M. Cox, G. Dismukes, R. Eachus, T. Froggatt, H. Ganzinger, C. Hewitt,
S. Kamin, R. Kotler, O. Lecarme, J.A.N. Lee, J.L. Mansion, F. Minel, T. Phinney,
J. Roehrich, V. Schneider, A. Singer, D. Slosberg, I.C. Wand, the reviewers of
Ada-Europe, AdaTech, Afcet, those of the LMSC review team, and those of the Ada
Tokyo Study Group.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[These reviews and comments, the numerous evaluation
reports received at the end of the first and second phase, the nine hundred
language issue reports and test and evaluation reports received from fifteen
different countries during the third phase of the project, the thousands of
comments received during the ANSI Canvass, and the on-going work of the IFIP
Working Group 2.4 on system implementation languages and that of the Purdue
Europe LTPL-E committee, all had a substantial influence on the final definition
of Ada.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The Military Departments and Agencies have provided
a broad base of support including funding, extensive reviews, and countless
individual contributions by the members of the High Order Language Working Group
and other interested personnel. In particular, William A. Whitaker provided
leadership for the program during the formative stages. David A. Fisher was
responsible for the successful development and refinement of the language
requirement documents that led to the Steelman specification.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[The Ada 83 language definition was developed by Cii
Honeywell Bull and later Alsys, and by Honeywell Systems and Research Center,
under contract to the United States Department of Defense. William E. Carlson
and later Larry E. Druffel served as the technical representatives of the United
States Government and effectively coordinated the efforts of all participants in
the Ada program.]}


@SubHeading(Acknowledgements@Chg{Version=[2],New=[ for the Ada 95 edition],Old=[]})

@begin{Intro}

This @IntlStdTitle was prepared by the Ada 9X Mapping/Revision
Team based at Intermetrics, Inc., which has included:
W. Carlson, Program Manager;
T. Taft, Technical Director;
J. Barnes (consultant);
B. Brosgol (consultant);
R. Duff (Oak Tree Software);
M. Edwards;
C. Garrity;
R. Hilliard;
O. Pazy (consultant);
D. Rosenfeld;
L. Shafer;
W. White;
M. Woodger.

The following consultants to the Ada 9X Project
contributed to the Specialized Needs Annexes:
T. Baker (Real-Time/Systems Programming @em SEI, FSU);
K. Dritz (Numerics @em Argonne National Laboratory);
A. Gargaro (Distributed Systems @em Computer Sciences);
J. Goodenough (Real-Time/Systems Programming @em SEI);
J. McHugh (Secure Systems @em consultant);
B. Wichmann (Safety-Critical Systems @em NPL: UK).

This work was regularly reviewed by
the Ada 9X Distinguished Reviewers
and the members of the Ada 9X Rapporteur Group (XRG):
E. Ploedereder, Chairman of DRs and XRG (University of Stuttgart: Germany);
B. Bardin (Hughes);
J. Barnes (consultant: UK); @Comment{XRG - UK}
B. Brett (DEC);
B. Brosgol (consultant);
R. Brukardt (RR Software);
N. Cohen (IBM);
R. Dewar (NYU);
G. Dismukes (TeleSoft);
A. Evans (consultant);
A. Gargaro (Computer Sciences);
M. Gerhardt (ESL);
J. Goodenough (SEI); @Comment{Also XRG - U.S.}
S. Heilbrunner (University of Salzburg: Austria); @Comment{Also XRG - Belgium}
P. Hilfinger (UC/Berkeley); @Comment{No longer a DR.}
B. K@latin1(228)llberg (CelsiusTech: Sweden); @Comment{XRG - Sweden}
M. Kamrad II (Unisys);
J. van Katwijk (Delft University of Technology: The Netherlands); @Comment{XRG - The Netherlands}
V. Kaufman (Russia); @Comment{XRG - Russia}
P. Kruchten (Rational); @Comment{Also XRG - France}
R. Landwehr (CCI: Germany); @Comment{Also XRG - Germany}
C. Lester (Portsmouth Polytechnic: UK);
L. M@latin1(229)nsson (TELIA Research: Sweden); @Comment{No longer a DR.}
S. Michell (Multiprocessor Toolsmiths: Canada); @Comment{Also XRG - Canada}
M. Mills (US Air Force);
D. Pogge (US Navy);
K. Power (Boeing);
O. Roubine (Verdix: France);
A. Strohmeier (Swiss Fed Inst of Technology: Switzerland); @Comment{XRG - Switzerland}
W. Taylor (consultant: UK);
J. Tokar (Tartan);
E. Vasilescu (Grumman);
J. Vladik (Prospeks s.r.o.:
   Czech Republic); @Comment{XRG - Czech Republic}
S. Van Vlierberghe (OFFIS: Belgium). @Comment{XRG - Belgium}


Other valuable feedback influencing the revision
process was provided by
the Ada 9X Language Precision
Team (Odyssey Research Associates), the Ada 9X User/Implementer
Teams (AETECH, Tartan, TeleSoft), the Ada 9X Implementation
Analysis Team (New York University) and the Ada community-at-large.


Special thanks go to R. Mathis,
Convenor of ISO/IEC JTC 1/SC 22 Working Group 9.
@Comment{Also XRG - U.S.}


The Ada 9X Project was sponsored by the Ada Joint Program Office.
Christine M. Anderson at the Air Force Phillips Laboratory (Kirtland
AFB, NM) was the project manager.

@AddedSubHeading{Version=[1],Acknowledgements for the Corrigendum version}

@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{New=<The editor [R. Brukardt (USA)] would like to thank the many people
whose hard work and assistance has made this
@Chg{Version=[3],New=[update],Old=[revision]} possible.>,Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[Thanks go out to all of the members of the ISO/IEC JTC 1/SC 22/WG 9
Ada Rapporteur Group, whose work on creating and editing the wording
corrections was critical to the entire process. Especially valuable
contributions came from the chairman of the ARG, E. Ploedereder (Germany), who
kept the process moving; J. Barnes (UK) and K. Ishihata (Japan), whose
extremely detailed reviews kept the editor on his toes; G. Dismukes (USA),
M. Kamrad (USA), P. Leroy (France), S. Michell (Canada), T. Taft (USA),
J. Tokar (USA), and other members too numerous to mention.],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[Special thanks go to R. Duff (USA) for his explanations of the
previous system of formatting of these documents during the tedious conversion
to more modern formats. Special thanks also go to the convenor of
ISO/IEC JTC 1/SC 22/WG 9, J. Moore (USA), without whose help and support
the Corrigendum and this consolidated reference manual would not have been possible.],Old=[]}

@AddedSubHeading{Version=[2],Acknowledgements for the Amendment 1 version}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@Chg{Version=[2],New=<The editor [R. Brukardt (USA)] would like to thank the many
people whose hard work and assistance has made this
@Chg{Version=[3],New=[update],Old=[revision]} possible.>,Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Thanks go out to all of the members of the
ISO/IEC JTC 1/SC 22/WG 9
Ada Rapporteur Group, whose work on creating and editing the wording
corrections was critical to the entire process. Especially valuable
contributions came from the chairman of the ARG, P. Leroy (France), who kept
the process on schedule; J. Barnes (UK) whose careful reviews found many
typographical errors; T. Taft (USA), who always seemed to have a suggestion
when we were stuck, and who also was usually able to provide the valuable
service of explaining why things were as they are; S. Baird (USA), who found
many obscure problems with the proposals; and A. Burns (UK), who pushed many of
the real-time proposals to completion. Other ARG members who contributed were:
R. Dewar (USA), G. Dismukes (USA), R. Duff (USA), K. Ishihata (Japan), S.
Michell (Canada), E. Ploedereder (Germany), J.P. Rosen (France), E. Schonberg
(USA), J. Tokar (USA), and T. Vardanega (Italy).]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[Special thanks go to Ada-Europe and the Ada Resource
Association, without whose help and support the Amendment and this consolidated
reference manual would not have been possible. M. Heaney (USA) requires special
thanks for his tireless work on the containers packages. Finally, special
thanks go to the convenor of ISO/IEC JTC 1/SC 22/WG 9, J. Moore (USA), who
guided the document through the standardization process.],Old=[]}


@AddedSubHeading{Version=[3],Acknowledgements for the Ada 2012 edition}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=<The editor [R. Brukardt (USA)] would like to thank the many
people whose hard work and assistance has made this revision possible.>}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Thanks go out to all of the members of the
ISO/IEC JTC 1/SC 22/WG 9
Ada Rapporteur Group, whose work on creating and editing the wording
changes was critical to the entire process. Especially valuable
contributions came from the chairman of the ARG, E. Schonberg (USA), who
guided the work; T. Taft (USA), whose insights broke many logjams, both in
design and wording; J. Barnes (UK) whose careful reviews uncovered many
editorial errors; S. Baird (USA), who
repeatedly found obscure interactions with the proposals that the rest of
us missed. Other ARG members who substantially contributed were:
A. Burns (UK), J. Cousins (UK), R. Dewar (USA), G. Dismukes (USA), R. Duff (USA), P. Leroy
(France), B. Moore (Canada), E. Ploedereder (Germany), J.P. Rosen (France),
B. Thomas (USA), and T. Vardanega (Italy).]}@Comment{Pascal Leroy worked
extensively on this work in the early days, although he hasn't participated
recently.}

@ChgRef{Version=[3],Kind=[Added]}
@Chg{Version=[3],New=[Special thanks go to Ada-Europe and the Ada Resource
Association, without whose help and support this third edition of the
Ada Standard would not have been possible. A special mention has to go to
A. Beneschan (USA) for his efforts in eliminating sloppiness in our wording.
M. Heaney (USA) also deserves a mention for his efforts to improve the
containers packages. Finally, special thanks go to the convenor of ISO/IEC JTC
1/SC 22/WG 9, J. Tokar (USA), who guided the document through the
standardization process.],Old=[]} @Comment{The other financial contributors
wanted to remain anonymous, so they are not mentioned here.}

@AddedSubHeading{Version=[4],Acknowledgements for the Ada 2012 Corrigendum 1 version}

@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=<The editor [R. Brukardt (USA)] would like to thank the many
people whose hard work and assistance has made this update possible.>}

@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[Thanks go out to all of the members of the
ISO/IEC JTC 1/SC 22/WG 9
Ada Rapporteur Group, whose work on creating and editing the wording
changes was critical to the entire process. Especially valuable
contributions came from the chairman of the ARG, J. Cousins (UK), who
guided the work; T. Taft (USA), who seems to have the ability to cut any
Gordian knot we encounter in wording; J. Barnes (UK) who continues to be
able to find editorial errors invisible to most; S. Baird (USA), who
so frequently finds obscure interactions that we now have named such things
for him. Other ARG members who substantially contributed were:
A. Burns (UK), R. Dewar (USA), G. Dismukes (USA), R. Duff (USA),
B. Moore (Canada), E. Ploedereder (Germany), J.P. Rosen (France),
E. Schonberg (USA), and T. Vardanega (Italy).]}

@ChgRef{Version=[4],Kind=[Added]}
@Chg{Version=[4],New=[Finally, special thanks go to the convenor of ISO/IEC JTC
1/SC 22/WG 9, J. Tokar (USA), who guided the document through the
standardization process.],Old=[]} @Comment{The financial contributors
wanted to remain anonymous, so they are not mentioned here.}

@AddedSubHeading{Version=[5],Acknowledgements for the Ada 2022 version}

@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=<The editor [R. Brukardt] would like to thank the
many people whose hard work and assistance has made this revision possible.>}

@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[Thanks go out to all of the members of the
ISO/IEC JTC 1/SC 22/WG 9 Ada Rapporteur Group, whose work in all steps of the
process, from determining problems to address, reviewing feature designs, and
creating and editing wording changes, was critical to the entire process.
Especially valuable contributions came from the chairman of the
ARG through June 2018, J. Cousins,
who guided the work and ensured we followed defined procedures; his replacement
as chairman, S. Baird, who ably powered through obstacles to complete the work
while continuing to find obscure interactions; T. Taft, who often solved
difficult problems that had stumped others; B. Moore, whose frequent suggestions
for parallel constructs greatly improved the result.
Other ARG members who substantially contributed were:
R. Amiard, J. Barnes, A. Burns, A. Charlet, G. Dismukes, C. Dross, R. Duff, 
E. Ploedereder, J.P. Rosen, F. Schanda, E. Schonberg, J. Squirek, T. Vardanega, 
and R. Wai.]}
@Comment{E. Fish has 2.0 meeting points as of #62 (Oct 2019), he has not
attended since. Added his name if he reappears.}

@Comment{Leave the thanks for Pat until we get closer to done.
@ChgRef{Version=[5],Kind=[Added]}
@Chg{Version=[5],New=[Finally, special thanks go to the convenor of ISO/IEC JTC
1/SC 22/WG 9, P. Rogers, who guided the document through the
standardization process.],Old=[]} @Comment{The financial contributors
wanted to remain anonymous, so they are not mentioned here.}}
@end{Intro}
@end{NotISO}


@begin{NotISO}
@begin{Intro}
@AddedSubHeading{Version=[1],Using this version of the Ada Reference Manual}

@ChgNote{The following text should be redone once we decide what form Ada 2022
will take.}

@begin{RMOnly}
@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
@Chg{New=[This document has been revised with @Chg{Version=[4],New=[
the corrections specified in Technical Corrigendum 1 for Ada 2012
(which corresponds to ISO/IEC 8652:2012/COR.1:2016)@Chg{Version=[5],New=[ 
and other changes specifically for Ada 2022],Old=[]}],
Old=[the corrections specified in Technical Corrigendum 1 for Ada 95
(which corresponds to ISO/IEC 8652:1995/COR.1:2001)@Chg{Version=[2],New=[ and
Amendment 1 (which corresponds to ISO/IEC 8652/AMD 1:2007)@Chg{Version=[3],New=[,
along with changes specifically for this third edition],Old=[]}],Old=[]}]}.
In addition, a variety of editorial errors have been corrected.],Old=[]}
@end{RMOnly}
@begin{AARMOnly}
@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
@Chg{New=[This document has been revised with @Chg{Version=[4],New=[
the corrections specified in Technical Corrigendum 1 for Ada 2012
(which corresponds to ISO/IEC 8652:2012/COR.1:2016)@Chg{Version=[5],New=[ 
and other changes specifically for Ada 2022],Old=[]}],
Old=[the corrections specified in Technical Corrigendum 1
(which corresponds to ISO/IEC 8652:1995/COR.1:2001)@Chg{Version=[2],New=[ and
Amendment 1 (which corresponds to ISO/IEC 8652/AMD 1:2007)@Chg{Version=[3],New=[,
along with changes specifically for this third edition],Old=[]}],Old=[]}]}.
In addition, @Chg{Version=[3],New=[more],Old=[additional]} annotations
have been added and a variety of editorial errors have been corrected.],Old=[]}
@end{AARMOnly}

@begin{RMOnly}
@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
@Chg{New=[Changes to the original 1995 version of the Ada Reference Manual 
can be identified by the version
number @Chg{Version=[2],New=[],Old=[/1 ]}following the paragraph
number.@Chg{Version=[2],New=[ Paragraphs with a version number of /1 were
changed by Technical Corrigendum 1 @Chg{Version=[4],New=[for Ada 95 ],Old=[]}or
were editorial corrections at that time,
while paragraphs with a version number of /2 were changed by Amendment 1 or were
more recent editorial corrections@Chg{Version=[3],New=[, and paragraphs with a
version number of /3 were changed by the 2012 edition of the Reference Manual
or were still
more recent editorial corrections],Old=[]}.],Old=[]}@Chg{Version=[4],New=[ Paragraphs
with a version number of /4 are changed by Technical Corrigendum 1 for Ada 2012
or were editorial corrections at that time.],Old=[]}@Chg{Version=[5],New=[ Paragraphs
with a version number of /5 are changes or
editorial corrections for Ada 2022.],Old=[]}
Paragraphs not so marked are unchanged since the original 1995 edition of
the Ada Reference Manual, and have the same paragraph numbers as in that
edition. In addition, some versions of this document include revision bars 
near the
paragraph numbers. Where paragraphs are inserted, the paragraph numbers are of
the form pp.nn, where pp is the number of the preceding paragraph, and nn is an
insertion number. For instance, the first paragraph inserted after paragraph 8
is numbered 8.1, the second paragraph inserted is numbered 8.2, and so on.
Deleted paragraphs are indicated by the text @i{@shrink{This paragraph was
deleted.}} Deleted paragraphs include empty paragraphs that were numbered in
the @Chg{Version=[3],New=[1995 edition of the],Old=[original]}
Ada Reference Manual.],Old=[]}
@end{RMOnly}
@begin{AARMOnly}
@ChgRef{Version=[1],Kind=[AddedNormal]}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
@Chg{New=[Changes to the original 1995 version of the Annotated Ada Reference
Manual (AARM) can be identified by the version
number @Chg{Version=[2],New=[],Old=[/1 ]}following the paragraph
number.@Chg{Version=[2],New=[ Paragraphs with a version number of /1 were
changed by Technical Corrigendum 1 @Chg{Version=[4],New=[for Ada 95 ],Old=[]}or 
were editorial corrections at that time,
while paragraphs with a version number of /2 were changed by Amendment 1 or were
more recent editorial corrections@Chg{Version=[3],New=[, and paragraphs with a
version number of /3 were changed by the 2012 edition of the AARM
or were still
more recent editorial corrections],Old=[]}.],Old=[]}@Chg{Version=[4],New=[ Paragraphs
with a version number of /4 are changed by Technical Corrigendum 1 for Ada 2012
or were editorial corrections at that time.],Old=[]}@Chg{Version=[5],New=[ Paragraphs
with a version number of /5 are changes or
editorial corrections for Ada 2022.],Old=[]}
Paragraphs not so marked are unchanged since the original 1995 edition of
the Annotated Ada Reference Manual, and have the same paragraph numbers as in that
edition. Inserted text is indicated
by underlining, and deleted text is
indicated by strikethroughs. @Chg{Version=[2],New=[Some versions also use
color to indicate the version of the change. ],Old=[]}Where paragraphs are
inserted, the paragraph numbers are of the form pp.nn, where pp is the number
of the preceding paragraph, and nn is an insertion number. For instance, the
first paragraph inserted after paragraph 8 is numbered 8.1, the second
paragraph inserted is numbered 8.2, and so on. Deleted paragraphs are indicated
by text such as @i{@shrink{This paragraph was deleted.}} Deleted paragraphs include
empty paragraphs that were numbered in the @Chg{Version=[3],New=[1995 edition of
the],Old=[original]} Ada Reference Manual. Similar markings and numbering
are used for changes to annotations.],Old=[]}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The paragraph number is considered part of the
  paragraph; when a paragraph is moved to a different paragraph number, it is
  marked as changed even if the contents have not changed.]}
@end{Honest}
@end{AARMOnly}
@end{Intro}
@end{NotISO}

