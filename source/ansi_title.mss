@Part(ansititle, Root="ada.mss")
@Define(I2Title, Copy TitlePage, Sink 8.5in, Font HeadingFont)
@Define(I2TBox, Copy TitleBox, Fixed 2.5inches)
@Define(I2Credit, Copy ResearchCredit, Fixed 9inches)
@Define(I2CRNotice, Copy CopyrightNotice, Fixed 8.5inches,
        AfterExit "Reprinting permitted if accompanied by this statement")
@Define(I2CR, Copy Copyright, Fixed 8.5inches,
        AfterExit "Reprinting permitted if accompanied by this statement")

@comment{$Source: e:\\cvsroot/ARM/Source/ansi_title.mss,v $}
@comment{$Revision: 1.12 $ $Date: 2000/05/27 04:44:03 $}
@begin(I2Title)
@begin{FlushRight, Fixed 0.75inch}
@Case{DocumentNickname,
  RM9X=
[ANSI/ISO/IEC CD 8652
ISO/IEC JTC1/SC22 N 1451
ISO/IEC JTC1/SC22 WG9 N 191],
  ISO=
[ANSI/ISO/IEC CD 8652
ISO/IEC JTC1/SC22 N 1451
ISO/IEC JTC1/SC22 WG9 N 191],
  else=
[ISO/IEC JTC1/SC22 WG9 N 193]}
@end{FlushRight}
@begin(I2TBox)
@String{OfficialPrefix="@i(proposed)
American National Standard
for Information Technology -
Programming Languages"}
@Case{DocumentNickname,
  RM9X=[@Heading{@Value{OfficialPrefix}}],
  ISO=[@Heading{@Value{OfficialPrefix}}],
  else=[@Value{OfficialPrefix}]}
@MajorHeading{Programming Language Ada}
@Comment{We got the above mouthful in a mail message from
Bob Mathis on 8 Sep 1993.}
@Heading{Language and Standard Libraries}

@Case{DocumentNickname,
  RM9X=[@i{Draft}],
  ISO=[@i{Draft}],
  AARM=[@i{Annotated Draft}],
  Syntax9X=[@i{Syntax Only Draft}]}

Version @Value(Version)
@Value(Date)

@Value(IRno)
@end(I2TBox)
Ada 9X Mapping/Revision Team
Intermetrics, Inc.
733 Concord Avenue
Cambridge, Massachusetts 02138
(617) 661-1840




@end(I2Title)

@PageHeading()
@PageFooting()
@comment{case statement: If draft/=2 then it's a public document and
use the reprinting permitted copyright.  Otherwise use the restricted
copyright.}

@Case{Draft, 2="
@PrefaceSection()
@begin(center)
@BlankSpace(4in)
@i(Published by)
Intermetrics, Inc.
733 Concord Avenue
Cambridge, Massachusetts 02138
@BlankSpace(2.5in)
@CopyRightNotice(Intermetrics, Inc.)
@end(Center)

FOR OFFICIAL USE ONLY in performance of Contract F08635-90-C-0066.

This report has been produced under the sponsorship of the Ada 9X
Project Office under contract F08635-90-C-0066.",
else="
@PrefaceSection()
@begin(center)
@BlankSpace(4in)
@i(Published by)
Intermetrics, Inc.
733 Concord Avenue
Cambridge, Massachusetts 02138
@BlankSpace(2.5in)
@CopyRightNotice(Intermetrics, Inc.)
Reprinting permitted if accompanied by this statement.






This work is sponsored by the Ada 9X Project Office under
contract F08635-90-C-0066.
@end(Center)
"}