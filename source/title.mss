@Part(title, Root="ada.mss")

@PageHeading(Immediate)
@PageFooting(Immediate)

@Define(I2Title, Copy TitlePage, Sink 8.5in)
@Case{Device,
    Postscript=<@Modify(I2Title, Font HeadingFont, PageBreak Off)>}

@Define(I2TBox, Copy TitleBox, Fixed 3.5inches)

@Define(I2ISO, FlushLeft)
@Case{Device,
    Postscript=<@Modify(I2ISO, Font HeadingFont, Fixed 2inches, Size 6, Spread 2points)>,
    else=<>}

@Define(I2Copyright, FlushLeft)
@Case{Device,
    Postscript=<@Modify(I2Copyright, Font BodyFont, Fixed 10inches)>,
    else=<@Modify(I2Copyright, Fill)>}

@comment{$Source: e:\\cvsroot/ARM/Source/title.mss,v $}
@comment{$Revision: 1.4 $ $Date: 2000/05/17 00:17:45 $}
@begin(I2Title)
@begin{FlushRight, Fixed 0.75inch}
@HelveticaBold{INTERNATIONAL STANDARD} ISO/IEC 8652:@Value{StandardYear}(E)
@end{FlushRight}

@begin{I2ISO}
INTERNATIONAL ORGANIZATION FOR STANDARDIZATION
INTERNATIONAL ELECTROTECHNICAL COMMISSION
@end{I2ISO}

@begin(I2TBox)
@Case{DocumentNickname,
    Chg839X=<>,
    else=<@begin{MajorHeading, FlushLeft}
Information technology @Em Programming languages @Em Ada
@end{MajorHeading}
@begin{FlushLeft}
[Revision of first edition (ISO 8652:1987)]
@end{FlushLeft}>}

@begin{NotISO}
@Heading{@Value{DocumentName}}
@end{NotISO}

Language and Standard Libraries

@begin{NotISO}
Version @Value(Version)
@Value(Date)
@end{NotISO}

@Case{Device,
    File=<@BlankSpace[4lines]
          @I2Copyright[@Value[CopyrightText]]

>,
    else=<@I2Copyright{Copyright @Latin1(169) 1992,1993,1994,1995@:  Intermetrics, Inc.}>}
@end(I2TBox)
@end(I2Title)
@NewPage()
