@Part(syntax, Root="ada.mss")

@Comment{$Date: 2000/05/16 04:48:25 $}
@LabeledInformativeAnnex{Syntax Summary}

@comment{$Source: e:\\cvsroot/ARM/Source/syntax.mss,v $}
@comment{$Revision: 1.10 $}

@begin{Intro}
@Defn2{Term=[syntax], Sec=(complete listing)}
@Defn2{Term=[grammar], Sec=(complete listing)}
@Defn2{Term=[context free grammar], Sec=(complete listing)}
@Defn2{Term=[BNF (Backus-Naur Form)], Sec=(complete listing)}
@Defn2{Term=[Backus-Naur Form (BNF)], Sec=(complete listing)}
This Annex summarizes the complete syntax of the language.
See @RefSecNum{Method of Description and Syntax Notation}
for a description of the notation used.


@end{Intro}

@Case{Device,
    Postscript="@Define{SmallDisplay, use DisplayWithoutParaNum,
        BlankLines=HingeKeep, Size 9, Spacing .9}",
    else="@Define{SmallDisplay, use DisplayWithoutParaNum}"}


@begin{SmallDisplay}
@SyntaxSummary
@end{SmallDisplay}

@NewPage{}
@Heading{Syntax Cross Reference}

@Defn2{Term=[syntax], Sec=(cross reference)}
@Defn2{Term=[grammar], Sec=(cross reference)}
@Defn2{Term=[context free grammar], Sec=(cross reference)}
@Defn2{Term=[BNF (Backus-Naur Form)], Sec=(cross reference)}
@Defn2{Term=[Backus-Naur Form (BNF)], Sec=(cross reference)}

@Comment{The cross reference won't fit in two columns in the .doc file.}
@Case{Device,
    Postscript="@Define{TwoColSyntaxSummary,
        Columns=2, Boxed, NoFill, ColumnMargin .25inch,
        Size 9, Spacing .9, BlankLines Kept}",
    else="@Define{TwoColSyntaxSummary, use Display}"}

@begin{TwoColSyntaxSummary}
@tabclear()
@Case{Device,
    Postscript="@TabSet(.25inches,2.5inches)",
    else="@TabSet(3chars,40chars)"}
@SyntaxXRef
@end{TwoColSyntaxSummary}
