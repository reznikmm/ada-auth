@Part(01, Root="ada.mss")

@LabeledSection{General}
@Comment{$Date: 2022/05/14 04:06:52 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/general.mss,v $}
@Comment{$Revision: 1.1 $}

@begin{Intro}
@Chgref{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0299-1]}
@ChgDeleted{Version=[3],Text=[Ada is a programming language designed to support
the construction of long-lived, highly reliable software systems. The language
includes facilities to define packages
of related types, objects, and operations.
The packages may be parameterized
and the types may be extended to support the construction of libraries
of reusable, adaptable software components. The operations
may be implemented as subprograms using conventional sequential
control structures, or as entries that include synchronization
of concurrent threads of control as part of their invocation.
The language treats modularity in the physical
sense as well, with a facility to support separate compilation.]}

@Chgref{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0269-1],ARef=[AI05-0299-1]}
@ChgDeleted{Version=[3],Text=[The language includes a complete facility for
the support of real-time, concurrent programming.
Errors can be signaled as exceptions and handled explicitly.
The language also
covers systems programming; this requires precise control over the
representation of data and access to system-dependent properties. Finally,
a predefined environment of standard packages is provided, including
facilities for, among others, input-output, string manipulation,
numeric elementary functions, and random
number generation.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgRef{Version=[5],Kind=[Revised]}
This Annotated Ada Reference Manual (AARM) contains the entire text of
the @Chg{Version=[3],New=[third edition of the ],Old=[]}Ada Reference
Manual @Chg{Version=[5],New=[as updated
for Ada 202x (referred to here as the Ada 202x RM)],
Old=[@Chg{Version=[4],New=[as updated by Technical Corrigendum 1],
Old=[]}@Chg{Version=[3],New=[ (the
Ada 2012 RM],Old=[@Chg{Version=[2],New=[with
Amendment 1 (the Ada 2005 RM],Old=[(RM95]}]})]},
plus certain annotations.
The annotations give a more in-depth analysis of the language.
They describe the reason for each nonobvious rule,
and point out interesting ramifications of the rules
and interactions among the rules
(interesting to language lawyers, that is).
Differences between Ada 83@Chg{Version=[2],New=[, Ada 95,
@Chg{Version=[3],New=[],Old=[and ]}Ada 2005@Chg{Version=[3],New=[@Chg{Version=[5],New=[, Ada 2012, and Ada 202x],Old=[, and Ada
2012]}],Old=[]}],Old=[ and Ada 95]} are listed.
(The text you are reading now is an annotation.)

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
The AARM stresses detailed correctness and uniformity over
readability and understandability.
We're not trying to make the language @lquotes@;appear@rquotes@; simple here;
on the contrary, we're trying to expose hidden complexities,
so we can more easily detect language bugs.
The @Chg{Version=[2],New=[Ada @Chg{Version=[3],New=[@Chg{Version=[5],New=[202x],Old=[2012]}],Old=[2005]} RM],Old=[RM95]}, on the other hand, is intended to be a more
readable document for programmers.

@Leading@keepnext@;The annotations in the AARM are as follows:
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
Text that is logically redundant is shown
@Redundant[in square brackets, like this].
Technically, such text could be written as a @NotesName
in the @Chg{Version=[2],New=[Ada @Chg{Version=[3],New=[@Chg{Version=[5],New=[202x],Old=[2012]}],Old=[2005]} RM
(and the Ada 95 @Chg{Version=[3],New=[and 2005 RMs],Old=[RM]} before it)],Old=[RM95]},
since it is really a theorem that can
be proven from the nonredundant rules of the language.
We use the square brackets instead when it seems to make the
@Chg{Version=[2],New=[Ada @Chg{Version=[3],New=[@Chg{Version=[5],New=[202x],Old=[2012]}],Old=[2005]} RM],Old=[RM95]} more readable.

The rules of the language (and some AARM-only text) are categorized,
and placed under certain @i{sub-headings} that indicate
the category.
For example, the distinction between @ResolutionName@;s
and @LegalityName@;s is particularly important,
as explained in @RefSecNum{The Context of Overload Resolution}.

Text under the following sub-headings appears in both documents:
@begin(Inneritemize)
The unlabeled text at the beginning of each clause or subclause,

@SyntaxTitle,

@ResolutionTitle,

@LegalityTitle,

@StaticSemTitle,

@LinkTimeTitle,

@RunTimeTitle,

@BoundedTitle,

@ErronTitle,

@ImplReqTitle,

@DocReqTitle,

@MetricsTitle,

@ImplPermTitle,

@ImplAdviceTitle,

@NotesTitle,

@ExamplesTitle.
@end(Inneritemize)

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
Text under the following sub-headings
does not appear in the @Chg{Version=[2],New=[Ada @Chg{Version=[3],New=[@Chg{Version=[5],New=[202x],Old=[2012]}],Old=[2005]} RM],Old=[RM95]}:
@begin(Inneritemize)
@MetaRulesTitle,

@Inconsistent83Title,

@Incompatible83Title,

@Extend83Title,

@ChgRef{Version=[2],Kind=[Revised]}
@DiffWord83Title@Chg{Version=[2],New=[,],Old=[.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@Inconsistent95Title,]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@Incompatible95Title,]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@Extend95Title,]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}
@ChgAdded{Version=[2],Text=[@DiffWord95Title@Chg{Version=[3],New=[,],Old=[.]}]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@Inconsistent2005Title,]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@Incompatible2005Title,]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@Extend2005Title,]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgRef{Version=[4],Kind=[RevisedAdded]}
@ChgAdded{Version=[3],Text=[@DiffWord2005Title@Chg{Version=[3],New=[,],Old=[.]}]}

@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[@Inconsistent2012Title,]}

@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[@Incompatible2012Title,]}

@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[@Extend2012Title,]}

@ChgRef{Version=[4],Kind=[Added]}
@ChgAdded{Version=[4],Text=[@DiffWord2012Title.]}
@end(Inneritemize)

The AARM also includes the following kinds of annotations.
These do not necessarily annotate the immediately preceding
rule, although they often do.
@end{Itemize}
@end{Discussion}
@begin{Reason}
An explanation of why a certain rule is necessary,
or why it is worded in a certain way.
@end{Reason}
@begin{Ramification}
An obscure ramification of the rules that is of interest
only to language lawyers.
(If a ramification of the rules is of interest to programmers,
then it appears under @NotesTitle.)
@end{Ramification}
@begin{TheProof}
An informal proof explaining how a given
@NotesName or
@Redundant[marked-as-redundant] piece of text
follows from the other rules of the language.
@end{TheProof}
@begin{ImplNote}
A hint about how to implement a feature, or a particular potential
pitfall that an implementer needs to be aware of.

@b{Change:} Change annotations are not used in this version. Changes from
previous versions have been removed. Changes in this version are marked with
versioned paragraph numbers, as explained in the
@lquotes@;Corrigendum Changes@rquotes@; clause of the
@lquotes@;Introduction@rquotes@;.
@end{ImplNote}

@begin{Discussion}
Other annotations not covered by the above.
@end{Discussion}
@begin{Honest}
A rule that is considered logically necessary to the definition of the
language, but which is so obscure or pedantic that only a language
lawyer would care.
These are the only annotations that could be considered part of the
language definition.
@end{Honest}
@begin{GlossaryMarker}
The text of a Glossary entry @em this text will also appear
in @RefSec{Glossary}.
@end{GlossaryMarker}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
In general, @Chg{Version=[2],New=[the Ada @Chg{Version=[3],New=[@Chg{Version=[5],New=[202x],Old=[2012]}],Old=[2005]} RM],Old=[RM95]} text appears in the normal font,
whereas AARM-only text appears in a smaller font.
@NotesName@;s also appear in the smaller font,
as recommended by ISO/IEC style guidelines.
Ada examples are also usually printed in a smaller font.

If you have trouble finding things, be sure to use the index.
@Defn{italics, like this}
Each defined term appears there,
and also in @i{italics, like this}.
Syntactic categories defined in BNF are also indexed.

A definition marked @lquotes@;[distributed]@rquotes@; is the main definition for a
term whose complete definition is given in pieces distributed
throughout the document.
The pieces are marked @lquotes@;[partial]@rquotes@;
or with a phrase explaining what cases the partial definition applies
to.
@end{Discussion}
@end{Intro}

@Comment{Following subclauses are found in a number of separate files,
which ones depend on which version of the documents are being created.}