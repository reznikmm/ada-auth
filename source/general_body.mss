@Part(01, Root="ada.mss")

@Comment{$Date: 2022/09/23 04:34:04 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/general_body.mss,v $}
@Comment{$Revision: 1.5 $}

@Comment{The clause header is found in General.Mss. Other parts are found
in other files, depending upon the version. Note that the subclause labeled
"Extent" has its own file, Extent.Mss.}

@LabeledRevisedSubClauseISOCLause{Version=[0],New=[Structure],Old=[Structure]}

@begin{Intro}
@begin{NotISO}@Comment{The organization differs from ISO and non-ISO versions, so we need different versions of this text}
@Chgref{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @IntlStdName contains thirteen @Chg{Version=[3],New=[clauses],Old=[sections]},
@Chg{Version=[3],New=[fifteen],Old=[fourteen]} annexes,
and an index.
@end{NotISO}
@begin{ISOOnly}
This @IntlStdName contains sixteen clauses,
fourteen annexes, a bibliography, and an index.
@end{ISOOnly}

@begin{Discussion}
@Chgref{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
@ChgAdded{Version=[3],Text=[What Ada 83 called a @ldquote@;chapter@rdquote and
Ada 95 (and Ada 2005) called a @ldquote@;section@rdquote is called a @ldquote@;clause@rdquote
in this @StdTitle. Similarly, what Ada 83 called a @ldquote@;section@rdquote and
Ada 95 (and Ada 2005) called a @ldquote@;clause@rdquote is called a @ldquote@;subclause@rdquote
in this @StdTitle. Confused yet? This terminology is out of our hands; it is
(and was) forced by ever-changing ISO rules for drafting @StdTitle@;s.]}
@end{Discussion}

@begin{NotISO}@Comment{The text differs from ISO and non-ISO versions}
@Leading@Defn{core language}
The @i{core} of the Ada language consists of:
@end{NotISO}
@begin{ISOOnly}
@Leading@Defn{core language}
All implementations shall conform to the @i{core} of the Ada language, which
consists of: 
@end{ISOOnly}
@begin{Itemize}
@begin{NotISO}@Comment{The organization differs from ISO and non-ISO versions, so we need different versions of this text}
@Chgref{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Chg{Version=[3],New=[Clauses],Old=[Sections]} 1 through 13
@end{NotISO}
@begin{ISOOnly}
Clauses 1 through 16
@end{ISOOnly}

@RefSec{Predefined Language Environment}

@RefSec{Interface to Other Languages}

@RefSec{Obsolescent Features}
@end{Itemize}

@ChgTermDef{Version=[5],Kind=(AddedNormal),Group=[C],Term=[core language],
    Def=[a clause or annex in which are defined language constructs or
         capabilities that are provided by all conforming implementations],
    Note1=[A construct is said to be part of the core language
           if it is defined in a core language clause or annex.]}

@begin{NotISO}@Comment{The text differs from ISO and non-ISO versions}
@begin{WideAbove}
@Leading@Defn{Specialized Needs Annexes}
@Defn2{Term=[Annex],Sec=(Specialized Needs)}
@Defn{application areas}
The following @i{Specialized Needs Annexes}
define features that are needed by certain
application areas:
@end{WideAbove}
@end{NotISO}
@begin{ISOOnly}
@begin{WideAbove}
@Leading@Defn{Specialized Needs Annexes}
@Defn2{Term=[Annex],Sec=(Specialized Needs)}
@Defn{application areas}
In addition, an implementation may conform separately to one or more
@i{Specialized Needs Annexes}, which define features that are needed
by certain application areas: 
@end{WideAbove}
@end{ISOOnly}

@begin{Itemize}
@RefSec{Systems Programming}

@RefSec{Real-Time Systems}

@RefSec{Distributed Systems}

@RefSec{Information Systems}

@RefSec{Numerics}

@RefSec{High Integrity Systems}
@end{Itemize}

@ChgTermDef{Version=[5],Kind=(AddedNormal),Group=[C],Term=[specialized needs annex],
    Def=[an annex in which are defined language 
          constructs or capabilities that are not necessarily provided by all
          conforming implementations]}

@begin{WideAbove}
@Leading@Defn{normative}
@Defn2{Term=[Annex],Sec=(normative)}
The core language and the Specialized Needs Annexes are normative,
except that the material in each of the items listed below
is informative:
@end{WideAbove}
@begin(Itemize)
    Text under a NOTES or Examples heading.

@Chgref{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
    Each @Chg{Version=[3],New=[],Old=[clause or ]}subclause whose title starts
    with the word @lquotes@;Example@rquotes@; or @lquotes@;Examples@rquotes@;.
@end(Itemize)

@begin{NotISO}@Comment{This is given above in ISO versions}
All implementations shall conform to the core language.
In addition, an implementation may conform separately to one or more
Specialized Needs Annexes.
@end{NotISO}

@begin{WideAbove}
@Leading@Keepnext@Defn{informative}
@IndexSee{Term=[nonnormative],See=(informative)}
@Defn2{Term=[Annex],Sec=(informative)}
The following Annexes are informative:
@end{WideAbove}
@begin{Itemize}
@RefSec{Language-Defined Aspects and Attributes}

@RefSec{Language-Defined Pragmas}

@Chgref{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
@Chg{Version=[3],New=[@RefSec{Summary of Documentation Requirements}],Old=[@RefSec{Implementation-Defined Characteristics}]}

@begin{NotISO}@Comment{The glossary is now omitted from the ISO version,
and is empty in the RM version. Starting with version 5.}
@RefSec{Glossary}
@end{NotISO}

@RefSec{Syntax Summary}

@Chgref{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[@RefSec{Language-Defined Entities}]}

@end{Itemize}
@begin(Discussion)
The idea of the Specialized Needs Annexes is that implementations
can choose to target certain application areas.
For example, an implementation specifically targeted to embedded
machines might support the application-specific features for
Real-time Systems, but not the application-specific features
for Information Systems.

The Specialized Needs Annexes extend the core language only in ways that
users, implementations, and standards bodies are allowed to extend the
language;
for example, via additional library units,
attributes, representation items (see @RefSecNum{Operational and Representation Aspects}),
@nt{pragma}s,
and constraints on semantic details that are left unspecified by the
core language.
Many implementations already provide much of the functionality defined
by Specialized Needs Annexes;
our goal is to increase uniformity among implementations by defining
standard ways of providing the functionality.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}@ChgNote{Make consistent with 18009}
We recommend that the @Chg{Version=[2],New=[certification],Old=[validation]}
procedures allow implementations to
@Chg{Version=[2],New=[certify],Old=[validate]} the core language, plus any
set of the Specialized Needs Annexes. We recommend that
implementations @i{not} be allowed to @Chg{Version=[2],New=[certify],Old=[validate]}
a portion of one of the Specialized Needs Annexes,
although implementations can, of course, provide @Chg{Version=[2],
New=[uncertified],Old=[unvalidated]} support for such
portions. We have designed the Specialized Needs Annexes assuming that
this recommendation is followed. Thus, our decisions about what to
include and what not to include in those annexes are
based on the assumption that each
annex is @Chg{Version=[2],New=[certified],Old=[validated]} in an
@lquotes@;all-or-nothing@rquotes@; manner.

An implementation may, of course,
support extensions that are different from
(but possibly related to)
those defined by one of the Specialized Needs Annexes.
We recommend that, where appropriate, implementations do this by adding
library units that are children of existing language-defined library
packages.

An implementation should not provide extensions that conflict with
those defined in the Specialized Needs Annexes, in the following sense:
Suppose an
implementation supports a certain error-free program that uses only
functionality defined in the core and in the Specialized Needs Annexes.
The implementation should ensure that that program will still be error
free in some possible full implementation of all of the Specialized Needs
Annexes, and that the semantics of the program will not change.
For example, an implementation should not provide a package
with the same name as one defined in one of the Specialized Needs Annexes,
but that behaves
differently, @i{even if that implementation does not claim
conformance to that Annex}.

Note that the Specialized Needs Annexes do not conflict with each
other; it is the intent that a single implementation can conform
to all of them.
@end(Discussion)

@begin{WideAbove}
@Chgref{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Chgref{Version=[4],Kind=[Revised],ARef=[AI12-0056-1]}
Each @Chg{Version=[4],New=[clause],Old=[section]} is divided into
@Chg{Version=[3],New=[],Old=[clauses and]} subclauses that have a
common structure.
Each @Chg{Version=[3],New=[],Old=[section, ]}clause@Chg{Version=[3],New=[],Old=[,]}
and subclause first introduces its subject.
After the introductory text, text is labeled with the following headings:
@end{WideAbove}
@end{Intro}

@begin{MetaRules}
These are not rules of the language, but guiding principles or goals used in
defining the rules of the language.
In some cases, the goal is only partially met;
such cases are explained.

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
This is not part of the definition of the language,
and does not appear in the @Chg{Version=[2],New=[Ada
@Chg{Version=[3],New=[@Chg{Version=[5],New=[2022],Old=[2012]}],Old=[2005]} RM],Old=[RM95]}.
@end{MetaRules}

@begin{Syntax}
@begin{SyntaxText}
@Defn2{Term=[syntax], Sec=(under Syntax heading)}
@Defn2{Term=[grammar], Sec=(under Syntax heading)}
@Defn2{Term=[context free grammar], Sec=(under Syntax heading)}
@Defn2{Term=[BNF (Backus-Naur Form)], Sec=(under Syntax heading)}
@Defn2{Term=[Backus-Naur Form (BNF)], Sec=(under Syntax heading)}
Syntax rules (indented).
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@Chgref{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Defn{name resolution rules}
@Defn{overloading rules}
@Defn{resolution rules}
Compile-time rules that are used in name resolution,
including overload resolution.
@begin{Discussion}
These rules are observed at compile time.
(We say @lquotes@;observed@rquotes rather than @lquotes@;checked@rquotes,
because these rules are not individually checked.
They are really just part of the @LegalityName@;s in @Chg{Version=[3],New=[Clause],Old=[Section]}
@RefSecNum{Visibility Rules} that require exactly one interpretation of each
constituent of a complete context.) The only rules used in overload resolution
are the @SyntaxName@;s and the @ResolutionName@;s.

When dealing with nonoverloadable declarations it sometimes makes no
semantic difference whether a given rule is a @ResolutionName
or a @LegalityName,
and it is sometimes difficult to decide which it should be.
We generally make a given rule a @ResolutionName only if it has to be.
For example,
@lquotes@;The @nt{name}, if any, in a @nt{raise_statement} shall be the @nt{name}
of an exception.@rquotes@;
is under @lquotes@;@LegalityTitle.@rquotes@;
@end{Discussion}
@end{Resolution}

@begin{Legality}
@Defn{legality rules}
@Defn{compile-time error}
@Defn2{Term=[error], Sec=(compile-time)}
Rules that are enforced at compile time.
@Defn2{Term=[legal], Sec=(construct)}
@Defn2{Term=[illegal], Sec=(construct)}
A construct is @i{legal} if it obeys all of the @LegalityName@;s.
@begin{Discussion}
These rules are not used in overload resolution.

Note that run-time errors are always attached to exceptions;
for example, it is not @lquotes@;illegal@rquotes@; to divide by zero,
it just raises an exception.
@end{Discussion}
@end{Legality}

@begin{StaticSem}
@Defn{static semantics}
@Defn{compile-time semantics}
A definition of the compile-time effect of each construct.
@begin{Discussion}
The most important compile-time effects represent the effects
on the symbol table associated with declarations (implicit or
explicit). In addition, we use this heading as a bit of a grab
bag for equivalences, package specifications, etc.
For example, this is where we put statements like so-and-so is
equivalent to such-and-such. (We ought to try to really mean it when we say
such things!)
Similarly, statements about magically-generated implicit declarations
go here.
These rules are generally written as statements of fact about the
semantics, rather than as a you-shall-do-such-and-such sort of thing.
@end{Discussion}
@end{StaticSem}

@begin{LinkTime}
@Defn{post-compilation error}
@Defn{post-compilation rules}
@IndexSee{Term=[link-time error],See=(post-compilation error)}
@Defn2{Term=[error], Sec=(link-time)}
Rules that are enforced before running a partition.
@Defn2{Term=[legal], Sec=(partition)}
@Defn2{Term=[illegal], Sec=(partition)}
A partition is legal if its compilation units are legal
and it obeys all of the @LinkTimeName@;s.
@begin{Discussion}
It is not specified exactly when these rules are checked, so
long as they are checked for any given partition before that partition starts
running. An implementation may choose to check some such rules at compile
time, and reject @nt{compilation_unit}s accordingly.
Alternatively, an implementation may check such rules when the partition is
created (usually known as @lquotes@;link time@rquotes@;),
or when the partition is mapped to a particular piece of hardware (but before
the partition starts running).
@end{Discussion}
@end{LinkTime}

@begin{RunTime}
@Defn{dynamic semantics}
@Defn{run-time semantics}
@Defn{run-time error}
@Defn2{Term=[error], Sec=(run-time)}
A definition of the run-time effect of each construct.
@begin{Discussion}
This heading describes what happens at run time.
runtime checks,
which raise exceptions upon failure,
are described here.
Each item that involves a runtime check is marked with the name
of the check @em these are the same check names that are used in a
@nt{pragma} Suppress.
Principle: Every check should have a name,
usable in a @nt{pragma} Suppress.
@end{Discussion}
@end{RunTime}

@begin{Bounded}
@Defn{bounded error}
@SeeAlso{Primary=[error], Other=(bounded error)}
Situations that result in bounded (run-time) errors
(see @RefSecNum{Classification of Errors}).
@begin{Discussion}
The @lquotes@;bounds@rquotes@; of each such error are described here @em
that is, we characterize the set of all possible behaviors that can
result from a bounded error occurring at run time.
@end{Discussion}
@end{Bounded}

@begin{Erron}
@Defn{erroneous execution}
@SeeAlso{Primary=[error], Other=(erroneous execution)}
Situations that result in erroneous execution
(see @RefSecNum{Classification of Errors}).
@end{Erron}

@begin{ImplReq}
@Defn{implementation requirements}
Additional requirements for conforming implementations.
@begin{Discussion}
...as opposed to rules imposed on the programmer.
An example might be,
@lquotes@;The smallest representable duration, Duration'Small,
shall not be greater than twenty milliseconds@rquotes.

It's really just an issue of how the rule is worded.
We could write the same rule as @lquotes@;The smallest representable duration is
an implementation-defined value less than or equal to 20 milliseconds@rquotes
and then it would be under @lquotes@;@StaticSemTitle@rquotes.
@end{Discussion}
@end{ImplReq}

@begin{DocReq}
@Defn{documentation requirements}
Documentation requirements for conforming implementations.
@begin{Discussion}
These requirements are beyond those that are implicitly specified by
the phrase @lquotes@;implementation defined@rquotes@;. The
latter require documentation as well, but we don't repeat these cases
under this heading. Usually this heading is used for when the
description of the documentation requirement is longer and does not
correspond directly to one, narrow normative sentence.
@end{Discussion}
@end{DocReq}

@begin{Metrics}
@Defn{metrics}
Metrics that are specified for the time/space properties of the execution
of certain language constructs.
@end{Metrics}

@begin{ImplPerm}
@Defn{implementation permissions}
Additional permissions given to the implementer.
@begin{Discussion}
For example, @lquotes@;The implementation is allowed to impose further
restrictions on the record aggregates allowed in code statements.@rquotes
When there are restrictions on the permission,
those restrictions are given here also.
For example, @lquotes@;An implementation is allowed to restrict the kinds of
subprograms that are allowed to be main subprograms.
However, it shall support at least parameterless procedures.@rquotes
@em we don't split this up between here and @lquotes@ImplReqTitle@rquotes.
@end{Discussion}
@end{ImplPerm}

@begin{ImplAdvice}
@Defn{implementation advice}
@Defn{advice}
Optional advice given to the implementer.
The word @lquotes@;should@rquotes@; is used to indicate that the advice is
a recommendation, not a requirement.
It is implementation defined
whether or not a given recommendation is obeyed.
@ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],
Text=[Whether or not each recommendation
given in @ImplAdviceTitle is followed@Chg{Version=[2],
New=[ @em see @RefSec{Implementation Advice} for a listing],Old=[]}.]}
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Revised]}
The advice generally shows the intended implementation, but
the implementer is free to ignore it.
The implementer is the sole arbiter of whether or not the advice has
been obeyed, if not, whether the reason is a good one,
and whether the required documentation is sufficient.
@Chg{Version=[1],New=[],Old=[@PDefn2{Term=[ACVC],Sec=(Ada Compiler Validation Capability)}
@PDefn2{Term=[Ada Compiler Validation Capability],Sec=(ACVC)}]}
It would be wrong for the @Chg{Version=[1],New=[ACATS],Old=[ACVC]} to enforce any of
this advice.

For example,
@lquotes@;Whenever possible, the implementation should choose a value no
greater than fifty microseconds for the smallest representable duration,
Duration'Small.@rquotes

We use this heading, for example, when the rule is so low level or
implementation-oriented as to be untestable.
We also use this heading when we wish to encourage implementations
to behave in a certain way in most cases, but we do not wish to
burden implementations by requiring the behavior.
@end{Discussion}
@end{ImplAdvice}

@begin{Notes}
@Defn{notes}
Notes emphasize consequences of the rules
described in the (sub)clause or elsewhere.
This material is informative.
@end{Notes}

@begin{Examples}
@NewExample@;Examples illustrate the possible forms of the constructs described.
This material is informative.
@begin{Discussion}
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0386-1]}
@ChgAdded{Version=[5],Text=[Names used in examples refer either to 
language-defined entities or to entities declared in other @ExamplesTitle in 
this @StdTitle. In the latter case, if the reference is to a later example, we
use a cross-reference (@ExamCom{-- See <<some clause>>.}) to show the source
of the declaration. In theory, one could combine all of the examples (excepting
examples of fragments like identifiers or @nt{type_definition}s) in an
appropriate unit and compile them all.]}

@ @* @Comment{Two blank lines: why? Because it was in the original.}
@*
The next three headings list all language changes between Ada 83
and Ada 95. Language changes are any change that changes the set of
text strings that are legal Ada programs, or changes the meaning of
any legal program.
Wording changes, such as changes in terminology, are not language
changes.
Each language change falls into one of the following three
categories:
@end{Discussion}
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
This heading lists all of the upward inconsistencies between Ada 83 and Ada
95. Upward inconsistencies are situations in which a legal Ada 83 program
is a legal Ada 95 program with different semantics.
This type of upward incompatibility is the worst type for users,
so we only tolerate it in rare situations.

(Note that the semantics of a program is not the same thing as the
behavior of the program.
Because of Ada's indeterminacy,
the @lquotes@;semantics@rquotes@; of a given feature describes a @i{set} of behaviors
that can be exhibited by that feature.
The set can contain more than one allowed behavior.
Thus, when we ask whether the semantics changes,
we are asking whether the set of behaviors changes.)

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
This is not part of the definition of the language,
and does not appear in the @Chg{Version=[2],New=[Ada 95@Chg{Version=[3],New=[,],Old=[ or]}
Ada 2005@Chg{Version=[3],New=[@Chg{Version=[5],New=[, Ada 2012,
or Ada 2022],Old=[, or Ada 2012]}],Old=[]} RM],Old=[RM95]}.
@end{Inconsistent83}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
This heading lists all of the upward incompatibilities between Ada 83
and Ada 95, except for the ones listed under @lquotes@;@Inconsistent83Title@rquotes@;
above. These are the situations in which a legal Ada 83 program is
illegal in Ada 95.
We do not generally consider a change that turns erroneous execution
into an exception, or into an illegality, to be upwardly incompatible.

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
This is not part of the definition of the language,
and does not appear in the @Chg{Version=[2],New=[Ada 95@Chg{Version=[3],New=[,],Old=[ or]}
Ada 2005@Chg{Version=[3],New=[@Chg{Version=[5],New=[, Ada 2012, or
Ada 2022],Old=[, or Ada 2012]}],Old=[]} RM],Old=[RM95]}.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
This heading is used to list all upward compatible language changes;
that is, language extensions.
These are the situations in which a legal Ada 95 program is not a
legal Ada 83 program.
The vast majority of language changes fall into this category.

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
This is not part of the definition of the language,
and does not appear in the @Chg{Version=[2],New=[Ada 95@Chg{Version=[3],New=[,],Old=[ or]}
Ada 2005@Chg{Version=[3],New=[@Chg{Version=[5],New=[, Ada 2012, or
Ada 2022],Old=[, or Ada 2012]}],Old=[]} RM],Old=[RM95]}.

@Leading@ @* @Comment{Two blank lines: why? Because it was in the original.}
@*
As explained above,
the next heading does not represent any language change:
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[2],Kind=[Revised]}
@Defn{wording changes from Ada 83}
This heading lists some of the nonsemantic changes between @Chg{Version=[2],
New=[the Ada 83 RM],Old=[RM83]} and
the @Chg{Version=[2],New=[Ada 95 RM],Old=[RM95]}.
It is incomplete; we have not attempted to list all wording
changes, but only the @lquotes@;interesting@rquotes@; ones.

@ChgRef{Version=[2],Kind=[Revised]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
This is not part of the definition of the language,
and does not appear in the @Chg{Version=[2],New=[Ada 95@Chg{Version=[3],New=[,],Old=[ or]}
Ada 2005@Chg{Version=[3],New=[@Chg{Version=[5],New=[, Ada 2012, or
Ada 2022],Old=[, or Ada 2012]}],Old=[]} RM],Old=[RM95]}.
@end{DiffWord83}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@ @* @Comment{Two blank lines: why? Because it was in the Ada 95 original.}
@*
The next three headings list all language changes between Ada 95
and Ada 2005 (the language defined by the Ada 95 standard plus
Technical Corrigendum 1 plus Amendment 1).
Each language change falls into one of the following three
categories:]}
@end{Discussion}

@begin{Inconsistent95}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
This heading lists all of the upward inconsistencies between Ada 95 and Ada
2005. Upward inconsistencies are situations in which a legal Ada 95 program
is a legal Ada 2005 program with different semantics.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@ChgAdded{Version=[2],Text=[Inconsistencies marked with
@b[Corrigendum:]@Defn{Corrigendum} are corrections to the original Ada 95
definition introduced by Corrigendum 1. Inconsistencies marked with
@b[Amendment Correction:]@Defn{Amendment Correction} are corrections to the
original Ada 95 definition added by Amendment 1. Formally, these are
inconsistencies caused by Ada Issues classified as Binding Interpretations;
implementations of Ada 95 are supposed to follow these corrections, not the
original flawed language definition. Thus, these strictly speaking are not
inconsistencies between Ada 95 and Ada 2005. Practically, however, they very
well may be, as early Ada 95 implementations @Chg{Version=[3],New=[might],Old=[may]}
not follow the recommendation.
Inconsistencies so marked are not portable between Ada 95 implementations,
while usually Ada 2005 will have more clearly defined behavior. Therefore, we
document these for completeness.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2005 @Chg{Version=[3],New=[@Chg{Version=[5],New=[,
Ada 2012, or Ada 2022],Old=[or Ada 2012]}],Old=[]} RM.]}
@end{Inconsistent95}

@begin{Incompatible95}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
This heading lists all of the upward incompatibilities between Ada 95
and Ada 2005, except for the ones listed under @lquotes@;@Inconsistent95Title@rquotes@;
above. These are the situations in which a legal Ada 95 program is
illegal in Ada 2005.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@ChgAdded{Version=[2],Text=[As with inconsistencies, incompatibilities
marked with @b[Corrigendum:]
are corrections to the original Ada 95 definition introduced by Corrigendum
1. Incompatibilities marked with @b[Amendment Correction:] are corrections
to the original Ada 95 definition added by Amendment 1. Formally, these are
incompatibilities caused by Ada Issues classified as Binding Interpretations;
implementations of Ada 95 are supposed to follow these corrections, not the
original flawed language definition. Thus,
these strictly speaking are not incompatibilities between Ada 95 and Ada 2005.
Practically, however, they very well may be, as early Ada 95 implementations
@Chg{Version=[3],New=[might],Old=[may]} not follow the recommendation.
Therefore, some Ada 95 implementations
may be able to compile the examples, while others
@Chg{Version=[3],New=[might],Old=[may]} not. In
@Chg{Version=[3],New=[contrast],Old=[constrast]},
Ada 2005 compilers will have consistent behavior. Therefore, we document these
for completeness.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2005 @Chg{Version=[3],New=[@Chg{Version=[5],New=[,
Ada 2012, or Ada 2022],Old=[or Ada 2012]}],Old=[]} RM.]}
@end{Incompatible95}

@begin{Extend95}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@Defn{extensions to Ada 95}
This heading is used to list all upward compatible language changes;
that is, language extensions.
These are the situations in which a legal Ada 2005 program is not a
legal Ada 95 program.
The vast majority of language changes fall into this category.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@ChgAdded{Version=[2],Text=[As with incompatibilities, extensions
marked with @b[Corrigendum:]
are corrections to the original Ada 95 definition introduced by Corrigendum
1. Extensions marked with @b[Amendment Correction:] are corrections
to the original Ada 95 definition added by Amendment 1. Formally, these are
extensions allowed by Ada Issues classified as Binding Interpretations.
As corrections, implementations of Ada 95 are allowed to implement these
extensions. Thus, these strictly speaking are not extensions of Ada 95;
they're part of Ada 95. Practically, however, they very well may be extensions,
as early Ada 95 implementations @Chg{Version=[3],New=[might],Old=[may]} not
implement the extension. Therefore, some Ada 95 implementations may be able
to compile the examples, while others @Chg{Version=[3],New=[might],Old=[may]}
not. In @Chg{Version=[3],New=[contrast],Old=[constrast]},
Ada 2005 compilers will always support the extensions.
Therefore, we document these for completeness.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2005 @Chg{Version=[3],New=[@Chg{Version=[5],New=[,
Ada 2012, or Ada 2022],Old=[or Ada 2012]}],Old=[]} RM.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@ @* @Comment{Two blank lines: why? Because it was in the Ada 95 original.}
@*
As explained above,
the next heading does not represent any language change:]}
@end{Extend95}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@Chg{Version=[2],New=[@Defn{wording changes from Ada 95}
This heading lists some of the nonsemantic changes between the Ada 95 RM and
the Ada 2005 RM. This heading lists only @lquotes@;interesting@rquotes@; changes
(for instance, editorial corrections are not listed). Changes which
come from Technical Corrigendum 1 are marked @b{Corrigendum}; unmarked changes
come from Amendment 1.],Old=[]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[Revised]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[2],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2005 @Chg{Version=[3],New=[@Chg{Version=[5],New=[,
Ada 2012, or Ada 2022],Old=[or Ada 2012]}],Old=[]} RM.]}
@end{DiffWord95}


@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Leading],Text=[@ @* @Comment{Two blank lines: why? Because it was in the Ada 95 original.}
@*
The next three headings list all language changes between Ada 2005
(the language defined by the Ada 95 standard plus
Technical Corrigendum 1 plus Amendment 1)
and Ada 2012 (the language defined by the third edition of the @StdTitle@Comment{was:
Ada 95 standard plus Technical Corrigendum 1 plus Amendment 1 plus Amendment 2}).
Each language change falls into one of the following three categories:]}
@end{Discussion}

@begin{Inconsistent2005}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}
This heading lists all of the upward inconsistencies between Ada 2005 and Ada
2012. Upward inconsistencies are situations in which a legal Ada 2005 program
is a legal Ada 2012 program with different semantics.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Inconsistencies marked with
@b[Correction:]@Defn{Correction} are corrections to the original Ada 2005
definition added by the third edition of the @StdTitle.
Formally, these are
inconsistencies caused by Ada Issues classified as Binding Interpretations;
implementations of Ada 2005 are supposed to follow these corrections, not the
original flawed language definition. Thus, these strictly speaking are not
inconsistencies between Ada 2005 and Ada 2012. Practically, however, they very
well may be, as early Ada 2005 implementations might not follow the recommendation.
Inconsistencies so marked are not portable between Ada 2005 implementations,
while usually Ada 2012 will have more clearly defined behavior. Therefore, we
document these for completeness.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[3],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2012@Chg{Version=[5],New=[ or Ada 2022],Old=[]}
RM.]}
@end{Inconsistent2005}

@begin{Incompatible2005}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}
This heading lists all of the upward incompatibilities between Ada 2005
and Ada 2012, except for the ones listed under @lquotes@;@Inconsistent2005Title@rquotes@;
above. These are the situations in which a legal Ada 2005 program is
illegal in Ada 2012.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[As with inconsistencies, incompatibilities
marked with @b[Correction:] are corrections to the original Ada 2005 definition
added by the third edition. Formally, these are
incompatibilities caused by Ada Issues classified as Binding Interpretations;
implementations of Ada 2005 are supposed to follow these corrections, not the
original flawed language definition. Thus,
these strictly speaking are not incompatibilities between Ada 2005 and Ada 2012.
Practically, however, they very well may be, as early Ada 2005 implementations
might not follow the recommendation. Therefore, some Ada 2005 implementations
may be able to compile the examples, while others might not. In
@Chg{Version=[3],New=[contrast],Old=[constrast]},
Ada 2012 compilers will have consistent behavior. Therefore, we document these
for completeness.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[3],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2012@Chg{Version=[5],New=[ or Ada 2022],Old=[]}
RM.]}
@end{Incompatible2005}

@begin{Extend2005}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
This heading is used to list all upward compatible language changes;
that is, language extensions.
These are the situations in which a legal Ada 2012 program is not a
legal Ada 2005 program.
The vast majority of language changes fall into this category.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[As with incompatibilities, extensions
marked with @b[Correction:]
are corrections to the original Ada 2005 definition added by the third edition.
Formally, these are extensions allowed by Ada Issues classified as Binding
Interpretations. As corrections, implementations of Ada 2005 (and sometimes Ada 95)
are allowed to implement these extensions. Thus, these strictly speaking are not
extensions of Ada 2005; they're part of Ada 2005. Practically, however, they
very well may be extensions, as early Ada 2005 implementations might not implement
the extension. Therefore, some Ada 2005 implementations may be able to compile
the examples, while others might not. In contrast, Ada 2012 compilers will always
support the extensions. Therefore, we document these for completeness.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[3],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2012@Chg{Version=[5],New=[ or Ada 2022],Old=[]}
RM.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Type=[Leading],Text=[@ @* @Comment{Two blank lines: why? Because it was in the Ada 95 original.}
@*
As explained above, the next heading does not represent any language change:]}
@end{Extend2005}

@begin{DiffWord2005}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[@Defn{wording changes from Ada 2005}
This heading lists some of the nonsemantic changes between the Ada 2005 RM and
the Ada 2012 RM. This heading lists only @lquotes@;interesting@rquotes@; changes
(for instance, editorial corrections are not listed). Items marked
@b[Correction:] come from Ada Issues classified
as Binding Interpretations and strictly speaking belong to Ada 2005; other items
only belong to Ada 2012.]}

@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgRef{Version=[4],Kind=[Revised]}
@ChgAdded{Version=[3],Text=[This is not part of the definition of the language,
and does not appear in the Ada 2012@Chg{Version=[5],New=[ or Ada 2022],Old=[]}
RM.]}
@end{DiffWord2005}


@begin{Discussion}
@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Type=[Leading],Text=[@ @* @Comment{Two blank lines: why? Because it was in the Ada 95 original.}
@*
The next three headings list all language changes between
@Chg{Version=[5],New=[Ada 2012
(the language defined by ISO/IEC 8652:2012(E))
and Ada 2022 (the language defined by this edition of the @StdTitle)],Old=[original
Ada 2012 (the language defined by unmodified ISO/IEC 8652:2012(E)) and current
Ada 2012 (the language defined by ISO/IEC 8652:2012(E) as
corrected by the Corrigendum ISO/IEC 8652:2012/Cor 1:2016)]}.
Each language change falls into one of the following three categories:]}
@end{Discussion}

@begin{Inconsistent2012}
@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}
This heading lists all of the upward inconsistencies between
@Chg{Version=[5],New=[Ada 2012 and Ada 2022],Old=[original and current Ada 2012]}.
Upward inconsistencies are situations in which a legal
@Chg{Version=[5],New=[Ada 2012 program is a legal Ada 2022],Old=[
original Ada 2012 program is a legal current Ada 2012]}
program with different semantics.]}

@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Text=[Inconsistencies marked with
@b[Corrigendum:]@Defn{Corrigendum} are corrections to the original Ada 2012
definition added by Technical Corrigendum 1 to Ada 2012.@Chg{Version=[5],
New=[ Inconsistencies marked
with @b[Correction:]@Defn{Correction} are corrections to the original Ada 2012
definition added by this edition of the @StdTitle.],Old=[]}
Formally, these are
inconsistencies caused by Ada Issues classified as Binding Interpretations;
implementations of Ada 2012 are supposed to follow these corrections, not the
original flawed language definition. Thus, these strictly speaking are not
inconsistencies between @Chg{Version=[5],New=[Ada 2012 and Ada 2022],Old=[current
and original Ada 2012]}. Practically, however, they very
well may be, as early Ada 2012 implementations might not follow the recommendation.
Inconsistencies so marked are not portable between Ada 2012
implementations@Chg{Version=[5],New=[, while usually Ada 2022 will have
more clearly defined behavior],Old=[]}. Therefore, we
document these for completeness.]}

@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgAdded{Version=[4],Text=[This is not part of the definition of the language,
and does not appear in any version of the Ada RM.]}
@end{Inconsistent2012}

@begin{Incompatible2012}
@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Text=[@Defn{incompatibilities with Ada 2012}
This heading lists all of the upward incompatibilities between
@Chg{Version=[5],New=[Ada 2012 and Ada 2022],Old=[original and current Ada 2012]},
except for the ones listed under @lquotes@;@Inconsistent2012Title@rquotes@;
above. These are the situations in which a legal
@Chg{Version=[5],New=[Ada 2012 program is illegal in Ada 2022],Old=[
original Ada 2012 program is illegal in current Ada 2012]}.]}


@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Text=[As with inconsistencies, incompatibilities
marked with
@b[Corrigendum:]@Defn{Corrigendum} are corrections to the original Ada 2012
definition added by Corrigendum 1 to Ada 2012@Chg{Version=[5],
New=[, and incompatibilities marked
with @b[Correction:]@Defn{Correction} are corrections to the original Ada 2012
definition added by this edition.],Old=[]}
Formally, these are
incompatibilities caused by Ada Issues classified as Binding Interpretations;
implementations of Ada 2012 are supposed to follow these corrections, not the
original flawed language definition. Thus,
these strictly speaking are not incompatibilities between
@Chg{Version=[5],New=[Ada 2012 and Ada 2022],Old=[current and original Ada 2012]}.
Practically, however, they very well may be, as early Ada 2012 implementations
might not follow the recommendation. Therefore, some Ada 2012 implementations
may be able to compile the examples, while others might
not.@Chg{Version=[5],New=[ In contrast, Ada 2022 compilers will have
consistent behavior.],Old=[]} Therefore, we document these
for completeness.]}

@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgAdded{Version=[4],Text=[This is not part of the definition of the language,
and does not appear in any version of the Ada RM.]}
@end{Incompatible2012}

@begin{Extend2012}
@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Text=[@Defn{extensions to Ada 2012}
This heading is used to list all upward compatible language changes;
that is, language extensions.
These are the situations in which a legal
@Chg{Version=[5],New=[Ada 2022 program is not a legal Ada 2012],Old=[
current Ada 2012 program is not a legal original Ada 2012]} program.
The vast majority of language changes fall into this category.]}

@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Text=[As with incompatibilities, extensions
marked with
@b[Corrigendum:]@Defn{Corrigendum} are corrections to the original Ada 2012
definition added by Corrigendum 1 to Ada 2012@Chg{Version=[5],
New=[, and extensions
marked with @b[Correction:] are corrections to the original Ada 2012 definition
added by this edition],Old=[]}.
Formally, these are extensions allowed by Ada Issues classified as Binding
Interpretations. As corrections, implementations of Ada 2012 (and sometimes Ada 95)
are allowed to implement these extensions. Thus, these strictly speaking are not
extensions of Ada 2012; they're part of Ada 2012. Practically, however, they
very well may be extensions, as early Ada 2012 implementations might not implement
the extension. Therefore, some Ada 2012 implementations may be able to compile
the examples, while others might not.@Chg{Version=[5],New=[ In contrast,
Ada 2022 compilers will always support the extensions.],Old=[]}
Therefore, we document these for completeness.]}

@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgAdded{Version=[4],Text=[This is not part of the definition of the language,
and does not appear in any version of the Ada RM.]}

@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgAdded{Version=[4],Type=[Leading],Text=[@ @* @Comment{Two blank lines: why? Because it was in the Ada 95 original.}
@*
As explained above, the next heading does not represent any language change:]}
@end{Extend2012}

@begin{DiffWord2012}
@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgRef{Version=[5],Kind=[Revised]}
@ChgAdded{Version=[4],Text=[@Defn{wording changes from Ada 2012}
This heading lists some of the nonsemantic changes between the
@Chg{Version=[5],New=[Ada 2012 RM and
the Ada 2022 RM],Old=[original and corrected Ada 2012 RM]}. This heading lists
only @lquotes@;interesting@rquotes@; changes
(for instance, editorial corrections are not listed). Items marked
@b[Corrigendum:]@Defn{Corrigendum} are included in Corrigendum 1 to Ada
2012@Chg{Version=[5],New=[, and items marked with @b[Correction:] are added by this edition.
In both cases, the],Old=[. These]}
items come from Ada Issues classified
as Binding Interpretations and strictly speaking belong to Ada
2012@Chg{Version=[5],New=[; other items only belong to Ada 2022],Old=[]}.]}

@ChgRef{Version=[4],Kind=[AddedNormal]}
@ChgAdded{Version=[4],Text=[This is not part of the definition of the language,
and does not appear in any version of the Ada RM.]}
@end{DiffWord2012}


@LabeledRevisedSubClauseISOCLause{Version=[5],New=[Conformity of an Implementation],Old=[Conformity of an Implementation with the Standard]}

@begin{ImplReq}

@Leading@Keepnext@Defn2{Term=[conformance],Sec=(of an implementation)}
A conforming implementation shall:
@begin{Discussion}
@Defn{implementation}
The @i{implementation} is the software and hardware that implements
the language.
This includes compiler, linker, operating system, hardware, etc.

We first define what it means to @lquotes@;conform@rquotes@; in general @em
basically, the implementation has to properly implement the normative
rules given throughout the @StdTitle.
Then we define what it means to conform to a Specialized Needs
Annex @em the implementation must support the core features plus the
features of that Annex.
Finally, we define what it means to @lquotes@;conform to the @StdTitle@rquotes@; @em
this requires support for the core language,
and allows partial (but not conflicting) support for
the Specialized Needs Annexes.
@end{Discussion}
@begin(Itemize)
     Translate and correctly execute
     legal programs written in Ada,
     provided that they are not so large as to exceed the capacity of
     the implementation;

     Identify all programs or program units
     that are so large as to exceed the capacity of
     the implementation (or raise an appropriate
     exception at run time);
     @ImplDef{Capacity limitations of the implementation.}

     Identify all programs or program units that contain
     errors whose detection is required by this
     @IntlStdTitle;
     @begin{Discussion}
       Note that we no longer use the term @lquotes@;rejection@rquotes@; of
       programs or program units.
       We require that programs or program units with errors or that
       exceed some capacity limit be @lquotes@;identified@rquotes@;.
       The way in which errors or capacity problems are reported is not
       specified.

       An implementation is allowed to use standard error-recovery
       techniques.
       We do not disallow such techniques from being used across
       @nt{compilation_unit} or @nt{compilation} boundaries.

       See also the @ImplReqTitle of @RefSecNum{Program Execution},
       which disallow the execution of illegal partitions.

     @end{Discussion}

     Supply all language-defined library units required by this
     @IntlStdTitle;
     @begin{ImplNote}
       An implementation cannot add to or modify the visible part of
       a language-defined library unit,
       except where such permission is explicitly granted,
       unless such modifications are semantically neutral with respect
       to the client compilation units of the library unit.
       An implementation defines the contents of the private part and
       body of language-defined library units.

       An implementation can add @nt{with_clause}s and @nt{use_clause}s,
       since these modifications are semantically neutral to clients.
       (The implementation might need @nt{with_clause}s in order to
       implement the private part, for example.)
       Similarly, an implementation can add a private part even in cases
       where a private part is not shown in the @StdTitle.
       Explicit declarations can be provided implicitly or by renaming,
       provided the changes are semantically neutral.

       @Defn2{Term=[italics],Sec=(implementation-defined)}
       Wherever in the @StdTitle the text of a language-defined library
       unit contains an italicized phrase starting with
       @lquotes@;@i{implementation-defined}@rquotes@;, the implementation's version
       will replace that phrase with some implementation-defined text
       that is syntactically legal at that place, and follows any other
       applicable rules.

       Note that modifications are permitted, even if there are other tools
       in the environment that can detect the changes (such as a program
       library browser), so long as the modifications make no difference
       with respect to the static or dynamic semantics of the resulting
       programs, as defined by the @StdTitle.
     @end{ImplNote}

     Contain no variations except
     those explicitly permitted by this
     @IntlStdTitle, or those that are impossible or impractical
     to avoid given the implementation's execution environment;
     @ImplDef{Variations from the standard that are impractical to avoid
     given the implementation's execution environment.}
     @begin{Reason}
     The @lquotes@;impossible or impractical@rquotes@; wording comes from AI-325.
     It takes some judgement and common sense to interpret this.
     Restricting compilation units to less than 4 lines is probably
     unreasonable, whereas restricting them to less than 4 billion lines
     is probably reasonable (at least given today's technology).
     We do not know exactly where to draw the line,
     so we have to make the rule vague.
     @end{Reason}

     Specify all such variations in the manner prescribed
     by this @IntlStdTitle.
@end(Itemize)

@begin{WideAbove}
@Leading@keepnext@Defn2{Term=[external effect], Sec=(of the execution of an Ada program)}
@Defn2{Term=[effect], Sec=(external)}
The @i(external effect) of the execution of an Ada program is
defined in terms of its interactions
with its external environment.
@Defn{external interaction}
The following are defined as @i(external interactions):
@end{WideAbove}
@begin(Itemize)
  Any interaction with an external file
  (see @RefSecNum(External Files and File Objects));

  The execution of
  certain @nt<code_statement>s
  (see @RefSecNum{Machine Code Insertions});
  which @nt{code_statement}s cause external interactions
  is implementation defined.
  @ImplDef{Which @nt{code_statement}s cause external interactions.}

  Any call on an imported subprogram
  (see @RefSecNum(Interface to Other Languages)),
  including any parameters passed to it;

  Any result returned or exception
  propagated from a main subprogram
  (see @RefSecNum(Program Execution))
  or an exported subprogram
  (see @RefSecNum(Interface to Other Languages)) to an external caller;
  @begin{Discussion}
    By @lquotes@;result returned@rquotes@; we mean to include function results
    and values returned in [@key(in)] @key(out) parameters.

    @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0094],ARef=[AI95-00119-01]}
    @Chg{Version=[1],New=[The lack of a result from a program that does not terminate
    is also included here.],Old=[]}
  @end{Discussion}

  @Redundant[Any read or update of an atomic or volatile object
  (see @RefSecNum(Shared Variable Control));]

  The values of imported and exported objects
  (see @RefSecNum(Interface to Other Languages)) at the time
  of any other interaction with the external environment.
    @begin{Honest}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
    Also other uses of imported and exported entities,
    as defined by the implementation,
    if the implementation supports such @Chg{Version=[3],New=[importing or
    exporting],Old=[@nt{pragma}s]}.
    @end{Honest}
@end(Itemize)

@begin{WideAbove}
A conforming implementation
of this @IntlStdTitle shall produce for the
execution of a given Ada program
a set of interactions with the external environment whose
order and timing are consistent with the definitions and requirements of this
@IntlStdTitle for the semantics of the given program.
@end{WideAbove}
@begin{Ramification}
  There is no need to produce any of the @lquotes@;internal effects@rquotes@;
  defined for the semantics of the program @em all of these
  can be optimized away @em so long as an appropriate sequence
  of external interactions is produced.
@end{Ramification}
@begin{Discussion}
  See also @RefSecNum(Exceptions and Optimization) which specifies
  various liberties associated with optimizations in
  the presence of language-defined checks,
  that could change the external effects that
  might be produced. These alternative external effects
  are still consistent with the standard, since
  @RefSecNum(Exceptions and Optimization) is part of the standard.

  Note also that we only require @lquotes@i(an appropriate) sequence
  of external interactions@rquotes rather than @lquotes@;@i(the same) sequence...@rquotes
  An optimizer may cause a different sequence of external interactions
  to be produced than would be produced without the optimizer, so
  long as the new sequence still satisfies the requirements
  of the standard. For example, optimization might affect
  the relative rate of progress of two concurrent tasks, thereby
  altering the order in which two external interactions occur.

@ChgRef{Version=[2],Kind=[Revised]}
  Note that @Chg{Version=[2],New=[the Ada 83 RM],Old=[RM83]} explicitly
  mentions the case of an @lquotes@;exact effect@rquotes@;
  of a program, but since so few programs have their effects defined
  that exactly,
  we don't even mention this @lquotes@;special@rquotes@; case. In particular,
  almost any program that uses floating point or tasking has to have
  some level
  of inexactness in the specification of its effects. And if one
  includes aspects of the timing of the external interactions
  in the external effect of the program (as is appropriate for a real-time
  language), no @lquotes@;exact effect@rquotes@; can be specified.
  For example, if two external interactions initiated by a single task
  are separated by a @lquotes@;@key(delay) 1.0;@rquotes@; then the language rules
  imply that the two external interactions have to be separated in time
  by at least one second, as defined by the clock associated with
  the @nt<delay_relative_statement>. This in turn implies that
  the time at which an external interaction occurs is part of
  the characterization of the external interaction, at least in
  some cases, again making the specification of the required
  @lquotes@;exact effect@rquotes@; impractical.
@end{Discussion}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0441-1]}
An implementation that conforms to this @StdTitle shall support each
capability required by the core language as specified.
In addition,
an implementation that conforms to this @StdTitle may conform to one
or more Specialized Needs Annexes (or to none).
Conformance to a Specialized Needs Annex means that each capability
required by the Annex @Chg{Version=[5],New=[shall be],Old=[is]} provided as
specified.
@begin{Discussion}
  The last sentence defines what it means to say that an
  implementation conforms to a Specialized Needs Annex, namely, only
  by supporting all capabilities required by the Annex.
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
An implementation conforming to this @IntlStdTitle may provide additional
@Chg{Version=[3],New=[aspects, ],Old=[]}attributes, library units, and pragmas.
However, it shall not provide any
@Chg{Version=[3],New=[aspect, ],Old=[]}attribute,
library unit, or pragma having the same name
as an @Chg{Version=[3],New=[aspect, ],Old=[]}attribute, library unit,
or pragma (respectively)
specified in a Specialized Needs Annex unless the provided construct
is either as specified in the Specialized Needs Annex or is more
limited in capability than that required by the Annex.
A program that attempts to use an unsupported capability of an Annex
shall either be identified by the implementation before run time or
shall raise an exception at run time.
@begin{Discussion}
  The last sentence of the preceding paragraph defines what an
  implementation is allowed to do when it does not "conform" to a
  Specialized Needs Annex.
  In particular, the sentence forbids implementations from providing
  a construct with the same name as a corresponding construct in a
  Specialized Needs Annex but with a different syntax
  (e.g., an extended syntax) or quite different semantics.
  The phrase concerning "more limited in capability" is intended to
  give permission to provide a partial implementation, such as not
  implementing a subprogram in a package or having a restriction not
  permitted by an implementation that conforms to the Annex.
  For example, a partial implementation of the package Ada.Decimal
  might have Decimal.Max_Decimal_Digits as 15 (rather than the
  required 18).
  This allows a partial implementation to grow to a fully conforming
  implementation.

  A restricted implementation might be restricted by not providing
  some subprograms specified in one of the packages defined by an
  Annex.
  In this case, a program that tries to use the missing subprogram
  will usually fail to compile.
  Alternatively, the implementation might declare the subprogram as
  abstract, so it cannot be called.
  Alternatively, a subprogram body might be implemented just to raise
  Program_Error.
  The advantage of this approach is that a program to be run under a
  fully conforming Annex implementation can be checked syntactically
  and semantically under an implementation that only partially
  supports the Annex.
  Finally, an implementation might provide a package declaration
  without the corresponding body, so that programs can be compiled,
  but partitions cannot be built and executed.

  To ensure against wrong answers being delivered by a partial
  implementation, implementers are required to raise an exception
  when a program attempts to use an unsupported capability and this
  can be detected only at run time.
  For example, a partial implementation of Ada.Decimal might require
  the length of the Currency string to be 1, and hence, an
  exception would be raised if a subprogram were called in the
  package Edited_Output with a length greater than 1.
@end{Discussion}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0179-1],ARef=[AI12-0265-1]}
@ChgAdded{Version=[5],Text=[For an implementation that conforms to this
@StdTitle, the implementation of a language-defined unit shall abide by all
postconditions, type invariants, and default initial conditions specified
for the unit by this @IntlStdName (see
@RefSecNum{Pragmas Assert and Assertion_Policy}).]}
@end{ImplReq}

@begin{DocReq}
@Defn{implementation defined}
@Defn{unspecified}
@Defn{specified (not!)}
@IndexSee{Term=[implementation-dependent],See=(unspecified)}
@Defn{documentation (required of an implementation)}
Certain aspects of the semantics are defined to be either
@i{implementation defined} or @i{unspecified}.
In such cases, the set of possible effects is specified, and
the implementation may choose any effect in the set.
Implementations shall document their behavior in
implementation-defined situations, but documentation is not required
for unspecified situations.
The implementation-defined characteristics are summarized in
@RefSecNum{Implementation-Defined Characteristics}.
@begin{Discussion}
  We used to use the term @lquotes@;implementation dependent@rquotes
  instead of @lquotes@;unspecified@rquotes.
  However, that sounded too much like @lquotes@;implementation defined@rquotes.
  Furthermore, the term @lquotes@;unspecified@rquotes is used in the ANSI C and
  POSIX standards for this purpose, so that is another advantage.
  We also use @lquotes@;not specified@rquotes and @lquotes@;not specified by the language@rquotes@;
  as synonyms for @lquotes@;unspecified@rquotes.
  The documentation requirement is the only difference between
  implementation defined and unspecified.

  Note that the @lquotes@;set of possible effects@rquotes@; can be @lquotes@;all imaginable
  effects@rquotes@;, as is the case with erroneous execution.
@end{Discussion}

The implementation may choose to document implementation-defined behavior
either by documenting what happens in general,
or by providing some mechanism for the user to determine what
happens in a particular case.
@begin(Discussion)
For example, if the @StdTitle says that library unit elaboration order
is implementation defined,
the implementation might describe (in its user's manual)
the algorithm it uses to determine the elaboration order.
On the other hand, the implementation might provide a
command that produces
a description of the elaboration order for a partition upon request
from the user.
It is also acceptable to provide cross references to existing
documentation (for example, a hardware manual), where appropriate.

Note that dependence of a program on implementation-defined or
unspecified functionality is not defined to be an error;
it might cause the program to be less portable, however.
@end(Discussion)
@ChgDocReq{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],Text=[
The behavior of implementations in implementation-defined situations
shall be documented @em see @RefSec{Implementation-Defined Characteristics}
for a listing.]}]}
@end{DocReq}

@begin{ImplAdvice}
If an implementation detects the use of an unsupported
Specialized Needs Annex feature at run time,
it should raise Program_Error if feasible.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Program_Error should be raised when an unsupported
Specialized Needs Annex feature is used at run time.]}]}
@begin{Reason}
The reason we don't @i{require} Program_Error is that there are
situations where other exceptions might make sense.
For example, if the Real Time Systems Annex requires
that the range of System.Priority include at least 30 values,
an implementation could conform to the @StdTitle (but not to the Annex)
if it supported only 12 values.
Since the rules of the language require Constraint_Error to be raised
for out-of-range values,
we cannot require Program_Error to be raised instead.
@end{Reason}

If an implementation wishes to provide implementation-defined
extensions to the functionality of a language-defined library unit,
it should normally do so by adding children to the library unit.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Implementation-defined extensions to the functionality of a
language-defined library unit should be provided by adding children
to the library unit.]}]}
@begin(ImplNote)
If an implementation has support code
(@lquotes@;run-time system code@rquotes@;)
that is needed for the execution of user-defined code,
it can put that support code in child packages of System.
Otherwise, it has to use some trick to avoid polluting the user's
namespace.
It is important that such tricks not be available
to user-defined code
(not in the standard mode, at least)
@em that would defeat the purpose.
@end(ImplNote)
@end{ImplAdvice}

@begin{SingleNote}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0440-1]}
The above requirements imply that an implementation conforming
to this @StdTitle @Chg{Version=[5],New=[can],Old=[may]} support some of the
capabilities required by a
Specialized Needs Annex without supporting all required
capabilities.
@begin{Discussion}
  A conforming implementation can partially support a
  Specialized Needs Annex.
  Such an implementation does not conform to the Annex,
  but it does conform to the @StdTitle.
@end{Discussion}
@end{SingleNote}


@LabeledRevisedSubClauseISOCLause{Version=[0],New=[Method of Description and Syntax Notation],Old=[Method of Description and Syntax Notation]}

@begin{Intro}
The form of an Ada program is described by means of a context-free
syntax together with context-dependent requirements expressed by
narrative rules.

The meaning of Ada programs is described by means of narrative
rules defining both the effects of each construct and the composition
rules for constructs.

@Leading@keepnext@;@Defn2{Term=[syntax], Sec=(notation)}
@Defn2{Term=[grammar], Sec=(notation)}
@Defn2{Term=[context free grammar], Sec=(notation)}
@Defn2{Term=[BNF (Backus-Naur Form)], Sec=(notation)}
@Defn2{Term=[Backus-Naur Form (BNF)], Sec=(notation)}
The context-free syntax of the language is described using a simple variant
of Backus-Naur Form. In particular:
@begin(Itemize)
@begin{NotISO}
@leading@keepnext@;Lower case words in a sans-serif font,
some containing embedded underlines, are used to
denote syntactic categories, for example:
@end{NotISO}
@begin{ISOOnly}
@leading@keepnext@;Lower case words in a fixed-width font,
some containing embedded underlines, are used to
denote syntactic categories, for example:
@end{ISOOnly}
@begin(Display)
@nt<case_statement>
@end(Display)

@leading@keepnext@;Boldface words are used to denote reserved words,
for example:
@begin(Display)
@key(array)
@end(Display)

@leading@keepnext@;Square brackets enclose optional items. Thus the two
following rules are equivalent.
@begin(Display)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}@Chg{Version=[2],New=[@nt<simple_return_statement>],Old=[@nt<return_statement>]} ::= @key(return) [@nt<expression>];
@Chg{Version=[2],New=[@nt<simple_return_statement>],Old=[@nt<return_statement>]} ::= @key(return); | @key(return) @nt<expression>;
@end(Display)

@leading@keepnext@;Curly brackets enclose a repeated item. The item may
appear zero or more times; the repetitions occur from left to right as with an
equivalent left-recursive rule. Thus the two following rules are
equivalent.
@begin(Display)
@nt<term> ::= @nt<factor> {@nt<multiplying_operator> @nt<factor>}
@nt<term> ::= @nt<factor> | @nt<term> @nt<multiplying_operator> @nt<factor>
@end(Display)

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0212-1]}
@leading@keepnext@;A vertical line separates
alternative items@Chg{Version=[5],New=[, for example],Old=[ unless it occurs
immediately after an opening curly bracket,
in which case it stands for itself]}:
@begin(Display)
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0212-1]}@nt<constraint> ::= @nt<scalar_constraint> | @nt<composite_constraint>@Chg{Version=[5],New=[],Old=[
@nt<discrete_choice_list> ::= @nt<discrete_choice> {| @nt<discrete_choice>}]}
@end(Display)

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0212-1]}
@ChgAdded{Version=[5],Type=[Leading],Keepnext=[T],Text=[For symbols used in this
notation (square brackets, curly brackets, and the vertical line), the symbols
when surrounded by ' represent themselves, for example:]}
@begin{NotISO}
@begin(Display)
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0212-1]}@ChgAdded{Version=[5],Text=<@nt<discrete_choice_list> ::= @nt<discrete_choice> {'|' @nt<discrete_choice>}
@nt<named_container_aggregate> ::= '[' @nt<container_element_association_list> ']'>}
@end(Display)
@end{NotISO}
@begin{ISOOnly}
@begin(Display)
@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0212-1]}@ChgAdded{Version=[5],Text=<@nt<discrete_choice_list> ::= @nt<discrete_choice> {'|' @nt<discrete_choice>}
@nt<named_container_aggregate> ::=
    '[' @nt<container_element_association_list> ']'>}
@end(Display)
@end{ISOOnly}

@Defn2{Term=[italics],Sec=(syntax rules)}
If the name of any syntactic category starts with an italicized
part, it is equivalent to the category name without the italicized
part. The italicized part is intended to convey some semantic
information. For example @i(subtype_)@nt<name> and
@i(task_)@nt<name> are both equivalent to @nt<name> alone.
@end(Itemize)
@begin(Discussion)
@Defn{LR(1)}
@Defn{ambiguous grammar}
@Defn2{Term=[grammar],Sec=(resolution of ambiguity)}
@Defn2{Term=[grammar],Sec=(ambiguous)}
The grammar given in @Chg{Version=[2],New=[this @IntlStdName],
old=[the RM95]} is not LR(1).
In fact, it is ambiguous; the ambiguities are resolved
by the overload resolution rules
(see @RefSecNum{The Context of Overload Resolution}).

We often use @lquotes@;if@rquotes to mean @lquotes@;if and only if@rquotes in definitions.
For example, if we define @lquotes@;photogenic@rquotes by saying,
@lquotes@;A type is photogenic if it has the following properties...@rquotes,
we mean that a type is photogenic if @i{and only if}
it has those properties.
It is usually clear from the context,
and adding the @lquotes@;and only if@rquotes seems too cumbersome.

When we say, for example, @lquotes@;a @nt{declarative_item} of a
@nt{declarative_part}@rquotes, we are talking about a @nt{declarative_item}
immediately within that @nt{declarative_part}. When we say @lquotes@;a
@nt{declarative_item} in, or within, a @nt{declarative_part}@rquotes@;, we are
talking about a @nt{declarative_item} anywhere in the
@nt{declarative_part}, possibly deeply nested within other
@nt{declarative_part}s. (This notation doesn't work very well for
@nt{name}s, since the name @lquotes@;of@rquotes@; something also has another meaning.)

When we refer to the name of a language-defined
entity (for example, Duration),
we mean the language-defined entity even in programs where the declaration
of the language-defined entity is hidden by another declaration.
For example, when we say that the expected type for the @nt<expression>
of a @nt<delay_relative_statement> is Duration, we mean the language-defined
type Duration that is declared in Standard, not some type
Duration the user might have declared.
@end(Discussion)

@begin{WideAbove}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0004-1],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The delimiters, compound delimiters, reserved words, and
@nt{numeric_literal}s are exclusively made of the
characters whose code @Chg{Version=[3],New=[point],Old=[position]} is
between 16#20# and 16#7E#, inclusively.
The special characters for which names are defined in this
@IntlStdName (see @RefSecNum{Character Set}) belong to the same range.
@Redundant[For example, the character E in the definition of @Chg{Version=[3],
New=[@nt{exponent}],Old=[exponent]} is the
character whose name is @lquotes@;LATIN CAPITAL LETTER E@rquotes@;, not
@lquotes@;GREEK CAPITAL LETTER EPSILON@rquotes@;.]]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This just means that programs can be written in plain
ASCII characters; no characters outside of the 7-bit range are required.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00395-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0227-1],ARef=[AI05-0299-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0263-1]}
@ChgAdded{Version=[2],Text=[When this @IntlStdName mentions the
conversion of some character or sequence of characters to upper case, it means
the character or sequence of characters obtained by using
@Chg{Version=[3],New=[simple upper case mapping],Old=[locale-independent
full case folding]}, as defined by documents referenced in
@Chg{Version=[5],New=[],Old=[the note
in ]}@Chg{Version=[3],New=[Clause],Old=[section]} @Chg{Version=[5],New=[2],Old=[1]}
of ISO/IEC 10646:@Chg{Version=[3],New=[@Chg{Version=[5],New=[2017],Old=[2011]}],Old=[2003]}.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[],Old=[Unless otherwise
  specified for sequences of characters, case folding is applied to the
  sequence, not to individual characters. It sometimes can make a
  difference.]}]}
@end{Discussion}
@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0263-1]}
  @ChgAdded{Version=[5],Text=[The @ldquote@;documents
  referenced@rdquote means Unicode, Chapter 4 (specifically, section
  4.2 @em Case). Machine-readable versions of Simple Uppercase Mapping
  and Simple Lowercase Mapping can be found in
  @URLLink{URL=[http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt],
  Text=[http://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt]}.
  Data for older Unicode versions can be found on this site as
  well; start at @URLLink{URL=[http://www.unicode.org/Public/],
  Text=[http://www.unicode.org/Public/]} and find the appropriate version
  number. Simple Uppercase Mapping is the 12th field in this file (the 13th
  element of each line, since Unicode counts from 0); the Simple Lowercase
  Mapping is the 13th field in this file. In both cases, if no character
  is present in the field, the character maps to itself.]}
@end{ImplNote}
@end{WideAbove}

@begin{WideAbove}
@Defn{syntactic category}
A @i{syntactic category} is
a nonterminal in the grammar defined in BNF under @lquotes@;@SyntaxTitle@rquotes.
Names of syntactic categories are set in a different font,
@ntf{like_this}.
@end{WideAbove}

@ToGlossaryAlso{Term=<Construct>,
  Text=<A @i(construct) is a piece of text
  (explicit or implicit) that is an instance of a syntactic category
  defined under @lquotes@;@SyntaxTitle@rquotes@;.>}
@ChgTermDef{Version=[5],Kind=(AddedNormal),Group=[C],Term=[construct],
        Def=[a piece of text (explicit or implicit) that is an instance
          of a syntactic category defined under @SyntaxTitle]}
@begin{Ramification}
For example, an @nt{expression} is a construct.
A declaration is a construct,
whereas the thing declared by a declaration is an @lquotes@;entity@rquotes.
@end{Ramification}
@begin{Discussion}
@lquotes@;Explicit@rquotes@; and @lquotes@;implicit@rquotes@; don't mean exactly what you might think
they mean: The text of an instance of a generic is
considered explicit, even though it does not appear explicitly (in
the nontechnical sense) in the program text,
and even though its meaning is not defined entirely in terms of that
text.
@end{Discussion}

@Defn2{term=<constituent>, Sec=<of a construct>}
A @i{constituent} of a construct is the construct itself,
or any construct appearing within it.

@Defn{arbitrary order}
Whenever the run-time semantics
defines certain actions to happen in an @i{arbitrary order},
this means that the implementation shall arrange for these actions
to occur in a way that is equivalent to some sequential order,
following the rules that result from that sequential order.
When evaluations are defined to happen in an arbitrary order,
with conversion of the results to some subtypes,
or with some runtime checks,
the evaluations, conversions, and checks may be arbitrarily
interspersed, so long as each expression is evaluated before converting
or checking its value.
@PDefn2{Term=[type conversion],Sec=(arbitrary order)}
@PDefn2{Term=[conversion],Sec=(arbitrary order)}
@Redundant[Note that the effect of a program can depend on the
order chosen by the implementation.
This can happen, for example,
if two actual parameters of a given call have side effects.]
@begin{Discussion}
Programs will be more portable if their external effect does not
depend on the particular order chosen by an implementation.
@end{Discussion}
@begin{Ramification}
Additional reordering permissions are given in
@RefSec(Exceptions and Optimization).

There is no requirement that the implementation always choose
the same order in a given kind of situation. In fact, the
implementation is allowed to choose a different order for two
different executions of the same construct.
However, we expect most implementations will behave in a relatively
predictable manner in most situations.
@end{Ramification}
@begin{Reason}
The @lquotes@;sequential order@rquotes@; wording is intended to allow the programmer
to rely on @lquotes@;benign@rquotes@; side effects.
For example, if F is a function that returns a unique integer by
incrementing some global and returning the result,
a call such as P(F, F) is OK if the programmer cares only
that the two results of F are unique;
the two calls of F cannot be executed in parallel,
unless the compiler can prove that parallel execution is
equivalent to some sequential order.
@end{Reason}
@end{Intro}

@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}

@begin{Notes}
The syntax rules describing structured constructs are presented in a
form that corresponds to the recommended paragraphing. For example, an
@nt{if_statement} is defined as:
@begin(Example)
@nt<if_statement> ::=
    @key(if) @nt<condition> @key(then)
      @nt<sequence_of_statements>
   {@key(elsif) @nt<condition> @key(then)
      @nt<sequence_of_statements>}
   [@key(else)
      @nt<sequence_of_statements>]
    @key(end if);
@end(Example)

The line breaks and indentation in the syntax rules indicate the
recommended line breaks and indentation in the corresponding constructs.
The preferred places for other line breaks are after semicolons.
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[We now explicitly say that the lexical elements
  of the language (with a few exceptions) are made up of characters in the
  lower half of the Latin-1 character set. This is needed to avoid confusion
  given the new capability to use most ISO 10646 characters in identifiers and
  strings.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[We now explicitly define what the @StdTitle means
  by upper case, as there are many possibilities for ISO 10646 characters.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
  @ChgAdded{Version=[2],Text=[The example for square brackets has been changed
  as there is no longer a @ntf{return_statement} syntax rule.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0227-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Upper case is defined by
  "simple upper case mapping", because "full case folding" is a mapping
  (mostly) to lower case.]}
@end{DiffWord2005}

@begin{DiffWord2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0212-1]}
  @ChgAdded{Version=[5],Text=[Changed the syntax description to quote
  symbols used in the notation when they appear as themselves. We made this
  change to allow square brackets in Ada syntax, but the change also allowed
  us to get rid of an old hack which allowed vertical lines to stand for
  themselves in limited circumstances.]}
@end{DiffWord2012}


@LabeledRevisedSubClauseISOCLause{Version=[0],New=[Classification of Errors],Old=[Classification of Errors]}

@begin{Metarules}
   @Chgref{Version=[5],Kind=[AddedNormal],ARef=[AI12-0204-1]}
   @ChgAdded{Version=[5],Text=[When the @StdTitle says that some construct
     @i<C1> has equivalent dynamic semantics to some other construct @i<C2>,
     then there should be a language rule that says that @i<C1> is illegal
     if @i<C2> is illegal.]}

     @begin{Reason}
       @Chgref{Version=[5],Kind=[AddedNormal],ARef=[AI12-0204-1]}
       @ChgAdded{Version=[5],Text=[We do not want to infer @LegalityTitle
       from @RuntimeTitle rules.]}
     @end{Reason}

   @Chgref{Version=[5],Kind=[AddedNormal],ARef=[AI12-0064-2]}
   @ChgAdded{Version=[5],Text=[When the value of an @nt{expression} is needed
     to enforce a @LegalityName or @LinktimeName, then there should be a
     language rule requiring the @nt{expression} to be static (see
     @RefSecNum{Static Expressions and Static Subtypes}).]}

     @begin{Discussion}
       @Chgref{Version=[5],Kind=[AddedNormal],ARef=[AI12-0064-2]}
       @ChgAdded{Version=[5],Text=[This most likely occurs when the
         @nt{expression} specifies the value of an aspect.]}
     @end{Discussion}
@end{Metarules}

@begin{ImplReq}
@Leading@Keepnext@;The language definition classifies errors into several
different categories:
@begin(Itemize)
     @Keepnext@;Errors that are required to be detected prior to run time by every
     Ada implementation;

     @NoPrefix@;These errors correspond to any violation of a rule given in this
     @IntlStdName, other than those listed below.
     In particular, violation of any rule that uses the
     terms shall, allowed, permitted, legal, or illegal belongs to this
     category. Any program that contains such an error is not a legal
     Ada program; on the other hand, the fact that a program is legal
     does not mean, @i(per se), that the program is free from other
     forms of error.

     @NoPrefix@Defn{compile-time error}
     @Defn2{Term=[error], Sec=(compile-time)}
     @IndexSee{Term=[link-time error],See=(post-compilation error)}
     @Defn2{Term=[error], Sec=(link-time)}
     The rules are further classified as either compile time rules, or
     post compilation rules, depending on whether a violation has to be
     detected at the time a compilation unit is submitted to
     the compiler,
     or may be postponed until the time a compilation unit is
     incorporated into a partition of a program.
     @begin{Ramification}
       See, for example, @RefSec(Subunits of Compilation Units),
       for some errors that are detected only after compilation.
       Implementations are allowed, but not required, to detect post
       compilation rules at compile time when possible.
     @end{Ramification}

     @Keepnext@;Errors that are required to be detected at run time by the
     execution of an Ada program;

     @NoPrefix@Defn{run-time error}
     @Defn2{Term=[error], Sec=(run-time)}
     The corresponding error situations are associated with the names of
     the predefined exceptions. Every Ada compiler is required to
     generate code that raises the corresponding exception if such an
     error situation arises during program execution.
     @Redundant[If such an error situation is certain to arise in every
     execution of a construct, then an implementation is allowed
     (although not required) to report this fact at compilation time.]


     @Keepnext@;Bounded errors;

     @NoPrefix@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0444-1]}@;The 
     language rules define certain kinds of errors that
     @Chg{Version=[5],New=[are not expected to],Old=[need not]} be
     detected either prior to or during run time, but if not detected,
     the range of possible effects shall be bounded.
     @Defn{bounded error}
     The errors of this category are called @i{bounded errors}.
     The possible effects of a given bounded error are specified for
     each such error, but in any case one possible effect of a bounded
     error is the raising of the exception Program_Error.
     @Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}

     @Keepnext@;Erroneous execution.

     @NoPrefix@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0444-1]}@Defn{erroneous execution}
     In addition to bounded errors, the language rules define certain
     kinds of errors as leading to @i{erroneous execution}. Like bounded
     errors, the implementation @Chg{Version=[5],New=[is not expected
     to],Old=[need not]} detect such errors either prior
     to or during run time. Unlike bounded errors, there is no
     language-specified bound on the possible effect of erroneous
     execution; the effect is in general not predictable.
     @begin{Ramification}
       Executions are erroneous, not programs or parts of programs.
       Once something erroneous happens, the execution of the entire program
       is erroneous from that point on, and potentially before given
       possible reorderings permitted by
       @RefSecNum(Exceptions and Optimization) and elsewhere.
       We cannot limit it to just one partition,
       since partitions are not required to live in separate address spaces.
       (But implementations are encouraged to limit it as much as possible.)

       Suppose a program contains a pair of things that will be executed @lquotes@;in
       an arbitrary order@rquotes.
       It is possible that one order will result in something sensible, whereas
       the other order will result in erroneous execution.
       If the implementation happens to choose the first order,
       then the execution is not erroneous.
       This may seem odd, but it is not harmful.

       Saying that something is erroneous is semantically
       equivalent to saying that the behavior is unspecified.
       However, @lquotes@;erroneous@rquotes has a slightly more disapproving
       flavor.
     @end{Ramification}

@end(Itemize)
@end{ImplReq}

@begin{ImplPerm}
@Redundant[@Defn2{Term={mode of operation}, Sec=(nonstandard)}
@Defn{nonstandard mode}An implementation may provide
@i(nonstandard modes) of operation.
Typically these modes would be selected by a @nt<pragma> or by a command line
switch when the compiler is invoked. When operating in
a nonstandard mode, the implementation may reject @nt<compilation_unit>s
that do not conform to additional requirements associated
with the mode, such as an excessive number of warnings or violation
of coding style guidelines. Similarly, in a nonstandard mode,
the implementation may apply special optimizations or alternative
algorithms that are only meaningful for programs that
satisfy certain criteria specified by the implementation.
@Defn2{Term={mode of operation}, Sec=(standard)}
@Defn{standard mode}
In any case, an implementation shall support a @i(standard) mode that
conforms to the requirements of this @IntlStdTitle; in particular, in the standard
mode, all legal @nt<compilation_unit>s shall be accepted.]
@begin{Discussion}
  These permissions are designed to authorize explicitly the
  support for alternative modes. Of course, nothing we say can
  prevent them anyway, but this (redundant) paragraph is designed
  to indicate that such alternative modes are in some sense @lquotes@;approved@rquotes@;
  and even encouraged where they serve the specialized needs of
  a given user community, so long as the standard mode, designed
  to foster maximum portability, is always available.
@end{Discussion}
@end{ImplPerm}

@begin{ImplAdvice}
If an implementation detects a bounded error or erroneous execution,
it should raise Program_Error.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[If a bounded error or erroneous execution is detected, Program_Error
should be raised.]}]}
@end{ImplAdvice}

@begin{DiffWord83}
Some situations that are erroneous in Ada 83 are no longer errors
at all.
For example, depending on the parameter passing mechanism when
unspecified is possibly nonportable, but not erroneous.

Other situations that are erroneous in Ada 83 are changed
to be bounded errors.
In particular, evaluating an uninitialized scalar variable is
a bounded error.
The possible results are to raise Program_Error (as always), or to
produce a machine-representable value (which might not be in the
subtype of the variable).
Violating a Range_Check or Overflow_Check raises
Constraint_Error, even if the value came from an uninitialized
variable.
This means that optimizers can no longer
@lquotes@;assume@rquotes@; that all variables are initialized within their subtype's
range.
Violating a check that is suppressed remains erroneous.

The @lquotes@;incorrect order dependences@rquotes@; category of errors is removed.
All such situations are simply considered potential nonportabilities.
This category was removed due to the difficulty of defining
what it means for two executions to have a @lquotes@;different effect@rquotes.
For example, if a function with a side effect is called twice in a single
expression, it is not in principle possible for the compiler to
decide whether the correctness of the resulting program depends on the order
of execution of the two function calls. A compile time warning
might be appropriate, but raising of Program_Error at
run time would not be.
@end{DiffWord83}


@Comment{References are found in a set of separate files}
@Comment{Terms and Definitions is found in a separate file}