@Part(crules, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/tobj.mss,v $}
@comment{$Revision: 1.4 $ $Date: 2014/01/31 06:00:07 $}

@LabeledSection{Test Objectives and Coverage}

Each ACATS test tests one or more @i{test objectives}.@Defn{test objective}
@ToGlossary{Term=[Test Objective],Text=[The intended purpose of an ACATS
test. A test objective ought to be relatable to rules given in the Standards
that define Ada.]} Test objectives ought to be relatable to rules given
in the Ada Standard documents (@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}},
@LocalLink{Target=[TC1],Sec=[References],Text={[TC1]}}, and
@LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}}).

The test objectives of modern ACATS tests are gathered into a
@i{Test Objectives Document}. Each modern test is listed along with the
objectives that it tests (legacy tests aren't included as they use
a different form of objective). This document provides easier searching for
particular objectives than a raw search of all of the test files.
@Defn{test objectives document}
@ToGlossary{Term=[Test Objectives Document],Text=[A document containing
the test objectives used for Modern ACATS tests. Information on Legacy tests is
not included.]}

This section provides information on how new test objectives are constructed
and how the adequacy of objectives is checked.


@LabeledClause{Test Objectives and Rules}

@leading@;A test objective should relate as directly as possible to the rules
of Ada. Ideally, a test objective will test an individual rule of Ada.
@i{Rule} means
whatever set of sentences makes up a testable statement. A rule may be as small
as a single line, or spread across multiple paragraphs of
the standards.@Defn2{Term=[rule],Sec=[Ada language]}

For instance,
@LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}} contains the
rule
@begin{Indent}
The implicit declaration of the limited view of a library package forms an
(implicit) compilation unit whose @s{context_clause} is empty.
@end{Indent}
@leading@;in subclause 10.1.1. The corresponding test objective is
@begin{Indent}
Check that the context clause of a limited view is empty.
@end{Indent}

However, virtually all rules require the use of other rules (such as those
for definitions of terms) in order to be testable. For instance, testing
the above rule needs the definition of terms like "limited view" and
"@s{context_clause}" (among others) in order for a test to be constructed.

As such, a test objective will generally apply to multiple Ada rules.
Typically, it will be recorded for a specific rule. For the
purposes of deciding if a rule is appropriately tested, it is necessary
to study the ACATS coverage documents (see @RefSecNum{Coverage Documents}).


@LabeledClause{Coverage of the Ada Standard}

The Ada Standard documents (@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}},
@LocalLink{Target=[TC1],Sec=[References],Text={[TC1]}}, and
@LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}}) include many
hundreds of rules. The ACATS Coverage
Documents (see @RefSecNum{Coverage Documents}) record the mapping of
test objectives and tests to Ada rules, making it possible to determine if
individual rules are adequately tested. The following clauses discuss these
documents and the rules by which they are constructed.


@LabeledSubClause{Coverage Documents}

ACVC 2.0 originated coverage documentation for what is now the ACATS. That
coverage document mapped tests to paragraph numbers in the
@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}} Standard. Over time,
a number of deficiencies in that approach have been identified:
@begin{Itemize}
Where a paragraph contains multiple rules, it is not possible to tell if all
of the rules are tested. Indeed, a number of instances where important rules
were untested have been identified.

Since there is not an indication of the type of rule represented by a paragraph,
it is not obvious whether the rule is appropriately tested. For instance, a
Legality Rule tested only with C-Tests is not appropriately tested (see
@RefSecNum{Coverage Guidelines for Specific Rule Categories} for more on this
topic).

Rules not modified in @LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}
were not considered. However, sometimes additional test objectives are needed
for existing rules because of other changes to the language. Moreover, there
is no determination of whether the legacy tests adequately tested the original
rule.
@end{Itemize}

For these reasons, the development of new and much more
detailed coverage documentation was initiated for ACATS 3.0. The new
documentation considers each sentence of the standard individually if necessary,
listing the test objectives for each sentence, and documenting why no objectives
are needed if none are provided.

It also lists tests for each objective (including Legacy tests),
testing priorities for untested
objectives, notes on each objective (including
any untested cases), and any additional notes needed.

There is also a summary document that summarizes totals for important metrics
for the coverage documents as a whole.

These documents are provided in Adobe Portable Document Format (PDF).

@leading@;For ACATS 3.1, the following clauses of the Ada Standards are
included in the new coverage documents:
@begin{Example}
3.9.3 through 3.10.1
4.1.3 through 4.4
6.5 through 6.7
7.4 through 7.6.1
8.3.1 through 8.5.3
Section 10 (10 through 10.2.1)
@end{Example}
These clauses were selected because of their importance in rule changes
in @LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}}.

The original coverage document is also included with the ACATS for those
portions of the Ada Standards that have not yet had new coverage documents
constructed. This document is of primary use for features
of @LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}},
as it does not cover Legacy tests nor any features
added or modified by @LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}}.
As noted above, it is being replaced, so it will disappear completely in a
future version of the ACATS.


@LabeledSubClause{General Coverage Guidelines}

As noted in Section @RefSecNum{ACATS coverage of Ada}, the ACATS strives for
complete coverage of the Ada standard. However, complete coverage does not
mean that every sentence in the standard has an associated test objective
and test. Not all rules are testable; one important way to determine
testability of a rule is to check its category. The next clause
(@RefSecNum{Coverage Guidelines for Specific Rule Categories}) will
discuss how the category of a rule affects its mapping to test objectives.

There are also additional considerations that don't apply to specific
rule categories.

Text sometimes contains definitions (especially in Static Semantics rules).
Definitions are hard to handle, because they usually are not testable by
themselves. The definition has to be used in another rule to make them visible.
For instance, a categorization rule such as "something is either this, that, or
fuzzy" cannot be tested by itself. "Something" has to be used in some other
rule in order for the categorization to matter. That being the case, it usually
makes sense to test the definition as part of the other rule(s). Such rules
are marked as having a kind of "Widely Used" or "Subpart" in the coverage
documentation. "Subpart" rules are tested as part of specific other objectives,
while "Widely Used" rules are thought to be tested by many tests indirectly
(and no attempt is made to verify that). In some cases,
especially those that otherwise would have a combinatorial explosion, it may
make more sense to test the definition as directly as possible (in which case
objectives will be assigned to the definition).

Text sometimes includes sentences marked as redundant by the AARM. Such
sentences should be given normatively elsewhere, and the testing should be done
in the place where they are normatively given. The AARM often will indicate
this place.

Combinational explosion is always a problem for visibility rules. Rules that
define the scope or visibility of something should always be tested in place
(even though other rules will need to be used to accomplish that).


@LabeledSubClause{Coverage Guidelines for Specific Rule Categories}

@leading@;The Ada standard is divided into various categories of rules. These
rule categories have different requirements and thus differ in what is the
appropriate testing philosophy. This translates into different types of
test objectives for each rule category.

@begin{Description}
Uncategorized text:@\This is generally introductory text, and does not require
any testing. This text sometimes includes definitions; these are handled as
described in @RefSec{General Coverage Guidelines}.

Syntax:@\Testing that legal syntax is implemented is accomplished by all of
the other kinds of tests, especially by the usage-oriented tests. Generally,
tests for illegal syntax have a very low value. Most compilers use a syntax
that is similar to
that of the standard, and many use parsers generated by tools, and thus the
likelihood of errors is low. Moreover, the number of possible bugs is
essentially unlimited. There are two cases where testing is warranted:
First, rules that
are given in text form rather than BNF are treated as legality rules (see below).
Second,
the Ada syntax grammar is not directly usable by either of the common technologies
for parsers (LL/recursive descent, and LR/LALR). In cases where the grammar
used by a compiler must necessarily be more general than that given in the
standard, the additional requirements are treated as legality rules.

Name Resolution Rules:@\Name resolution rules should usually have a B-Test that
checks that too much information is not used for resolution. (For instance,
whether a type is limited or non-limited should generally not be used.
Another example is whether an access type is pool-specific or general
access usually should not be used.) It also may have a C-Test to
check that legal cases are allowed; but this objective is often handled by
tests for other test objectives (if that's the case, a note should point out
where they are tested).

Legality Rules:@\Legality rules should almost always have an associated B-Test
that checks that illegal cases are detected. They may also have a C-Test to
check that legal cases are allowed; again, this is often handled by tests for
other test objectives. (They also can be covered in the B-Test with "OK"
cases.)

Static Semantics:@\Many Static Semantic rules are definitions, and as such
can be handled as described in @RefSec{General Coverage Guidelines}.

@\Many other static semantic rules are miscategorized by the standard. For
instance, most of the library packages are defined as static semantics, even
though most of the rules are really dynamic. These should be tested as
appropriate for the correct category.

Post-Compilation Rules:@\A post-compilation rule should always have L-Tests to
check that the check is made in illegal cases. These checks are especially
likely to be omitted (because they're often complex to implement), so these
tests should have a high priority. In some cases, a C-Test may be needed to
ensure that legal cases work and the check does not go too far. Often that will
be covered by other tests, so a separate test is not needed (a note should be
provided to show where it is covered).

Dynamic Semantics:@\Dynamic semantics rules can be divided into two sub-categories.

@\First, these rules can define run-time checks. These checks should have
C-Tests that verify that the check is made and the appropriate exception is
raised. Tests like this have to be carefully constructed to avoid
running afoul of 11.6 permissions.

@\Otherwise, these rules can define the normal operation of a construct. These
should have usage-oriented C-Tests. By making these tests usage-oriented, the
ACATS tests the normal usage of these features, not unusual corner-cases.

@\One special case: rules that say the execution of something "has no effect"
can't be usefully tested. Testing that nothing happens is not interesting. And
testing for the absence of an effect requires guessing incorrect effects that
might have occurred (which is unlikely to actually detect anything), and thus
any such test would have a very low value.

Bounded (Run-Time) Errors:@\These usually require C-Tests, but not always. If
the bound includes "work normally", then the bounded error is not usefully
testable, since the ACATS does not test implementation
choices. Whether something works or raises Program_Error is not interesting.
In other cases, however, since there is a bound on the behavior, it can be
useful to ensure that one of the prescribed results happens rather than havoc.

Erroneous Execution:@\This is not testable. Since the language allows anything
to happen, there is no useful information to be gained from including this in a
test. Indeed, it's important to avoid any such case in an executable test.

Implementation Requirements:@\These usually are dynamic requirements that
should be tested like the appropriate dynamic semantics rules. Otherwise, they
should be tested like a legality rule.

Implementation Permissions:@\Usually permissions are not testable. Since the
ACATS is not testing the quality of implementations, the choices made by
an implementation are not appropriate things to test. So a test should be
created only if there is value in testing for implementations exceeding
the permission.

Implementation Advice:@\Advice is usually not testable. Again, since the ACATS
is not testing quality of implementations, the choices made by an
implementation are not appropriate things to test. Moreover, advice need not be
followed, and other implementation choices can be made. Usually, if there are
bounds to what is acceptable, they are covered by other rules, and thus the
tests would be there.

Documentation Requirements and Metrics:@\No tests are required for
documentation requirements. Documentation existence could be part of the
conformity assessment process, but in any case it is outside of the scope of
the ACATS (which is testing the implementation, not the documentation).

Notes and Examples:@\These are non-normative text, and do not
require any testing.

@end{Description}

