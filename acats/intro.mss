@Part(intro, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/intro.mss,v $}
@comment{$Revision: 1.8 $ $Date: 2016/04/30 02:41:12 $}

@LabeledSection{Introduction}

The Ada Conformity Assessment Test Suite (ACATS) provides the official
tests used to check conformity of an Ada implementation with the
Ada programming
language standard (@LocalLink{Target=[Ada2012],Sec=[References],
Text=[ANSI/ISO/IEC 8652:2012]} and later corrigenda, including
@LocalLink{Target=[TC1-2012],Sec=[References],
Text=[ANSI/ISO/IEC 8652:2012/COR 1:2016]}). The ACATS User's
Guide is part of the ACATS and is distributed
with the test programs and testing support packages. It explains the contents
and use of the
test suite.@Defn{Ada Conformity Assessment Test Suite}@SeeOther{Primary=[ACATS],Other=[Ada Conformity Assessment Test Suite]}@Defn{Ada implementation}@Defn{Ada}@Defn{Ada programming language}

The ACATS is an important part of the conformity assessment process described
in ISO/IEC-18009, Ada: Conformity of a Language Processor @LocalLink{Target=[ISO99],
Sec=[References],Text={[ISO99]}}. This
standard provides a framework for testing language processors, providing a
stable and reproducible basis for testing. The Ada Resource Association (ARA) has
sponsored an instantiation of that process since October 1998. The process is
managed by the Ada Conformity Assessment Authority (ACAA).@Defn{Ada Conformity Assessment Authority}@Defn{Ada Resource Association}@Defn{Ada Validation Organization}

Prior to the ISO standard, the U.S. Department of Defense sponsored a similar
conformity assessment process under the Ada Joint Program Office (AJPO). The
test suite for that process was known as the Ada Compiler Validation Capability
(ACVC).@Defn{Ada Compiler Validation Capability}@Defn{Ada Joint Program Office}@SeeOther{Primary=[ACVC],Other=[Ada Compiler Validation Capability (ACVC)]}
The AJPO developed ACVC versions based on @LocalLink{Target=[Ada83],
Sec=[References],Text=[ANSI/MIL-STD-1815A-1983, ISO/8652:1987]}
(Ada 83), which were numbered 1.x where x ranged from 1 to 11. It
later developed ACVC versions based on ANSI/ISO/IEC 8652:1995
(@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}), numbered
2.0, 2.0.1, 2.1, and 2.2.

When the ACAA took over Ada conformity assessment, it adopted the ACVC as the
basis for its test suite. The ACAA determined to continue to use the same
version numbering for the test suite in order to avoid confusion. The version
of the ACVC current at the time (2.1) was initially used as ACATS 2.1. Later,
the already developed but unreleased ACVC 2.2 was released and used as ACATS
2.2. The ACAA later released ACATS 2.3, ACATS 2.4, ACATS 2.5, and then ACATS
2.6 to include maintenance changes and a few new tests.

In 2007, the ACAA developed ACATS version 3.0 to check for conformity to new
language features defined in ISO/IEC 8652:1995/AMD 1:2007
(@LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}}), as well as test
programs to check for conformity to language features defined in earlier
versions of Ada, including @LocalLink{Target=[Ada95],
Sec=[References],Text={[Ada95]}} and @LocalLink{Target=[Ada83],
Sec=[References],Text={[Ada83]}}. The ACAA later released ACATS 3.1 to improve
the coverage and correctness of the ACATS for features defined in
@LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}}.

In 2014, the ACAA developed ACATS version 4.0 to test the enhancements and
changes of the third edition of the Ada Standard,
ISO/IEC 8652:2012 (@LocalLink{Target=[Ada2012],
Sec=[References],Text={[Ada2012]}}). This version of the ACATS, version 4.1,
was released to improve the coverage and correctness of the ACATS for features
defined in @LocalLink{Target=[Ada2012],
Sec=[References],Text={[Ada2012]}}, including features originally defined in
earlier versions of the Ada Standard. Subsequent maintenance or enhancement
versions of the suite, if they are required, will be numbered 4.2, etc.

The ACATS User's Guide describes the set of ACATS tests and how they are to be
used in preparation for conformity assessment. The formal procedures for
conformity assessment are described in @LocalLink{Target=[Pro31],
Sec=[References],Text={[Pro31]}}, and the rules in that document
govern all conformity assessments, notwithstanding anything in this document
that may be interpreted differently. Moreover, this guide does not discuss
specific requirements on processing of the ACATS test suite, or submission and
grading of results that an Ada Conformity Assessment Laboratory (ACAL) may
impose.@Defn{Ada Conformity Assessment Laboratory}@Defn{Ada Validation Facility}

The User's Guide is intended to be used by compiler implementers, software
developers who maintain a version of the ACATS as a quality control or software
acceptance tool, and third-party testers (e.g., Ada Conformity Assessment
Laboratories).

Section @RefSecNum{Changes for ACATS 4.1} of the User's Guide for ACATS 4.1
summarizes the changes between
ACATS 4.0 and ACATS 4.1. Section @RefSecNum{Test Objectives and Coverage}
describes test objectives and their relationship to ACATS tests and
to the rules of the
Ada Standards documents. Section @RefSecNum{Configuration Information}
describes the configuration of the ACATS,
including a description of the ACATS software and delivery files. Section
@RefSecNum{Using the ACATS}
provides step-by-step instructions for installing and using the test programs
and test support packages, and for grading test results. The appendices include
other information that characterizes the ACATS 4.1 release, along with
information on test construction.

Refer to @RefSecNum{Definitions} and Section @RefSecNum{Grading Test Results}
for the definition of an acceptable result and the rules for grading ACATS 4.1
test program results. Section @RefSecNum{Deviation from Expected Results - Petition & Review}
provides instructions for submitting a
petition against a test program if a user believes that a deviation from the
acceptable results for a given test program is in fact conforming behavior.

The ACATS test suite is available from any ACAL and from the ACAA web site.
See @URLLink{URL=[http://www.ada-auth.org/acats.html],
Text=[http://www.ada-auth.org/acats.html]}.


@LabeledClause{ACATS Purpose}

The purpose of the ACATS is to check whether an Ada compilation system is a
conforming implementation, i.e., whether it produces an acceptable result for
every applicable test.

A fundamental goal of conformity assessment (validation) is to promote Ada
software portability by ensuring consistent processing of Ada language features
as prescribed by the Ada Standard documents (@LocalLink{Target=[Ada2012],
Sec=[References],Text={[Ada2012]}}, @LocalLink{Target=[TC1-2012],
Sec=[References],Text={[TC1-2012]}} and any future corrigendum
documents@Defn{Ada Standard documents}). ACATS
tests use language features in contexts and idioms expected in production
software. While they exercise a wide range of language feature uses, they do
not and cannot include examples of all possible feature uses and interactions.

It is important to recognize that the ACATS tests do not guarantee compiler
correctness. A compilation system that correctly processes the ACATS tests is
not thereby deemed error-free, nor is it thereby deemed capable of correctly
processing all software that is submitted to it.

The ACATS tests do not test the quality of an Ada implementation. In
particular, ACATS tests do not check or report performance parameters (e.g.,
compile-time capacities or run-time speed). They do not check or report for
characteristics such as the presence and effectiveness of compiler
optimization. They do not investigate or report compiler or implementation
choices in cases where the standard allows options.


@LabeledClause{ACATS Coverage of Ada}

The ACATS needs to test as many Ada language rules as possible in order to meet
the goal of enhancing Ada software portability. After all, a rule that is not
tested is far more likely to be incorrectly implemented than one that is tested.

Therefore the ACATS strives for complete coverage of the standard. @i{Complete
coverage}@Defn{complete coverage} means that every rule in the Ada standard has
one or more associated tests that ensure that the rule is implemented properly.

Complete coverage is especially important for legality rules and runtime
checks. It is easy for implementers to miss these rules, as their compiler may
do something useful in the absence of the checks. But allowing such incorrect
code can be a major portability problem when a program is moved to a compiler
(including a later version of the same compiler) that properly implements the
rules and checks.

Of course, complete coverage does not mean that every sentence in the Ada
standard has an associated test. There are many lines in the standard that are
not rules at all, such as notes and examples. There are also many lines in the
standard that are not testable; documentation requirements are but one example.
There are also rules in the standard which could be tested, but such tests
would be outside of the purpose of the ACATS. Rules which express options
for Ada implementations (such as permissions or advice) are in this category
(as the ACATS does not test implementation quality).

It should be obvious that the importance of testing particular rules in the Ada
standard vary widely. In part, that's because the various Ada features
themselves have widely varying importance. (For example, looking at three
features that ACATS 4.1 could have tested, proper operation of
if expressions is more important than correct functioning of record equality,
which in turn is more important than run-time accessibility checks for
coextensions.) But it's also because of the differing nature of the rules.
For instance, testing of syntax rules is not very important in most cases,
as correct syntax will be checked by tests for other rules, and tests for
incorrect syntax require guessing what error a compiler might make in
interpreting syntax. Unless an error seems particularly likely (as might
happen from some irregularity in the syntax), such guessing is unlikely to
find actual errors. This makes it impossible for there to be a definitive
set of rules that need to be tested to accomplish complete coverage,
as the decision as to whether a particular rule is usefully testable
can be a judgement call.

In any case, complete coverage is a goal for the ACATS. However, it is not
expected to be ever be achieved. As the ACATS gets nearer to the goal, the
value of additional tests will drop (since higher value tests get created
earlier), and at some point, the cost of creating and processing new tests
would outweigh their value.

Since exactly which rules need to be tested to accomplish complete coverage will
always be a judgement call, the test coverage analysis of the
ACATS is a set of living documents, which will be updated with new information
and tests with each new ACATS version.

Coverage testing will generally not test combinations of features, so problems
that only manifest themselves in such combinations will not be detected. Tests
designed primarily to cover language rules are most useful to prevent gross
errors in implementations (such as forgetting to implement checks or features).
As such, the ACATS also supplements those tests with tests written to emulate
the patterns of use of Ada features. Such tests provide tests of common
combinations of features, and ensure that common idioms are implemented
properly.

A detailed description of how coverage is determined for the Ada standard
can be found in Section @RefSecNum{Coverage of the Ada Standard}.


