@Part(defs, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/defs.mss,v $}
@comment{$Revision: 1.1 $ $Date: 2007/12/14 06:19:00 $}

@LabeledNormativeAnnex{Definitions}

@ToGlossary{Term=[Acceptable result],Text=[The result of processing an ACATS
test program that meets the explicit grading criteria for a grade of "passed"
or inapplicable.]}

@ToGlossary{Term=[ACATS Modification List (AML)],Text=[A list maintained by the
ACAA documenting the currently modified and withdrawn tests. It also documents
any new tests that have been or will be added to the test suite. The ACATS
modification list is updated from time to time as challenges from implementers
are received and processed, new tests are created, or as other technical
information is received.]}

@ToGlossary{Term=[ACVC Implementer's Guide (AIG)],Text=[A document describing
the test objectives used to produce test programs for @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}} ACVC versions
(1.1-1.11). AIG section references are embedded in @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}} test naming
conventions.]}

@ToGlossary{Term=[Ada],Text=[Short for @b{Ada programming language} (see). The term
Ada by itself always refers to the most current ISO/IEC standard document(s);
if a specific version of the language standards is meant it will always be
referred to explicitly (for instance, as @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}
or @LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}).]}

@ToGlossary{Term=[Ada Conformity Assessment Authority (ACAA)],Text=[The part of
the certification body that provides technical guidance for operations of the
Ada certification system]}

@ToGlossary{Term=[Ada Conformity Assessment Laboratory (ACAL)],Text=[The part
of the certification body that carries out the procedures required to perform
conformity assessment of an Ada implementation. (Formerly AVF)]}

@ToGlossary{Term=[Ada implementation],Text=[An Ada compilation system,
including any required run-time support software, together with its host  and
target computer systems.]}

@ToGlossary{Term=[Ada Joint Program Office (AJPO)],Text=[An organization within
the U.S. Department of Defense that sponsored the development of the ACVC and
formerly provided policy and guidance for an Ada certification system.]}

@ToGlossary{Term=[Ada programming language],Text=[The language defined by
reference [Ada95], its corrigendum [TC1], and its Amendment [Amend1].]}

@ToGlossary{Term=[Ada Resource Association (ARA)],Text=[The trade association
that sponsors the Ada conformity assessment system.]}

@ToGlossary{Term=[Ada Validation Facility (AVF)],Text=[Former designation of an
Ada Conformity Assessment Laboratory (which see).]}

@ToGlossary{Term=[Ada Validation Organization (AVO)],Text=[Organization that
formerly performed the functions of the Ada Conformity Assessment Authority
(which see).]}

@ToGlossary{Term=[Certification Body],Text=[The organizations (ACAA and ACALs)
collectively responsible for defining and implementing Ada conformity
assessments, including production and maintenance of the ACATS tests, and award
of Ada Conformity Assessment Certificates.]}

@ToGlossary{Term=[Certified Processors List (CPL)],Text=[A published list
identifying all certified Ada implementations. The CPL is available on the Ada
Information Clearinghouse Internet site
(@URLLink{URL=[http://www.adaic.org],Text=[www.adaic.org]}).]}

@ToGlossary{Term=[Challenge],Text=[A documented disagreement with the test
objective, test code, test grading criteria, or result of processing an ACATS
test program when the result is not PASSED or INAPPLICABLE according to the
established grading criteria. A challenge is submitted to the ACAA.]}

@ToGlossary{Term=[Conforming implementation],Text=[An implementation that
produces an acceptable result for every applicable test. Any deviation
constitutes a non-conformity.]}

@ToGlossary{Term=[Core language],Text=[Sections 2-13 and Annexes A, B, and J
of @LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}. All implementations are required to implement the core language.
The tests for core language features are required of all implementations.]}

@ToGlossary{Term=[Coverage documents],Text=[Documents containing an analysis of
every paragraph of the Ada Standards documents (@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}, [TC1], and [Amend1]).
Each paragraph has an indication of whether it contains a testable Ada
requirement, and if so, suggested test objectives to cover the requirements of
the paragraphs. Paragraphs that include objectives also indicate what ACATS
test(s) specifically test those objectives.]}

@ToGlossary{Term=[Deviation],Text=[Failure of an Ada implementation to produce
an acceptable result when processing an ACATS test program.]}

@ToGlossary{Term=[Foundation Code],Text=[Packages used by multiple tests;
foundation code is designed to be reusable. Generally a foundation is a package
containing types, variables, and subprograms that are applicable and useful to
a series of related tests. Foundation code is never expected to cause compile
time errors. It may be compiled once for all tests that use it or recompiled
for each test that uses it; it must be bound with each test that uses it.]}

@ToGlossary{Term=[Legacy Tests],Text=[Tests that were included in ACVC 1.12
that have been incorporated into later ACVC and ACATS versions. The vast
majority of these tests check for language features that are upwardly
compatible from @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}
to later versions of Ada. Some of these tests have been
modified from the ACVC 1.12 versions to ensure that Ada rules are properly
implemented in cases where there were extensions or incompatibilities from
@LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}} to later versions of Ada.]}

@ToGlossary{Term=[Specialized Needs Annex],Text=[One of annexes C through H of
@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}. Conformity testing against one or more Specialized Needs Annexes is
optional. There are tests that apply to each of the Specialized Needs Annexes.
Results of processing these tests (if processed during a conformity assessment)
are reported on the certificate and in the Validated Compilers List.]}

@ToGlossary{Term=[Test Objectives Document (TOD)],Text=[A document containing
the test objectives used for newer ACATS tests. Information on Legacy tests is
not included.]}

@ToGlossary{Term=[Validated Compilers List (VCL)],Text=[Former designation of
the Certified Processors List (which see).]}

@ToGlossary{Term=[Validated Implementation],Text=[Informally used to mean
Conforming Implementation (see).]}

@ToGlossary{Term=[Validation],Text=[ Informally used to mean conformity
assessment.]}

@ToGlossary{Term=[Withdrawn Test],Text=[A test found to be incorrect and not
used in conformity testing. A test may be incorrect because it has an invalid
test objective, fails to meet its test objective, or contains erroneous or
illegal use of the Ada programming language. Withdrawn tests are not applicable
to any implementation. Withdrawn tests are often modified and restored to
subsequent ACATS releases.]}

@ToGlossary{Term=[Witness Testing],Text=[Conformity assessment testing
performed in the presence of ACAL personnel. Witness testing adds the assurance
that the test procedures were followed and that the results were verified.]}

@GlossaryList

