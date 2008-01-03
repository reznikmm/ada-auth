@Part(defs, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/defs.mss,v $}
@comment{$Revision: 1.5 $ $Date: 2007/12/28 07:00:42 $}

@LabeledAnnex{Definitions}

@Comment{Some additional definitions are given in the appropriate sections
of this document.}

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

@ToGlossary{Term=[Ada],Text=[Short for @b{Ada programming language}. The term
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

@ToGlossary{Term=[Ada Conformity Assessment Test Report (ACATR)],Text=[A
report summarizing the results of formal ACATS testing. Test Reports are
issued only after witness testing is completed, and contain a summary
of the testing (including which Specialized Needs Annexes were tested, any
test modifications needed, and the values used in customizing the support
files). Recent test reports can be found on-line at
@URLLink{URL=[http://www.adaic.org/compilers/ada95.html],
Text=[http://www.adaic.org/compilers/ada95.html]}, linked from the
Certified Processors List.]}

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
Ada Conformity Assessment Laboratory.]}

@ToGlossary{Term=[Ada Validation Organization (AVO)],Text=[Organization that
formerly performed the functions of the Ada Conformity Assessment Authority.]}

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
every paragraph of the Ada Standards documents (@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}},
@LocalLink{Target=[TC1],Sec=[References],Text={[TC1]}}, and
@LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}}).
Each paragraph has an indication of whether it contains a testable Ada
requirement, and if so, suggested test objectives to cover the requirements of
the paragraphs. Paragraphs that include objectives also indicate what ACATS
test(s) specifically test those objectives.]}

@ToGlossary{Term=[Deviation],Text=[Failure of an Ada implementation to produce
an acceptable result when processing an ACATS test program.]}

@ToGlossary{Term=[Specialized Needs Annex (SNA)],Text=[One of annexes C through H of
@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}. Conformity testing against one or more Specialized Needs Annexes is
optional. There are tests that apply to each of the Specialized Needs Annexes.
Results of processing these tests (if processed during a conformity assessment)
are reported on the certificate and in the Certified Processors List.]}

@ToGlossary{Term=[Validated Compilers List (VCL)],Text=[Former designation of
the Certified Processors List.]}

@ToGlossary{Term=[Validated Implementation],Text=[Informally used to mean
Conforming Implementation.]}

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

