@Part(defs, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/defs.mss,v $}
@comment{$Revision: 1.7 $ $Date: 2010/08/23 18:51:28 $}

@LabeledAnnex{Definitions}

@Comment{Some additional definitions are given in the appropriate sections
of this document.}

@ToGlossary{Term=[Acceptable result],Text=[The result of processing an ACATS
test program that meets the explicit grading criteria for a grade of "passed"
or inapplicable.]}

@ToGlossary{Term=[ACATS Modification List],Text=[(Abbreviated @b{AML})
A list maintained by the
ACAA documenting the currently modified and withdrawn tests. It also documents
any new tests that have been or will be added to the test suite. The ACATS
modification list is updated from time to time as challenges from implementers
are received and processed, new tests are created, or as other technical
information is received.@SeeAlso{Primary=[AML],Other=[ACATS Modification List]}]}

@ToGlossary{Term=[ACVC Implementer's Guide],Text=[(Abbreviated @b{AIG}) A document describing
the test objectives used to produce test programs for @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}} ACVC versions
(1.1-1.11). AIG section references are embedded in @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}} test naming
conventions.@SeeOther{Primary=[AIG],Other=[ACVC Implementer's Guide]}]}

@ToGlossary{Term=[Ada],Text=[Short for @b{Ada programming language}. The term
Ada by itself always refers to the most current ISO/IEC standard document(s);
if a specific version of the language standards is meant it will always be
referred to explicitly (for instance, as @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}
or @LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}).]}

@ToGlossary{Term=[Ada Conformity Assessment Authority],Text=[(Abbreviated @b{ACAA})
The part of
the certification body that provides technical guidance for operations of the
Ada certification system.@SeeOther{Primary=[ACAA],Other=[Ada Conformity Assessment Authority]}]}

@ToGlossary{Term=[Ada Conformity Assessment Laboratory],Text=[(Abbreviated @b{ACAL})
The part
of the certification body that carries out the procedures required to perform
conformity assessment of an Ada implementation. (Formerly
AVF)@SeeOther{Primary=[ACAL],Other=[Ada Conformity Assessment Laboratory]}]}

@ToGlossary{Term=[Ada Conformity Assessment Test Report],Text=[(Abbreviated @b{ACATR})
A report summarizing the results of formal ACATS testing. Test Reports are
issued only after witness testing is completed, and contain a summary
of the testing (including which Specialized Needs Annexes were tested, any
test modifications needed, and the values used in customizing the support
files). Recent test reports can be found on-line at
@URLLink{URL=[http://www.ada-auth.org/cpl.html],
Text=[http://www.ada-auth.org/cpl.html]}, linked from the
Certified Processors List.@SeeOther{Primary=[ACATR],Other=[Ada Conformity Assessment Test Report]}]}

@ToGlossary{Term=[Ada implementation],Text=[An Ada compilation system,
including any required run-time support software, together with its host  and
target computer systems.]}

@ToGlossary{Term=[Ada Joint Program Office],Text=[(Abbreviated @b{AJPO})
An organization within
the U.S. Department of Defense that sponsored the development of the ACVC and
formerly provided policy and guidance for an Ada certification
system.@SeeOther{Primary=[AJPO],Other=[Ada Joint Program Office]}]}

@ToGlossary{Term=[Ada programming language],Text=[The language defined by
the current Ada Standard documents.]}

@ToGlossary{Term=[Ada Resource Association],Text=[(Abbreviated @b{ARA})
The trade association
that sponsors the Ada conformity assessment
system.@SeeOther{Primary=[ARA],Other=[Ada Resource Association]}]}

@ToGlossary{Term=[Ada Standard documents],Text=[The documents that
define the Ada programming language:
reference @LocalLink{Target=[Ada95],
Sec=[References],Text={[Ada95]}}, its corrigiendum @LocalLink{Target=[TC1],
Sec=[References],Text={[TC1]}}, and its Amendment @LocalLink{Target=[Amend1],
Sec=[References],Text={[Amend1]}}. An unofficial document
informally known as the @ldquote@;Ada 2005 Reference Manual@rdquote
merges all three documents and is what is usually used by Ada practitioners.]}

@ToGlossary{Term=[Ada Validation Facility],Text=[(Abbreviated @b{AVF})
Former designation of an
Ada Conformity Assessment Laboratory.@SeeOther{Primary=[AVF],Other=[Ada Validation Facility]}]}

@ToGlossary{Term=[Ada Validation Organization],Text=[(Abbreviated @b{AVO})
Organization that
formerly performed the functions of the Ada Conformity
Assessment Authority.@SeeOther{Primary=[AVO],Other=[Ada Validation Organization]}]}

@ToGlossary{Term=[Certification Body],Text=[The organizations (ACAA and ACALs)
collectively responsible for defining and implementing Ada conformity
assessments, including production and maintenance of the ACATS tests, and award
of Ada Conformity Assessment Certificates.]}

@ToGlossary{Term=[Certified Processors List],Text=[(Abbreviated @b{CPL})
A published list
identifying all certified Ada implementations. The CPL is available on the ACAA
Internet site
(@URLLink{URL=[http://www.ada-auth.org],Text=[www.ada-auth.org]}).@SeeOther{Primary=[CPL],Other=[certified processors list]}]}

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
every paragraph of the Ada Standard documents.
Each paragraph has an indication of whether it contains a testable Ada
requirement, and if so, suggested test objectives to cover the requirements of
the paragraphs. Paragraphs that include objectives also indicate what ACATS
test(s) specifically test those objectives.]}

@ToGlossary{Term=[Deviation],Text=[Failure of an Ada implementation to produce
an acceptable result when processing an ACATS test program.]}

@ToGlossary{Term=[Specialized Needs Annex],Text=[(Abbreviated @b{SNA})
One of annexes C through H of
@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}. Conformity testing against one or more Specialized Needs Annexes is
optional. There are tests that apply to each of the Specialized Needs Annexes.
Results of processing these tests (if processed during a conformity assessment)
are reported on the certificate and in the Certified Processors
List.@SeeOther{Primary=[SNA],Other=[specialized needs annex]}]}

@ToGlossary{Term=[Validated Compilers List],Text=[(Abbreviated @b{VCL})
Former designation of
the Certified Processors List.@SeeOther{Primary=[VCL],Other=[validated compilers list]}]}

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

