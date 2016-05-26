@Part(config, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/config.mss,v $}
@comment{$Revision: 1.9 $ $Date: 2016/04/30 02:41:12 $}

@LabeledSection{Configuration Information}

This section describes the physical and logical structure of the ACATS
delivery, and it describes the test classes, naming conventions used, test
program format, test structure, delivery structure, and file format.

ACATS 4.1 is an update of ACATS 4.0, and has a similar delivery
structure. The support tools are unchanged, except for updating
header comments and version identification.

The test suite does not provide tools or scripts that can be used to manage
complete test processing, since such tools are normally specific to
particular implementations and host systems.

@LabeledClause{Structure}

The ACATS 4.1 test software includes test code that exercises specific Ada
features, foundation code (used by multiple tests), support code (used to
generate test results), and tool code (used to build tools necessary to
customize ACATS tests). The suite includes tests for the core language and
tests for the Specialized Needs Annexes. The following table summarizes the
number of tests and files in the ACATS suite.

@table{Columns=[7],Alignment=[CenterExceptFirst],FirstColWidth=[2],LastColWidth=[1],
NoBreak=[T],Border=[T],SmallSize=[F],
Caption=[],
Headers=[@\Total@\Core Tests@\SNA Tests@\Found@!ations@\Docs@\Other],
Body=[Number of Files@\4787@\4287@\250@\54@\174@\22
Number of Tests@\3910@\3718@\192@\0@\0@\0]}

@leading@;Others consists of:
@table{Columns=[2],Alignment=[AllLeft],FirstColWidth=[1],LastColWidth=[4],
NoBreak=[T],Border=[F],SmallSize=[F],
Caption=[],
Headers=[],
Body=[1@\List of all files
14@\Code that is referenced by tests
3@\Code and data used for preprocessing tests to insert implementation specific information
4@\Test routines for reporting code ("CZ" tests)]}

The delivery structure of the test suite is described in
Section @RefSecNum{Delivery Directory Structure}.


@LabeledSubClause{Physical Organization}

The preceeding table summarizes the number of files that compose ACATS 4.1.
In addition to files containing test code proper, the ACATS 4.1 test suite
includes various support files.

Note that the number of files containing test code is larger than the number of
tests in the ACATS suite because some tests use code included in separate
files.

A file name consists of a name plus an extension. Multiple files that contain
code used by a single test have related names. File names are the same as that
of the test contained in the file when possible. File names conform to MS-DOS
naming conventions; therefore they may be shorter than the software name
because of file name length restrictions (e.g., enumchek rather than
enumcheck). File (and test) names follow conventions that indicate their
function in the test suite; naming conventions are explained in Section
@RefSecNum{Naming Convention}. The
files are organized into distinct directories and subdirectories based on their
function in the test suite. The directory organization is explained in
Section @RefSecNum{Delivery Directory Structure}.

@leading@;The ACATS is available to the general public from an ACAL or on the
Internet. Links to the ACATS distribution can be found on the ACAA's ACATS
page:
@begin{Indent}
@shrink{@URLLink{URL=[http://www.ada-auth.org/acats.html],Text=[http://www.ada-auth.org/acats.html]}}.
@end{Indent}

Note that the ACATS files are available in both compressed Unix tar and DOS
zipped formats. Section @RefSecNum{Guide to Decompressing Files} provides
a discussion of techniques to convert these files to a usable format.


@LabeledSubClause{Logical Organization}

The table summarizes the number of tests that check the conformance of an Ada
implementation to the core language and conformance to the Specialized Needs
Annexes of Ada.

Core tests apply to all implementations. Specialized Needs Annex tests are not
required for any implementation. Tests for a given Specialized Needs Annex may
be processed by implementations that claim implementation of that annex.

In general, no test result depends on the processing or the result of any other
test. Exceptions are noted in Section @RefSecNum{Dependencies}. No annex test
depends on the implementation of any other annex, except possibly in cases
where one annex specifically depends on another in Ada (e.g., no test for the
Information Processing Annex uses features from any other annex, however Real
Time Annex and Distributed Processing tests may depend on Systems Programming
Annex features). (There is a single exception to this rule: see Section
@RefSecNum{Tests for the Distributed Systems Annex}.) Annex tests
may use any core feature.

Tests may be created from one or more compilation units. If a test consists of
a single compilation unit (a main subprogram only), the test code will be
contained in a single file. Tests built from more than one compilation unit may
require multiple files. Moreover, some compilation units, called foundation
code, may be used by more than one test. Even in these cases, the resulting
tests are strictly independent: if test A and test B use the same foundation
code, the results of processing (and running, if appropriate) A have no effect
on the results of processing (and running, if appropriate) B. Foundation code
is more fully explained in Section @RefSecNum{Test Foundation Code}.

Tests are named using conventions that provide (limited) information about the
test. The test naming conventions are explained in Section @RefSecNum{Naming Convention}.
Each test
belongs to a single test class that indicates whether it is or is not an
executable test. Test classes are explained in Section @RefSecNum{Test Classes}.

In addition to test code and foundation code, there is code on which many or
all of the executable tests in the suite depend (e.g., package Report, package
ImpDef, package TCTouch). Some of this code must be customized to each
implementation. There is also code that must be used to build support tools
used to customize the suite of tests to an implementation. The customization
process is described in Section @RefSecNum{Tailoring the ACATS Test Suite}.


@LabeledSubClause{Legacy Tests}

@i{Legacy tests} are tests that were included in ACVC 1.12
that have been incorporated into later ACVC and ACATS versions.@Defn{legacy tests}
These tests check only language features that are common to all versions of Ada.
The vast majority of these tests came unmodified from the ACVC 1.12 suite. Some
tests were modified to check for the correct implementation of Ada rules in
cases where language rules
changed from @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}.

Unlike more modern tests, legacy tests use an ALL CAPITALS programming style
which is unusual and hard to read (@RefSecNum{General Standards}).
They also use the naming conventions of
early ACVC versions (see @RefSecNum{Legacy Naming}). @i{Modern tests}@Defn{modern tests}
(those created for the ACATS more recently than ACVC 1.12) use a more typical
Mixed Case programming style (see @RefSecNum{General Standards} and use a more
flexible naming scheme (see @RefSecNum{Modern Naming}).

@ToGlossary{Term=[Legacy Tests],Text=[Tests that were included in ACVC 1.12
that have been incorporated into later ACVC and ACATS versions. The vast
majority of these tests check for language features that are upwardly
compatible from @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}
to later versions of Ada. Some of these tests have been
modified from the ACVC 1.12 versions to ensure that Ada rules are properly
implemented in cases where there were extensions or incompatibilities from
@LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}} to later versions
of Ada.]}

@ToGlossary{Term=[Modern Tests],Text=[Tests that have been constructed and
added to the ACATS since the release of ACVC 1.12. These tests usually test
features added to Ada since
@LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}. Modern tests have a
coding style more like that used by typical programmers than the Legacy tests,
and have a different naming convention.]}


@LabeledSubClause{Test Foundation Code}

Some tests use @i{foundation code}.@Defn{foundation code}
Foundation code is reusable across multiple
tests that are themselves independent of each other. It is intended to be
compiled and included in an environment as part of the compilation process of a
test. If the test is executable, the foundation code must be bound with all
other code for the test prior to execution.

Foundation code is always expected to compile successfully; it is never
expected to be run by itself. Foundation code is not, in and of itself, a test,
and is therefore not characterized by a test class (see @RefSecNum{Test Classes}).
One may think of
it as providing some utility definitions and routines to a number of different
tests. Names of foundation units (and therefore names of files containing
foundation code) are distinguished as described in Naming Convention, Section
@RefSecNum{Naming Convention}.

@ToGlossary{Term=[Foundation Code],Text=[Code used by multiple tests;
foundation code is designed to be reusable. Generally a foundation is a package
containing types, variables, and subprograms that are applicable and useful to
a series of related tests. Foundation code is never expected to cause compile
time errors. It may be compiled once for all tests that use it or recompiled
for each test that uses it; it must be bound with each test that uses it.]}


@LabeledSubClause{Special Core Tests}


This section identifies tests that appear in the Core (since their requirements
are enunciated there) but that may be graded as non-supported for
implementations not claiming support of certain Specialized Needs Annexes.

@i{Annex C Requirements}

Clause 13 of the Ada Standard
includes implementation advice paragraphs include the words "recommended level
of support". The ACATS does not require implementations to conform to those
paragraphs unless they claim support for Annex C, Systems Programming (because
of
@URLLink{URL=[http://www.adaic.org/resources/add_content/standards/12rm/html/RM-C-2.html#p2],Text=[C.2(2)]}:
"The implementation shall support at least the functionality defined
by the recommended levels of support in Clause 13.")

@leading@;Tests that check conformance to the implementation advice are listed
below:

@begin{FourCol}
CD10001@*
CD20001@*
CD30001@*
CD30002@*
CD30003@*
CD30004@*
CD30006@*
CD30007@*
CD30008@*
CD30009@*
CD40001@*
CD72A01@*
CD72A02@*
CD90001@*
CDE0002
@end{FourCol}

Implementations that claim support for Annex C are required to process and pass
the tests listed above.

Implementations that do not claim support for Annex C are still
required to process these tests. Such implementations may reject the lines
marked with the special comment @exam{-- ANX-C RQMT}, in which case the test will
be graded as "unsupported". If an implementation accepts such lines in one of
these tests, then the test must be bound (linked) and executed, with a passed
or not_applicable result.


@LabeledSubClause{Foreign Language Code}

Several tests for Annex B features (and one Clause 13 test) include files
containing non-Ada code (Fortran, C, Cobol). These tests must be compiled,
bound, and run by implementations that support foreign language interfaces to
the respective non-Ada language. The foreign language code uses only the most
basic language semantics and should be compilable by all Fortran, C, and Cobol
compilers, respectively. In cases where a foreign language does not accept the
code as provided, modifications are allowable. See Section
@RefSecNum{Allowed Test Modifications}.

Files that contain foreign code are identified by a special file extension. See
Section @RefSecNum{Modern Naming}.

The tests that include Fortran code are: CXB5004 and CXB5005

The tests that include C code are: CD30005, CXB3004, CXB3006, CXB3013, CXB3017,
CXB3018, CXB3023, and CXB3024

The test that includes Cobol code is: CXB4009


@LabeledClause{Test Classes}

There are six different classes of ACATS tests, reflecting different testing
requirements of language conformity testing. Each test belongs to exactly one
of the six classes, and its membership is encoded in the test name, as
explained later. The purpose and nature of each test category is explained
below. The test classifications provide an initial indication of the criteria
that are used to determine whether a test has been passed or failed.


@LabeledSubClause{Class A}

Class A tests check for acceptance (compilation) of language constructs that
are expected to compile without error.

An implementation passes a class A test if the test compiles, binds, and
executes reporting "PASSED". Any other behavior is a failure.

Only legacy tests are included in this class.


@LabeledSubClause{Class B}

Class B tests check that illegal constructs are recognized and treated as fatal
errors. They are not expected to successfully compile, bind, or execute. Lines
that contain errors are marked @exam{-- ERROR:} and generally include a brief
description of the illegality on the same or following line. (The flag includes
a final ":" so that search programs can easily distinguish it from other
occurrences of the word "error" in the test code or documentation.) Some tests
also mark some lines as @exam{-- OK}, indicating that the line must not be flagged
as an error.

An implementation passes a class B test if each indicated error in the test is
detected and reported, and no other errors are reported. The test fails if one
or more of the indicated errors are not reported, or if an error is reported
that cannot be associated with one of the indicated errors. If the test
structure is such that a compiler cannot recover sufficiently to identify all
errors, it may be permissible to "split" the test program into separate units
for re-processing (see Section @RefSecNum{Allowed Test Modifications}
for instructions on modifying tests).

In some cases and for some constructs, compilers may adopt various error
handling and reporting strategies. In cases where the test designers determined
that an error might or might not be reported, but that an error report would be
appropriate, the line is marked with @exam{-- OPTIONAL ERROR:} or a similar phrase.
In such cases, an implementation is allowed to report an error or fail to
report an error without affecting the final grade of the test.

Similarly, in cases where the test designers determined that an error might
be reported at one of several source locations, all such source locations
are marked with @exam{-- POSSIBLE ERROR:} and an indication of which error (if
the test contains several) is expected. In such cases, an implementation is
considered passing if it reports an error at any of the possible places for
the error to be reported. It fails if no error is reported at any of the
places.


@LabeledSubClause{Class C}

Class C tests check that executable constructs are implemented correctly and
produce expected results. These tests are expected to compile, bind, execute
and report "PASSED" or "NOT-APPLICABLE". Each class C test reports "PASSED",
"NOT-APPLICABLE", or "FAILED" based on the results of the conditions tested.

An implementation passes a class C test if it compiles, binds, executes, and
reports "PASSED". It fails if it does not successfully compile or bind, if it
fails to complete execution (hangs or crashes), if the reported result is
"FAILED", or if it does not produce a complete output report.

The tests CZ1101A, CZ1102A, CZ1103A, and CZ00004 are treated separately, as
described in Section @RefSecNum{"CZ" Acceptance Tests}.


@LabeledSubClause{Class D}

Class D tests check that implementations perform exact arithmetic on large
literal numbers. These tests are expected to compile, bind, execute and report
"PASSED". Each test reports "PASSED" or "FAILED" based on the conditions
tested. Some implementations may report errors at compile time for some of
them, if the literal numbers exceed compiler limits.

An implementation passes a class D test if it compiles, binds, executes, and
reports "PASSED". It passes if the compiler issues an appropriate error message
because a capacity limit has been exceeded. It fails if does not report
"PASSED" unless a capacity limits is exceeded. It fails if it does not
successfully compile (subject to the above caveat) or bind, if it fails to
complete execution (hangs or crashes), if the reported result is "FAILED", or
if it does not produce an output report or only partially produces one.

Only legacy tests are included in this class.


@LabeledSubClause{Class E}

Class E tests check for constructs that may require inspection to verify. They
have special grading criteria that are stated within the test source. They are
generally expected to compile, bind and execute successfully, but some
implementations may report errors at compile time for some tests. The
"TENTATIVELY PASSED" message indicates special conditions that must be checked
to determine whether the test is passed.

An implementation passes a class E test if it reports "TENTATIVELY PASSED", and
the special conditions noted in the test are satisfied. It also passes if there
is a compile time error reported that satisfies the special conditions. Class E
tests fail if the grading criteria in the test source are not satisfied, or if
they fail to complete execution (hang or crash), if the reported result is
"FAILED", or if they do not produce a complete output report.

Only legacy tests are included in this class.


@LabeledSubClause{Class L}

Class L tests check that all library unit dependences within a program are
satisfied before the program can be bound and executed, that circularity
among units is
detected, or that pragmas that apply to an entire partition are correctly
processed. These tests are normally expected to compile successfully but not to
bind or execute. Some implementations may report errors at compile time;
potentially illegal constructs are flagged with "-- ERROR:". Some class L
tests indicate where bind errors are expected. Successful processing does not
require that a binder match error messages with these indications.

An implementation passes a class L test if does not successfully complete the
bind phase. It passes a class L test if it detects an error and issues a
compile time error message. It fails if the test successfully binds and/or
begins execution. An L test need not report "FAILED" (although many do if they
execute).

As with B-tests, the test designers determined that some constructs may or may
not generate an error report, and that either behavior would be appropriate.
Such lines are marked with "-- OPTIONAL ERROR:" In such cases, an
implementation is allowed to report an error or fail to report an error. If an
error is reported at compile time, the binder need not be invoked. If no errors
are reported at compile time, the binder must be invoked and must not
successfully complete the bind phase (as indicated by the inability to begin
execution).


@LabeledSubClause{Foundation Code}

Files containing foundation code are named using the regular test name
conventions (see Section @RefSecNum{Naming Convention}). It may appear from
their names that they represent class F tests. There is no such test class.
Foundation code is only used to build other tests, so foundation units are not
graded. However, if a foundation unit fails to compile, then the tests that
depend on it cannot be compiled, and therefore will be graded as failed.


@LabeledSubClause{Specialized Needs Annex Tests}

Specialized Needs Annex tests have no separate classifications and are
classified in the same way as all other tests. There are Class B, Class C, and
Class L SNA tests.@SeeOther{Primary=[SNA],Other=[specialized needs annex]}


@LabeledClause{Naming Convention}

This section describes the naming conventions used in ACATS 4.1, specifically
as they apply to files. All file names are of the form <name>.<type>, where
<type> is a one, two, or three character extension. File names indicate test
class, compilation order (if applicable), and whether the test is
implementation dependent or requires customization. When a test is included in
a single file, <name> duplicates the test name. The same is true of a
foundation. In multiple file tests, the first 7 characters of the file <name>
are normally the same as the name of the test, however in some cases, the
structure of the test requires that the file name be different from the Ada
unit. The application of the conventions to tests is straightforward.

There are two different but similar naming conventions used in ACATS 4.1
Legacy tests use the naming conventions of early ACVC versions. Tests new since
ACVC 1.12 use the modern convention. The conventions are consistently
distinguishable at the 7th character of the name: legacy names have a letter in
the 7th position, whereas newer (modern) names have a digit.


@LabeledSubClause{Legacy Naming}

@leading@;The name of a legacy test is composed of seven or eight characters.
Each character position serves a specific purpose as described in the table
below. The first column identifies the character position(s) starting from the
left, the second column gives the kind of character allowed, and the third
gives the corresponding meaning:

@table{Columns=[3],Alignment=[AllLeft],FirstColWidth=[1],LastColWidth=[3],
NoBreak=[T],Border=[F],SmallSize=[F],
Caption=[],
Headers=[@b{Position}@\@b{Kind}@\@b{Meaning}],
Body=[1@\Letter@\Test class (see Section @RefSecNum{Test Classes})
2@\Hexadecimal@\AIG chapter containing the test objective
3@\Hexadecimal@\Section within the above AIG chapter
4@\Alphanumeric@\Sub-section of the above AIG section
5-6@\Decimal@\Number of the test objective within the above sub-section
7@\Letter@\Letter identifier of the sub-objective of the above objective.
8@\Alphanumeric@\@i{optional} @en Compilation sequence identifier @em indicates the compilation order of multiple files that make up a single test. This position is used only if the test comprises multiple files.]}

The convention is illustrated below.

@PictureAlone{Alignment=[Center],
Border=[None], Height=[180],Width=[345],Name=[LEG-NAME.PNG],
Descr=[Legacy File Name Convention]}
@Center{@Shrink{Legacy File Name Convention}}

In multiple file tests, the intended order of compilation is indicated by a
numeral at position 8. The first file to be compiled has '0', the second has
'1', and so forth.

The chapter and section numbers of the AIG (ACVC Implementer's Guide)
correspond to those in
@LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}.
@Defn{ACVC Implementer's Guide}

Note: The use of a ninth character ('m') to indicate the file containing the
main subprogram has been discontinued. The following table lists the files
containing the main subprograms of the legacy multiple file tests.

@begin{FourCol}
AD7001C0.ADA@*
AD7001D0.ADA@*
B38103C3.ADA@*
B38103E0.ADA@*
B63009C3.ADA@*
B73004B0.ADA@*
B83003B0.ADA@*
B83004B0.ADA@*
B83004C2.ADA@*
B83004D0.ADA@*
B83024F0.ADA@*
B83E01E0.ADA@*
B83E01F0.ADA@*
B86001A1.ADA@*
B95020B2.ADA@*
BA1001A0.ADA@*
BA1010A0.ADA@*
BA1010B0.ADA@*
BA1010C0.ADA@*
BA1010D0.ADA@*
BA1010E0.ADA@*
BA1010F0.ADA@*
BA1010G0.ADA@*
BA1010H0.ADA@*
BA1010I0.ADA@*
BA1010J0.ADA@*
BA1010K0.ADA@*
BA1010L0.ADA@*
BA1010M0.ADA@*
BA1010N0.ADA@*
BA1010P0.ADA@*
BA1010Q0.ADA@*
BA1011B0.ADA@*
BA1011C0.ADA@*
BA1020A0.ADA@*
BA1020B6.ADA@*
BA1020C0.ADA@*
BA1020F2.ADA@*
BA1101B0.ADA@*
BA1101C2.ADA@*
BA1109A2.ADA@*
BA1110A1.ADA@*
BA2001F0.ADA@*
BA2003B0.ADA@*
BA2011A1.ADA@*
BA3001A0.ADA@*
BA3001B0.ADA@*
BA3001C0.ADA@*
BA3001E0.ADA@*
BA3001F0.ADA@*
BA3006A6.ADA@*
BA3006B4.ADA@*
C38108C1.ADA@*
C38108D0.ADA@*
C39006C0.ADA@*
C39006F3.ADA@*
C64005D0.ADA@*
C83022G0.ADA@*
C83024E1.ADA@*
C83F01C2.ADA@*
C83F01D0.ADA@*
C83F03C2.ADA@*
C83F03D0.ADA@*
C86004B2.ADA@*
C86004C2.ADA@*
CA1011A6.ADA@*
CA1012A4.ADA@*
CA1012B4.ADA@*
CA1013A6.ADA@*
CA1014A0.ADA@*
CA1020E3.ADA@*
CA1022A6.ADA@*
CA1102A2.ADA@*
CA2001H3.ADA@*
CA2002A0.ADA@*
CA2003A0.ADA@*
CA2004A0.ADA@*
CA2007A0.ADA@*
CA2008A0.ADA@*
CA2009C0.ADA@*
CA2009F0.ADA@*
CA3011A4.ADA@*
CA5003A6.ADA@*
CA5003B5.ADA@*
CA5004B2.ADA@*
CC3019B2.ADA@*
CC3019C2.ADA@*
LA5001A7.ADA@*
LA5007A1.ADA@*
LA5007B1.ADA@*
LA5007C1.ADA@*
LA5007D1.ADA@*
LA5007E1.ADA@*
LA5007F1.ADA@*
LA5007G1.ADA@*
LA5008A1.ADA@*
LA5008B1.ADA@*
LA5008C1.ADA@*
LA5008D1.ADA@*
LA5008E1.ADA@*
LA5008F1.ADA@*
LA5008G1.ADA
@end{FourCol}

@begin{WideAbove}
@leading@;The file name extension is three characters long. There are four
extensions:
@end{WideAbove}

@begin{Description}
.ada@\A file that contains only Ada code. It does not require any
pre-processing to create a compilable test. It will be submitted directly to
the implementation for determination of test results. All implementations must
correctly process these tests.

.dep@\A file that has a test involving implementation-dependent features of the
language. These tests may not apply to all implementations.

.tst@\A file that has "code" that is not quite Ada; it contains "macro" symbols
to be replaced by implementation-dependent values, and it must be customized
(macro expanded) to prepare it for compilation
(see Section @RefSecNum{Macro Defs Customization}). Once
customized, the resulting test must be processed as indicated by its class.

.adt@\A file that has been modified by the macro processor. It contains only
Ada code and may be submitted to the implementation for results. All
implementations must correctly process these tests. There are no files in the
ACATS distribution with this extension; they are only produced as the output of
the macro processor.
@end{Description}

Modern tests use different file name extensions (see @RefSecNum{Modern Naming}).


@begin{Itemize}
@Noprefix@red{@i{Note that legacy tests have not been renamed for ACATS 4.1.
Since @LocalLink{Target=[Ada2012],Sec=[References],Text={[Ada2012]}} includes some
organizational differences from
@LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}, this means that the
name of a legacy test sometimes will not correspond to the clause of
@LocalLink{Target=[Ada2012],Sec=[References],Text={[Ada2012]}} in which the
tested feature is described.}}
@end{Itemize}


@LabeledSubClause{Modern Naming}

@leading@;The name of a modern ACATS test is composed of seven or eight
characters. Foundation code has a name composed of seven characters. The use of
each character position is described below. The first column indicates the
character position(s) starting from the left, and the second column indicates
the kind of character allowed, and the third column gives the corresponding
meaning:

@table{Columns=[3],Alignment=[AllLeft],FirstColWidth=[1],LastColWidth=[3],
NoBreak=[T],Border=[F],SmallSize=[F],
Caption=[],
Headers=[@b{Position}@\@b{Kind}@\@b{Meaning}],
Body=[1@\Letter@\Test class; foundations are marked 'F'.
2@\Alphanumeric@\If other than an 'x', the clause of the Ada Standard describing the feature under test. An 'x' indicates that the test includes one or more features from an annex of the Ada Standard.
3@\Alphanumeric@\Core subclause or annex letter identifier (either core or Specialized Needs Annex); clauses are a hexadecimal value.
4@\Alphanumeric@\Sub-subclause (if a core test), or subclause (if an annex test); a number if less than 10, otherwise a letter with 10='A', 11='B', and so on.
5@\Alphanumeric@\Foundation identifier (alphabetic, unless no foundation is required, in which case a '0').
6-7@\Decimal@\Sequence number of this test in a series of tests for the same clause; foundation code will have "00".
8@\Alphanumeric@\@i{optional} @en Compilation sequence identifier @em indicates the suggested or required compilation order of multiple files that make up a single test (0 is compiled first). This position is used only if the test comprises multiple files.]}

(Note: Formally groupings for all levels below the top-level grouping are known as subclauses; here we use subclause to specifically refer to the second level and sub-subclause to refer to the third level.)

The convention is illustrated below.

@PictureAlone{Alignment=[Center],
Border=[None], Height=[192],Width=[342],Name=[MOD-NAME.PNG],
Descr=[Modern File Name Convention]}
@Center{@Shrink{Modern File Name Convention}}

@leading@;The file name extension is a one or two character file name
extension. There are six extensions:

@begin{Description}
.a@\A file that contains only Ada code (except for configuration pragmas in the
case of some Specialized Needs Annex tests). It does not require any processing
to prepare it for compilation (unless configuration pragmas must be handled
separately). It is normally submitted directly to the implementation for
determination of test results.

.am@\A file that contains the main subprogram for a multi-file test. Generally,
this extension is used for only one file of a test. In rare cases (some Annex E
tests), a multi-file test may have more than one file containing a "main"
subprogram; in such cases, the correct testing procedure is described in the
Special Requirements section of the test prologue.

.au@\A file that contains only Ada code that contains characters outside of
the 7-bit ASCII character set. These files are provided in UTF-8 format with
a starting byte-order mark. For ACATS 4.1, these tests must
be compiled and run as all other tests of its test class, although
usage of a different workflow (which must be documented if it is necessary)
is allowed. (Note that @LocalLink{Target=[Ada2012],Sec=[References],Text={[Ada2012]}}
requires compilers to be able to process UTF-8 files, although the details
[such as compiler options] might be different than ASCII source files.)

.ftn@\A file that contains Fortran language code and must be compiled by a
Fortran compiler. These files are used by tests that check a foreign language
interface to Fortran.

.c@\A file that contains C language code and must be compiled by a C compiler.
These files are used by tests that check a foreign language interface to C.

.cbl@\A file that contains Cobol language code and must be compiled by a Cobol
compiler. These files are used by tests that check a foreign language interface
to Cobol.
@end{Description}

A test that depends on foundation code has an alphabetic character in the fifth
position of its name. The required foundation will have the same characters in
the second through fifth positions of its name. For example, C@b{123A}xx
depends on F@b{123A}00.


@LabeledSubClause{Multiple File Tests}

When tests are contained in multiple files (i.e., compilation units are
contained in different files), the file names are related. The first seven
positions of the names of all the files (other than foundation files) comprised
by a single test will be identical. The eighth position will provide a
distinguishing alphanumeric which indicates the required compilation order. In
legacy tests, the main subprogram is not indicated (see the table in section
@RefSecNum{Legacy Naming} for files containing main subprograms). For newer
(modern) tests, the extension ".am" indicates the file with the main subprogram.

All tests apply the convention of naming the main subprogram the same as the
file (excluding the file extension) plus, for legacy tests only, the
letter 'm'. For example, the legacy test, C39006F, is contained in four files,
named c39006f0.ada, c39006f1.ada, c39006f2.ada, and c39006f3.ada. The main
subprogram of the test is contained in c39006f3.ada and is named @exam{C390006F3M}.
The test C390006 is also contained in four files, named c3900060.a, c3900061.a,
c3900062.a, and c3900063.am. The main subprogram of the test is contained in
c3900063.am and is named @exam{C3900063}.

Unless otherwise required by a test objective, other library units in a test
are named with the test name and a suffix. Typically, the suffix will be
a number or an underscore followed by a few letters. Similarly, library
units making up a foundation are usually named with the foundation name
(or the foundation name and a suffix if there are multiple units in the
foundation). This convention reduces name collisions with other tests
and with implementation-defined units.

There are a small number of Specialized Needs Annex tests for the Distributed
Processing Annex that require two active partitions and have two main
subprograms. These tests have two files with the .am extension to signify the
location of the (multiple) main subprograms.


@LabeledClause{Test Program Format}

Each test file is composed of a test prologue, documenting the test, and the
test code proper. All prologue lines are marked as comments. (The prologue in
files containing non-Ada code is marked according to the comment conventions of
the foreign language.)

The prologue for all tests is based on that of legacy tests. Legacy tests are
generally, but not entirely, consistent in their use of the prologue. The
format of the prologue between test files and foundation files is slightly
different.

@leading@;The general format of the prologue is as follows:
@begin{Indent}
@begin{Description}
<file name>@\The distribution name of the file containing this prologue.

@exam{DISCLAIMER}@\Use restrictions for ACATS tests; included in all tests.

@exam{OBJECTIVE}@\A statement of the test objective; included in all tests.

@exam{TEST DESCRIPTION}@\A short description of the design or strategy of
the test or other pertinent information. Included in most newer tests but not
generally included in legacy tests.

@exam{SPECIAL REQUIREMENTS}@\@i{optional} @en Included if the test has any
special requirements for processing. Normally, this section will be found only
in Specialized Needs Annex tests. For example, an Annex E test may check for
the correct implementation of partitions; the requirements for test
partitioning and what to use as a main subprogram in each partition would be
documented in this section.

@exam{TEST FILES}@\@i{optional} @en Included if the test depends on multiple
files; identifies the component files of a multi-file test.

@exam{APPLICABILITY CRITERIA}@\@i{optional} @en Specifies the conditions
under which the test can be ruled inapplicable.

@exam{PASS/FAIL CRITERIA}@\@i{optional} @en Explains how to interpret
compilation, binding, and/or run-time results for grading the test.

@exam{MACRO SUBSTITUTIONS}@\@i{optional} @en Identifies the macro symbol(s)
in the file that must be replaced and provides a brief description of what the
replacement(s) represent. Appears only in legacy tests.

@exam{CHANGE HISTORY}@\History of the test file. Included in all tests.

@end{Description}
@end{Indent}

All tests have the line immediately after the disclaimer marked @exam{--*}.
Modern tests have the line after the last prologue line (before the first
line of executable code) marked @exam{--!}  No other comment lines are marked
with those conventions, so the start of the objective (which is the next line
after the disclaimer) and the first line of code may be found
quickly with an editor search.

Some tests are composed of multiple files (other than foundation code). Rather
than repeating the complete prologue in each file, an alternate approach has
been used. One file (usually the one containing the main subprogram or
the first file in the set) has the complete prologue; the other, related files
have those sections that apply to files (TEST FILES,
CHANGE HISTORY) and refer to the file with the complete prologue for
the other sections.

@LabeledClause{General Standards}

ACATS tests were developed to a general set of standards. To promote a variety
of code styles and usage idioms in the tests, standards were not necessarily
rigorously enforced but were used as guidelines for test writers. A maximum
line length of 79 characters was used to enhance electronic distribution of
tests (except when specific testing requirements dictated otherwise, usually in
.dep and .tst files). Tests tend to be about 120 executable lines long, though
many tests deviate from this norm (either longer or shorter) to achieve a
design that focuses on the objective and a readable, maintainable test.
Sometimes complex objectives have been divided into sub-objectives to achieve
complete coverage in comprehensible, maintainable tests. Some tests check
multiple objectives; in other cases, sub-objectives are checked in separate
tests.

Legacy tests use only the basic 55-character set (26 capital letters, 10
digits, and 19 punctuation marks). Unless there is a specific test requirement,
numeric values are in the range (-2048..2047), which can be represented in 12
bits. Numeric values are generally in the range (-128..127). Modern tests
use both upper and lower case letters and may use larger numeric values (but
within the range (-65536..65535) except in rare cases).

Legacy tests tend to use as few Ada features as necessary to write a
self-checking executable test that can be read and maintained. Modern tests tend
to exhibit a usage-oriented style, employing a rich assortment and interaction
of features and exemplifying the kind of code styles and idioms that compilers
may encounter in practice.

In modern tests, Ada reserved words are entirely in lower case. Identifiers
normally have their initial letter capitalized. Every attempt has been made to
choose meaningful identifiers. In B class tests, identifier names often provide
a clue to the specific case or situation under test. In C class tests,
identifiers are normally chosen to help document the test design or the intent
of the code.

Modern executable tests generally provide some visual separation of those
test elements that focus on conformance issues from those that govern the flow
of a test. For example, there is frequently a need to establish preconditions
for a test and examine post-conditions after a section of test code has
executed. To distinguish between constructs (types, objects, etc.) that are
part of the test code and those that are artifacts of the testing process
(e.g., pre-, post-conditions), the latter have @exam{TC_} prefixed to the
identifier name. This prefix is shorthand for @exam{Test_Control}.


@LabeledClause{Test Structure}

@leading@;Executable tests (class A, C, D, and E) generally use the following
format:
@begin{Example}
@key[with] Report;
@key[procedure] Testname @key[is]
   @Examcom{<declarations>}
@key[begin]
   Report.Test ("Testname", "Description ...");
   @examcom{...}
   @Examcom{<test situation yielding result>}
   @key[if] Post_Condition /= Correct_Value @key[then]
      Report.Failed ("Reason");
   @key[end if];
   @examcom{...}
   Report.Result;
@key[end] Testname;
@end{Example}

The initial call to Report.Test prints the test objective using Text_IO output
(unless the body of Report has been modified to do something else).
After each section of test code, there is normally a check of post conditions.
The if statement in this skeleton is such a check; unexpected results produce a
call to Report.Failed. The sequence of test code / check of results may be
repeated several times in a single test. Finally, there is a call to
Report.Result that will print the test result to Text_IO output. Often, but not
always, this structure is enclosed in a declare block.

One or more calls to Report.Failed will report a result of "FAILED" and a brief
suggestion of the likely reason for that result.

@leading@;More complex tests may include calls to Report.Failed in the code
other than in the main program, and therefore exhibit the following format for
the main procedure:
@begin{Example}
@key[with] Report;
@key[procedure] Testname @key[is]
   @examcom{<declarations>}
begin
   Report.Test ("Testname", "Description ...");
   @examcom{...}
   Subtest_Call;
   @examcom{...}
   Report.Result;
@key[end] Testname;
@end{Example}

Fail conditions are detected in subprograms (or tasks) and Report.Failed is
called within them.

Occasionally, as a test is running, it will determine that it is not
applicable. In such a case, it will call Report.Not_Applicable that will report
a result of "NOT_APPLICABLE" (unless there is also a call to Report.Failed).

Often, a test calls one of the functions Report.Ident_Int or Report.Ident_Bool
to obtain a value that could be provided as a literal. These functions are
intended to prevent optimizers from eliminating certain sections of test code.
The ACATS suite has no intention of trying to discourage the application of
optimizer technology, however satisfactory testing of language features often
requires the presence and execution of specific lines of test code.
Report.Ident_Int and Report.Ident_Bool are structured so that they can be
modified when needed to defeat optimizer advances.

Class B tests may be structured differently. Since they are not executable,
they normally do not include calls to Report.Test or Report.Result (since those
lines of code would have no output effect). Instead, intentional errors are
coded that invoke specific legality rules. The source code includes comments
that document expected compiler results. Legal constructs may also be included
in B class tests. Constructs that are allowed by the legality rules are marked
@Exam{-- OK}; constructs that are disallowed are marked @Exam{-- ERROR:}.
(Some additional markings can also be used, see @RefSecNum{Class B}.)
There is usually a brief indication of the nature of an intentional error on
the same line or the line following a comment. The indications of expected
results are approximately right justified to the code file margin, about column
79, for quick visual identification.

Class L tests are multifile tests with illegalities that should be detected at
bind time. They are generally structured like class C tests, often with calls
to Report.Test and Report.Result, but they are not expected to execute.


@LabeledClause{Delivery Directory Structure}

The delivery of ACATS tests is structured into a directory tree that reflects
the organization of the test suite and support code.

The top-level directory contains the support subdirectory, the docs
subdirectory, and a subdirectory for each major grouping of tests. The support
subdirectory contains all support packages (Report, ImpDef, TCTouch) and the
source code for all test processing tools (Macro expander, Wide Character
processor). Each of the other subdirectories contains all tests that begin with
the indicated prefix. For example, all of the B2* tests are in the @exam{b2}
subdirectory; all of the CXH* tests are in the @exam{cxh} subdirectory. Note
that all of the A* tests are in the @exam{a} directory, all of the D* tests
are included in the @exam{d} subdirectory, and all of the E* tests
are included in the @exam{e} subdirectory. The
@exam{l} directory contains the L tests for the core; other L tests are in
directories named with three letters, indicating the class (l) and the
Specialized Needs Annex to which the tests apply.

Subdirectories that would be empty are not stubbed.

The following figure sketches this scheme, but does not show complete detail. A
list of all subdirectories is included in
Section @RefSecNum{Guide to Decompressing Files}.

@PictureAlone{Alignment=[Center],
Border=[None], Height=[175],Width=[541],Name=[DIRS.PNG],
Descr=[Delivery Directory Structure]}
@Center{@Shrink{Delivery Directory Structure}}


@LabeledClause{File Format}

To conserve space and ease downloading, all files in the delivered ACATS 4.1
(including test files, foundation files, and support files) have been
compressed. Except as noted below, decompressed files
(see Section @RefSecNum{Guide to Decompressing Files}) use only
ASCII characters. A few tests use Unicode characters; these are indicated
by an @exam{.au} extension. Some of the documentation
files are provided in PDF and/or HTML forms for greater readability. (The HTML
documentation files include GIF and PNG graphics files.) Other than
the documentation files, no formatting control characters, rulers or other
information intended for editors or display programs are included in the files.

Files with the .zip extension have been compressed using a DOS zip utility;
files with the .Z extension have been first put in Unix tar format and then
compressed with Unix compress.
