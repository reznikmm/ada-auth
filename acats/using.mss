@Part(using, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/using.mss,v $}
@comment{$Revision: 1.7 $ $Date: 2015/01/15 02:30:35 $}

@LabeledSection{Using the ACATS}

There are eight major steps involved in using the ACATS test suite; two of them
are sometimes not required. The steps are: installing the software, tailoring
the software, processing the support files, establishing command scripts,
processing the ACATS tests, grading the test results, addressing problems (if
necessary), and reprocessing problem tests (if necessary). The first six of
these tasks must be completed successfully to accomplish a test run. The first
four normally need be completed only once for each ACATS release. Each step is
explained in the following sections. The flow from one to the next is
illustrated in the following figures.@*@Comment{Extra blank line to provide some space above the picture}

@PictureAlone{Alignment=[Center],
Border=[None], Height=[508],Width=[363],Name=[USAGE-1.PNG],
Descr=[Using the ACATS, part 1]}
@Center{@Shrink{Using the ACATS}}

@PictureAlone{Alignment=[Center],
Border=[None], Height=[372],Width=[313],Name=[USAGE-2.PNG],
Descr=[Using the ACATS, part 2]}
@Center{@Shrink{Using the ACATS (cont.)}}


@LabeledClause{Installation of the ACATS Test Suite}

The ACATS test suite must be unloaded from the delivery medium or downloaded
from a delivery site before it can be unpacked, customized for an
implementation, run, and graded.


@LabeledSubClause{Contents of the ACATS Delivery}

The delivery consists of 1 ZIP archive (set of compressed files) or 1
compressed tar file. Each ZIP archive or compressed tar file contains
compressed versions of ACATS software (test, foundation, and/or support code)
structured into a directory tree. Files must be extracted from the archives.
The archive contents is described later in this section.

Usually, some test errors will be noted in the test suite. If possible, the
ACAA will correct the errors and issue a corrected test. If a correction is not
possible, the test will be withdrawn. Withdrawn tests are not used in
conformity assessments. For a period after the issuance of a corrected test,
either the original or the corrected test can be used for conformity
assessment. See the ACAA's procedures
@LocalLink{Target=[Pro31],Sec=[References],Text={[Pro31]}} for details.

The ACAA also will issue new tests periodically. As with modified tests, new
tests must be available for a period of time before they are required in
conformity assessments.

These changes to the issued ACATS are documented in the ACATS Modification List
(AML).@Defn{ACATS Modification List}@Defn{AML} This list includes a list of all
new tests, all modified tests, and all withdrawn tests, and an indication as to
when each will be (or is) required for conformity assessments. Each version of
the modification list is given a suffix letter. A ZIP archive and tar file
containing the new and/or modified tests is available. The files are named
MOD_4_0x, where 'x' represents the suffix letter for the AML version.

@leading@;These files can be found on the ACAA's web site:
@begin{Indent}
@trailing@;@shrink{@URLLink{URL=[http://www.ada-auth.org/acats.html],Text=[www.ada-auth.org/acats.html]}}.
@end{Indent}

@leading@;The AML is also distributed by e-mail. To receive these lists, join
the ACAA mailing list. To do so, simply send a message to
@begin{Indent}
@shrink{@URLLink{URL=[mailto:listserv@ada-auth.org],Text=[listserv@ada-auth.org]}}.
@end{Indent}
@leading@;with a body of
@begin{Indent}
@shrink{Join Acaa}
@end{Indent}


@LabeledSubClause{Guide to Decompressing Files}

The ACATS files are provided in two forms: compressed in zip format and
compressed in Unix compress format. Zipped files are included in a zip archive
(files) with the file extension .zip. A Unix compressed files, with
extension .Z, contains a Unix tar file. This section provides generic
instructions for uncompressing them. These instructions are not the only ways
to uncompress the files; sophisticated users may wish to use their own
procedures.

@leading@;If the instructions below are used, the following subdirectories will
have been created and populated with test files after all decompression:

@begin{FourCol}
@exam{@shrink{./acats4_0/a}}@*
@exam{@shrink{./acats4_0/b2}}@*
@exam{@shrink{./acats4_0/b3}}@*
@exam{@shrink{./acats4_0/b4}}@*
@exam{@shrink{./acats4_0/b5}}@*
@exam{@shrink{./acats4_0/b6}}@*
@exam{@shrink{./acats4_0/b7}}@*
@exam{@shrink{./acats4_0/b8}}@*
@exam{@shrink{./acats4_0/b9}}@*
@exam{@shrink{./acats4_0/ba}}@*
@exam{@shrink{./acats4_0/bb}}@*
@exam{@shrink{./acats4_0/bc}}@*
@exam{@shrink{./acats4_0/bd}}@*
@exam{@shrink{./acats4_0/be}}@*
@exam{@shrink{./acats4_0/bxa}}@*
@exam{@shrink{./acats4_0/c2}}@*
@exam{@shrink{./acats4_0/c3}}@*
@exam{@shrink{./acats4_0/c4}}@*
@exam{@shrink{./acats4_0/c5}}@*
@exam{@shrink{./acats4_0/c6}}@*
@exam{@shrink{./acats4_0/c7}}@*
@exam{@shrink{./acats4_0/c8}}@*
@exam{@shrink{./acats4_0/c9}}@*
@exam{@shrink{./acats4_0/ca}}@*
@exam{@shrink{./acats4_0/cb}}@*
@exam{@shrink{./acats4_0/cc}}@*
@exam{@shrink{./acats4_0/cd}}@*
@exam{@shrink{./acats4_0/ce}}@*
@exam{@shrink{./acats4_0/cxa}}@*
@exam{@shrink{./acats4_0/cxb}}@*
@exam{@shrink{./acats4_0/cz}}@*
@exam{@shrink{./acats4_0/d}}@*
@exam{@shrink{./acats4_0/e}}@*
@exam{@shrink{./acats4_0/l}}@*
@exam{@shrink{./acats4_0/bxc}}@*
@exam{@shrink{./acats4_0/bxd}}@*
@exam{@shrink{./acats4_0/bxe}}@*
@exam{@shrink{./acats4_0/bxf}}@*
@exam{@shrink{./acats4_0/bxh}}@*
@exam{@shrink{./acats4_0/cxc}}@*
@exam{@shrink{./acats4_0/cxd}}@*
@exam{@shrink{./acats4_0/cxe}}@*
@exam{@shrink{./acats4_0/cxf}}@*
@exam{@shrink{./acats4_0/cxg}}@*
@exam{@shrink{./acats4_0/cxh}}@*
@exam{@shrink{./acats4_0/lxd}}@*
@exam{@shrink{./acats4_0/lxe}}@*
@exam{@shrink{./acats4_0/lxh}}@*
@exam{@shrink{./acats4_0/docs}}@*
@exam{@shrink{./acats4_0/support}}@Comment{This one is too long for RTF otherwise}
@end{Fourcol}

@begin{Wideabove}
Note that the names are given here in all lowercase; some systems may create
uppercase names. The path separator, shown here as '/', may also differ.
@end{Wideabove}


@LabeledSubSubClause{Decompressing Zipped Files}

All ACATS files have been compressed (zipped) into compressed archives
(zip-files) that have the MS-DOS file extension ".zip". A Windows command-line
utility was used
to compress them. They must be decompressed before they can be further
processed. A decompression utility is available from the source of the ACATS
distribution. All ACATS 4.0 files may be decompressed using the following
steps. Approximately 48 MB of free space on a Windows machine hard drive will
be required to accomplish the decompression using this technique.

Create a directory on the hard disk to contain ACATS. In these examples, we
assume the name is @Exam{acats4_0}, but any name can be used. Copy the archive
(file with .zip extension) to the hard disk in the new directory. Decompress it
insuring that directories are used. For the @Exam{unzip} program, this is the
default setting. For the @exam{pkunzip} program, this is the -d option. For the
@exam{winzip} program, ensure that "Use Directory Names" is checked. Also,
ensure that the files are decompressed into the proper directory. For command
line decompressors, this means ensuring that the current subdirectory is
acats4_0. For @exam{winzip}, this simply means selecting acats4_0 as the extract
path.

@leading@;For example, using unzip, and assuming that the archive name is ACATS40.zip,
type
@begin{Example}
cd acats4_0
@end{Example}
@leading@;to set the proper directory, and
@begin{Example}
unzip ACATS40
@end{Example}
to extract the files.

The files were compressed on a Windows system, where <CR><LF> is used as a line
terminator. Decompressors for other systems using other line terminators should
be able convert the line terminators. The ACAA has a short Ada program which
converts a file from Windows to Unix format; please send the ACAA mail at
@URLLink{URL=[mailto:agent@ada-auth.org],Text=[agent@ada-auth.org]}
to request it if needed.

After all files have been extracted from the archive, delete the archive file
from the hard disk if you wish to conserve space.

As it decompresses files, @Exam{unzip} will restore the directory structure of
the files, creating all needed subdirectories.

Some users may prefer to work with ACATS files in an alternate directory
structure or none at all. If the @exam{unzip} utility is invoked with the
"-j" option,
all files in the archive will be decompressed and placed in the local working
directory. In other words, none of the above subdirectories will be created.
Since there are too many ACATS files to fit into a root DOS directory, if you
wish to put all files in a single directory, you must first create a
subdirectory (e.g., mkdir \ACATS) and unzip all archives there.


@LabeledSubSubClause{Decompressing Unix Compress Files}

@leading@;All ACATS files have been included in 1 Unix tar format file and then
compressed using the Unix compress utility. To create a set of ACATS files,
first copy the compressed files acats_40.tar.Z from the distribution source to
a hard drive. Uncompress the file with the Unix command
@begin{example}
uncompress acats_40.tar.Z
@end{example}
@leading@;(Note that particular Unix implementations may have different formats
or require specific qualifiers.)  After the ACATS file has been uncompressed,
it must be untarred. Move to the directory where you want the acats4_0
directory to be created and then untar the ACATS files
@begin{example}
tar -xvf <path>/acats_40.tar
@end{example}
where <path> is the location of the uncompressed tar file.

Please note that these are generic instructions and may need to be customized
or modified for specific systems.


@LabeledSubClause{Files With Non-Graphic Characters}

Four ACATS test files contain ASCII non-graphic (control) characters that may
be lost or corrupted in the file transfer and decompression process. The user
must ensure that the proper characters are restored as necessary.

There are also a number of ACATS test files provided in UTF-8 format. These
files have an @exam{.au} file extension. The user must take care that any
processing tools do not corrupt UTF-8 files. (This would be unusual: it is
most likely if a tool does not recognize UTF-8 formatted files and also
modifies Latin-1 characters with codes larger than 127.)

The following
paragraphs describe the four tests with ASCII non-graphic characters.

@LabeledSubSubClause{A22006C}

@leading@;This test checks that format effectors can appear at the beginning of
a compilation. At the beginning of the file, the first line is empty (indicated
by the system's end-of-line marker, which may be a sequence of one or more
characters or may be indicated by some other means). The second line contains
20 characters: 6 control characters followed by the comment delimiter, a space,
and the file name (A22006C.ADA). The control characters are:
@table{Columns=[4],Alignment=[CenterExceptFirst],FirstColWidth=[1],LastColWidth=[1],
NoBreak=[T],Border=[F],SmallSize=[F],
Caption=[],
Headers=[@b{Common Name}@\@b{Ada Name}@\@b{Decimal Value}@\@b{Hex Value}],
Body=[Carriage return@\ASCII.CR@\13@\0D
Carriage return @\ASCII.CR@\13@\0D
Vertical tab@\ASCII.VT@\11@\0B
Line feed@\ASCII.LF@\10@\0A
Line feed@\ASCII.LF@\10@\0A
Form feed@\ASCII.FF@\12@\0C]}


@LabeledSubSubClause{B25002A}

This test checks that ASCII control characters (other than format effectors)
are not permitted in character literals. The expected characters are documented
in source code comments, using the customary 2- or 3-letter mnemonics. The 28
characters are used in their ASCII order, and have ASCII values 0 through 8, 14
through 31, and 127.


@LabeledSubSubClause{B25002B}

This test checks that the five ASCII format effector characters cannot be used
in character literals. There are two groups of code containing the illegal
characters; in each group, the characters appear in the order given
below:
@table{Columns=[4],Alignment=[CenterExceptFirst],FirstColWidth=[1],LastColWidth=[1],
NoBreak=[T],Border=[F],SmallSize=[F],
Caption=[],
Headers=[@b{Common Name}@\@b{Ada Name}@\@b{Decimal Value}@\@b{Hex Value}],
Body=[Horizontal tab@\ASCII.HT@\9@\09
Vertical tab@\ASCII.VT@\11@\0B
Carriage return @\ASCII.CR@\13@\0D
Line feed@\ASCII.LF@\10@\0A
Form feed@\ASCII.FF@\12@\0C]}


@LabeledSubSubClause{B26005A}

This test checks the illegality of using control characters in string literals.
Each string literal (ASCII codes 0 through 31 and 127) is used once, and the
uses appear in ASCII order. Each use is also documented in a source code
comment, which identifies the character by its common 2- or 3-character
mnemonic.


@LabeledClause{Tailoring the ACATS Test Suite}


There are some files in the delivery that require modification before ACATS 4.0
is ready for processing by an Ada implementation. Package ImpDef (impdef.a)
must be edited to include values suitable for proper testing of an
implementation if the defaults are not acceptable. The
macros.dfs file must similarly be edited to include values suitable for
testing. All .tst files (including package Spprt13 (spprt13s.tst)) must have
their macro symbols replaced by implementation specific values. A body for
FcnDecl (fcndecl.ada) must be provided if necessary. Finally, Package Report
(repbody.ada) must be modified if necessary.

The required customization is described in the following sections.

Customizations of these files from previous versions of the ACATS suite
generally can be used with ACATS 4.0, but users should ensure that neither
their requirements nor the underlying files have changed since the
customizations were made.


@LabeledSubClause{ImpDef Customization}

@begin{Indent}
@red{@i{For ACATS 4.0, there are two new parameters in Impdef.
Other than those parameters, there was no change to Impdef or
any of its children from ACATS 3.1 to ACATS 4.0. A version of any of these
packages that was tailored for ACATS 3.1 should be valid for ACATS 4.0 once the
new parameters are defined unless some implementation characteristics have
changed.}}
@end{Indent}

ACATS tests use the entities in ImpDef to control test execution. Much of the
information in ImpDef relates to the timing of running code; for example, the
minimum time required to allow a task switch may be used by a test as a
parameter to a delay statement. The time to use is obtained as an ImpDef
constant.

@exam{impdef.a} was added as a new feature to ACATS 2.0 suite. It is related to
@exam{macro.dfs} in that it must be customized with values specific to an
implementation and ACATS tests will rely on these values. ImpDef is different
in the following respects:

@begin{Itemize}
Defaults are provided. Some implementations may be able to rely entirely on the
default values and subprograms, so no customization would be necessary.

Some implementations may choose to provide bodies for procedures and/or
functions. Bodies so provided must satisfy requirements stated in ImpDef.

Tests depending on Impdef do not need customization (macro substitution).
Instead, ImpDef must be available at compile time (i.e., included in the
environment) for tests that rely upon it. This simplifies the customization
process and management and also is similar to the way that Ada projects
typically manage configuration parameters.
@end{Itemize}

There are child packages of ImpDef for each of the Specialized Needs Annexes.
An implementation that uses one or more of the Specialized Needs Annexes in its
conformity assessment must customize the associated ImpDef child packages (or
rely on their defaults) and must set the appropriate Booleans in impdef.a.
It is not necessary to customize Impdef children for Specialized Needs Annexes
that are not included in a particular conformity assessment.

Specific instructions for the values required by ImpDef and its children are
included in @exam{impdef.a}, @exam{impdefc.a}, @exam{impdefd.a},
@exam{impdefe.a}, @exam{impdefg.a}, and @exam{impdefh.a}.
(Note that @exam{impdefc}, for example, refers to Annex C.) An excerpt from
ImpDef is included in @RefSec{Parameterization Files}.

@begin{Indent}
@red{@i{All implementations must customize @exam{impdef.a} unless they
wish to rely on the defaults provided. ImpDef must be part of the environment
whenever a test that depends on it is processed. Similarly, the child of
Impdef corresponding to each Specialized Needs Annex that the implementer
intends to test during a conformity assessment must be customized and be
part of the environment when the Annex tests are processed.}}
@end{Indent}


@LabeledSubClause{Macro Defs Customization}

@begin{Indent}
@red{@i{
There was no change to the @exam{macro.dfs} file from ACATS 3.1 to ACATS 4.0. A
version of @exam{macro.dfs} that was tailored for ACATS 3.1 should be valid for ACATS
4.0 unless some implementation characteristics have changed.}}
@end{Indent}

Tests in files with the extension @exam{.tst} contain symbols that represent
implementation dependent values. The symbols are identifiers with a initial
dollar sign ('$'). Each symbol must be replaced with an appropriate textual
value to make the tests compilable. This process is sometime known as
@i{macro substitution}.@Defn{macro substitution}

The Macrosub program distributed with the ACATS can automatically perform the
required substitutions. This program reads the replacement values for the
symbols from the file @exam{macro.dfs} and edits all the @exam{.tst} tests in the suite to
make the needed changes. It writes the resulting, compilable programs into
files with the same name as the original but with the extension @exam{.adt}.
A sample @exam{macro.dfs} is included with the ACATS; it contains
descriptions of all the symbols used in the test suite.

@leading@;Substitutions using the Macrosub program may be made as follows:
@begin{Enumerate}
Edit the file @exam{macro.dfs} using values appropriate for the implementation.
Symbols that use the value of MAX_IN_LEN are calculated automatically and need
not be entered.

Create a file called @exam{tsttests.dat} that includes all of the @exam{.tst}
test file names, and their directory locations if necessary. A version of this
file (without directory information) is supplied.

Compile and bind MacroSub.

Run MacroSub.
@end{Enumerate}

The program will replace all symbols in the @exam{.tst} files with values from
@exam{macro.dfs}. Test files with the original test name but the extension
@exam{.adt} will contain the processable tests. The original @exam{.tst} files
will not be modified.


@LabeledSubClause{Packages SPPRT13 and FCNDECL}

Package SPPRT13 declares six constants of type System.Address that are
primarily used by tests of Section 13 features. It is in the file @exam{spprt13s.tst}.
As distributed, the package uses macro symbols that must be replaced. In most
cases, the substitution can be accomplished by the macro substitution described
in the preceding section. If appropriate literals, constants, or predefined
function calls can be used to initialize these constants, they should be
supplied in @exam{macro.dfs}. Otherwise, the package FCNDECL must be modified.

@begin{Indent}
@red{@i{All implementations should verify that package SPPRT13 can be
properly customized using the macro substitution technique. Note that a body
for SPPRT13 is illegal, nor is it allowed to add declarations to
package SPPRT13.}}
@end{Indent}

The specification for package FCNDECL is in the file @exam{fcndecl.ada}. SPPRT13
depends on FCNDECL (in a context clause that both @key[with]s it and @key[use]s
it). As
supplied with the ACATS, FCNDECL is an empty package specification. If
appropriate literals, constants, or predefined function calls cannot be used to
customize the constants declared in SPPRT13, the implementer must declare
appropriate functions in the specification of FCNDECL and provide bodies for
them in a package body or with a pragma Import.

Modifications to FCNDECL must receive advance approval from the ACAL (and, if
necessary, the ACAA) before use in a conformity assessment.


@LabeledSubClause{Modification of Package REPORT}

All executable tests use the Report support package. It contains routines to
automate test result reporting as well as routines designed to prevent
optimizers from removing key sections of test code. The specification of
package Report is in the file @exam{repspec.ada}; the body is
in @exam{repbody.ada}.

Under some conditions, the body of package Report may need to be modified. For
example, the target system for a cross-compiler may require a simpler I/O
package than the standard package Text_IO. In such a case, it may be necessary
to replace the context clause and the I/O procedure names in the body of
Report.

Modifications to Report must receive advance approval from the ACAL (and, if
necessary, the ACAA) before use in a conformity assessment.


@LabeledSubClause{Allowed Test Modifications}

Class B tests have one or more errors that implementations must identify. These
tests are structured such that, normally, implementations can report all
included errors. Occasionally, an implementation will fail to find all errors
in a B-test because it encounters a limit (e.g., error cascading, resulting in
too many error reports) or is unable to recover from an error. In such cases, a
user may split a single B-test into two or more tests. The resulting tests must
contain all of the errors included in the original test, and they must adhere
as closely as possible to the style and content of the original test. Very
often, the only modification needed is to comment out earlier errors so that
later errors can be identified. In some cases, code insertion will be required.
An implementation @i{must} be able to demonstrate that it can detect and report
@i{all} intended B-test errors.

Splits may also be required in executable tests, if, for example, an
implementation capacity limitation is encountered (e.g., a number of generic
instantiations too large for the implementation). In very exceptional cases,
tests may be modified by the addition of an attribute definition clause (to
alter the default size of a collection), or by the addition of an elaboration
Pragma (to force an elaboration order).

Tests that use configuration pragmas (see @RefSecNum{Tests that use Configuration Pragmas})
may require modification since the method of processing configuration pragmas
is implementation dependent.

Some tests include foreign language code (Fortran, C, or COBOL). While the
features used should be acceptable to all Fortran, C, and COBOL
implementations, respectively, some implementations may require modification to
the non-Ada code. Modifications must, of course, preserve the input-output
semantics of the (foreign language) subprogram; otherwise, the ACATS test will
report a failure.

All splits and modifications must be approved in advance by the ACAL (and, if
necessary, the ACAA) before they are used in a conformity assessment. It is the
responsibility of the user to propose a B-test split that satisfies the
intention of the original test. Modified tests should be named by appending an
alphanumeric character to the name of the original test. When possible, line
numbers of the original test should be preserved in the modification.

All tests must be submitted to the compiler as distributed (and customized, if
required). If a test is executable (class A, C, D, E) and compiles
successfully, then it must be run. Modified tests or split tests may be
processed next. Only the results of the modified tests will be graded.

If the ACAA has issued an ACATS Modification List (see
Section @RefSecNum{Contents of the ACATS Delivery}), then the
modified versions of tests with modifications @i{must} be
used.@Defn2{Term=[test],Sec=[modified AML classification]}
Either the original version or the modified version of a test with an
allowed modification may be used.@Defn2{Term=[test],Sec=[allowed modification AML classification]}


@LabeledClause{Processing the Support Files}

After all the files identified in
Section @RefSecNum{Tailoring the ACATS Test Suite}
have been customized as needed and required, the support files can be processed
and the reporting mechanism can be verified.


@LabeledSubClause{Support Files}

@Leading@;The following files are necessary to many of the
@Defn{support files}ACATS tests.
Implementations that maintain program libraries may wish to compile them into
the program library used for conformity assessment:

@table{Columns=[2],Alignment=[AllLeft],FirstColWidth=[1],LastColWidth=[2],
NoBreak=[T],Border=[F],SmallSize=[F],
Caption=[],
Headers=[],
Body=[@exam{repspec.ada}@\@exam{repbody.ada}
@exam{impdef.a}@\@exam{impdefc.a} @i{(if testing Annex C)}
@exam{fcndecl.ada}@\@exam{impdefd.a} @i{(if testing Annex D)}
@exam{checkfil.ada}@\@exam{impdefe.a} @i{(if testing Annex E)}
@exam{lencheck.ada}@\@exam{impdefg.a} @i{(if testing Annex G)}
@exam{enumchek.ada}@\@exam{impdefh.a} @i{(if testing Annex H)}
@exam{tctouch.ada}@\@exam{spprt13s.adt} @i{(after macro substitution)}]}

Depending on local requirements and strategy, it may also be convenient to
compile all foundation code into the program library as well.


@LabeledSubClause{"CZ" Acceptance Tests}

Four tests having names beginning "CZ" are part of the ACATS suite. Unlike
other tests in the suite, they do not focus on Ada language features. Instead,
they are intended primarily to verify that software needed for the correct
execution of the test suite works as expected and required. They check, for
example, to see that package Report and package TCTouch work correctly.

All CZ tests must execute correctly and exhibit the prescribed behavior for a
successful conformity assessment. CZ tests must be processed and run as the
first step of a conformity assessment to ensure correct operation of the
support software.

The acceptance test CZ1101A tests the correct operation of the
reporting facilities of package Report,
including checks that Not_Applicable and Failed calls are
reported properly, and that premature calls cause failure. Therefore, CZ1101A
will print some failure messages when it is executed. The presence of these
messages does @i{not} necessarily mean the test has failed. A listing of the
expected output for CZ1101A is included in @RefSec{Results of CZ Tests}
(times and dates in the actual output will differ).

The acceptance test CZ1102A tests the correct operation of the dynamic value
routines in Report. This test should report "PASSED"; any other result
constitutes a test failure.

The acceptance test CZ1103A ensures the correct operation of procedure
Checkfile. (Some of the executable file I/O tests use a file checking procedure
named Checkfile that determines an implementation's text file characteristics.
The source code for this procedure is in the file checkfil.ada.) CZ1103A
checks whether errors in text files are properly detected, therefore, CZ1103A
will print some failure messages when it is executed. The presence of these
messages does @i{not} necessarily mean the test has failed. A listing of the
expected output for CZ1103A is included in @RefSec{Results of CZ Tests} (times
and dates in the actual output will differ).

The acceptance test CZ00004 produces output that verifies the intent of the
conformity assessment. It relies on ImpDef having been correctly updated for
the conformity assessment and produces output identifying the annexes (if any)
that will be included as part of the conformity assessment. This test also
checks for the proper operation of the TCTouch package, includes checks that
assertion failures are reported properly, therefore CZ00004 will print some
failure messages when it is executed. The presence of these messages does
@i{not} necessarily mean the test has failed. A listing of the expected output
for CZ00004 is included in @RefSec{Results of CZ Tests}; since this output
includes values from the customized impdef, non-failure lines may vary from
those in the expected output. However, the number of lines and their relative
positions should not change (only the contents of the lines can change).


@LabeledClause{Establishing Command Scripts}

Users will often find it convenient to run large numbers of ACATS tests with
command scripts. This section discusses some of the issues to be considered in
developing a script.


@LabeledSubClause{Command Scripts}

All compiler options and switches that are appropriate and necessary to run the
ACATS tests must be identified and included in commands that invoke the
compiler. The same is true for the binder or any other post-compilation tools.
Any implementation dependent processing of partitions, configuration pragmas,
and strict mode processing must be part of the scripts for running tests that
rely on these features.@Defn{command script}@Defn{batch file}

A script should compile (only) all class B tests. It should compile and bind
all class L tests; if link errors are not explicitly given, the script should
attempt to execute the L tests. It should compile all class F files. It should
compile, bind, and execute all class A, C, D, and E tests.

Sample commands for processing the ACATS are a required part of a formal Ada
Comformity Assessment Test Report. If a test report is available for the
implementation being tested, these commands can be used as a guideline for
developing command scripts.@Defn{Ada Conformity Assessment Test Report}


@LabeledSubClause{Dependencies}

A command script must take account of all required dependencies. As noted
earlier, some tests are composed of multiple test files. Also, some tests
include foundation code, which may be used by other tests. If a foundation is
not already in the environment, it must be compiled as part of building the
test. All files that are used in a test must be compiled in the proper order,
as indicated by the file name. For implementations that require the extraction
individual compilation units from test files before submission to the compiler,
the individual units must be submitted to the compiler in the same order in
which they appear in the file.


@LabeledClause{Processing ACATS Tests}

After the ACATS tests and support code has been installed and all required
modifications and preliminary processing have been completed, the suite can be
processed by an implementation. This section describes the tests required for
conformity assessment, required partitioning, how tests may be bundled for
efficiency, and certain processing that may be streamlined. It also describes
how the suite has been organized to allow a user to focus on specific
development needs.


@LabeledSubClause{Required Tests}

An implementation may be tested against the core language only or the core
language plus one or more Specialized Needs Annexes. All core tests (except as
noted in @RefSecNum{Processing that may be Omitted}) must be processed with
acceptable results for conformity assessment of the core language. All legacy
tests, as well as all modern tests for clauses 2-13 and annexes A and B are core
tests. Conformity assessment including one or more Specialized Needs Annexes
requires that all tests for the annex(es) in question be correctly processed in
addition to all core tests.@Defn{core language}

Tests that are not applicable to an implementation (e.g., because of size
limitations) and tests that report "NOT APPLICABLE" when run by an
implementation must nevertheless be processed and demonstrate appropriate
results.@Defn2{Term=[processing],Sec=[inapplicable tests]}

Tests that are withdrawn on the current ACATS Modification List as maintained
by the ACAA need not be processed.@Defn2{Term=[processing],Sec=[withdrawn tests]}


@LabeledSubClause{Test Partitions}

Unless otherwise directed by the Special Requirements section of a test, all
tests are to be configured and run in a single partition. The method of
specifying such a partition is implementation dependent and not determined by
the ACATS. The only tests that must be run in multiple partitions are those
that test Annex E, Distributed Systems.


@LabeledSubClause{Bundling Test Programs}

In some situations, the usual test processing sequence may require an
unacceptable amount of time. For example, running tests on an embedded target
may impose significant overhead time to download individual tests. In these
cases, executable tests may be bundled into aggregates of multiple tests. A set
of bundled tests will have a driver that calls each test in turn; ACATS tests
will then be called procedures rather than main procedures. No source changes
in the tests are allowed when bundling; that is, the only allowed change is the
method of calling the test. Since ACATS tests are often designed to follow
common usage patterns, including reuse of units, and reusable units are often
self-initializing (so that they are resilient against misuse), not all ACATS
tests can be bundled arbitrarily. In particular, foundations and shared support
code may not (and cannot, in general, because of the need to test elaboration
actions) support re-initialization and thus a bundled test may malfunction if
it runs after another use of the same foundation or support routine. It is the
responsibility of the ACATS user to ensure that bundled tests execute properly
when bundled.

All bundles must be approved by the ACAL (and, if necessary, the ACAA) to
qualify for a conformity assessment. It is the responsibility of the user to
identify the tests to be bundled and to write a driver for them.


@LabeledSubClause{Processing that may be Omitted}

A user may streamline processing of the ACATS tests to the greatest degree
possible consistent with complete processing of all tests.

Many modern tests rely on foundation code. A foundation need not be compiled
anew each time a different test uses it. In a processing model based on a
program library, it is reasonable to compile the code into the library only
once and allow the binder to use the processed results for each test that
@b{with}s the foundation.

A user may determine, with ACAL concurrence, that some tests require support
that is impossible for the implementation under test to provide. For example,
there are tests that assume the availability of file I/O whereas some (embedded
target) implementations do not support file I/O. Those tests need not be
processed during witness testing; however, the implementer must demonstrate
that they are handled in accordance with the language standard. This
demonstration may be performed before witness testing, in which case it need
not be repeated.

Annex B tests that require foreign language code (Fortran, C, COBOL) to be
compiled and bound with Ada code need not be processed if an implementation
does not support a foreign language interface to the respective
language.@Defn2{Term=[processing],Sec=[foreign language tests]}

Tests for the Specialized Needs Annexes of Ada need not be processed except
by implementations that wish to have Annex results documented. In that case,
only the tests for the annex(es) in question (in addition to all core tests) need
be processed. If any tests for a particular Annex are processed, then all tests
for that Annex must be processed. If an implementation does not support a
feature in a Specialized Needs Annex test, then it must indicate the
non-support by rejecting the test at compile time or by raising an appropriate
exception at run time.@Defn2{Term=[processing],Sec=[SNA tests]}
(See Ada @URLLink{URL=[http://www.adaic.org/resources/add_content/standards/12rm/html/RM-1-1-3.html#p17],Text=[1.1.3(17)]}.)

No withdrawn test need be processed. Tests classified as Pending New in the
current ACATS Modification List also do not need to be processed. Pending New
tests are new tests included with the ACATS for review purposes, and are not
yet required for conformity assessment. (Tests classified as New in the
current ACATS Modification List @i{do} need to be processed; these are
required for
conformity assessments.)@Defn2{Term=[test],Sec=[new AML classification]}@Defn2{Term=[test],Sec=[pending new AML classification]}


@LabeledSubClause{Tests with Special Processing Requirements}

Some tests may require special handling. These are primarily SNA tests, but
some core tests are affected. For example, distributed processing tests may
require an executable image in multiple partitions, where partitions are
constructed in an implementation specific manner. Real-time processing tests
may have configuration pragmas that have to be handled in an implementation
specific way. Numeric processing tests require strict mode processing to be
selected. Each such test has a Special Requirements section in the test header
describing any implementation specific handling that is required for
the test.@Defn{special handling test}@Defn2{Term=[test],Sec=[special handling]}@Defn2{Term=[processing],Sec=[special handling tests]}

A list of all such tests is provided in @RefSec{Tests With Special Requirements}.


@LabeledSubSubClause{Tests Involving Limited Views}

@LocalLink{Target=[Amend1],Sec=[References],Text={[Amend1]}} added the
concept of limited views to Ada. For most ACATS tests, the possibility of
limited views can be ignored, as either they are not used at all, or they
need not be separated from the full view in the environment. (It is presumed
that adding the full view to the environment also adds the corresponding
limited view.)

However, a few tests require that the limited view of a unit
be added to the environment separately from the full view of the unit. Such
tests have dependencies in the full views on units that have not yet been
compiled (added to the environment). For these tests, any extra steps needed
to add the limited view to the environment separately from the full view
will need to be accomplished.

The tests identified below need to add the limited view of one or more units
to the environment separately from the full view of the units.

@begin{FourCol}
ca11023@*
ca12001
@end{FourCol}


@LabeledSubSubClause{Foreign Language Interface Tests}

Annex B, Interface to Other Languages, is part of the Ada core language. Any
implementation that provides one or more of the packages Interfaces.C,
Interfaces.COBOL, or Interfaces.Fortran @i{must} correctly process, and pass,
the tests for interfaces to C, COBOL, and/or Fortran code respectively, with
the possible exception of tests containing actual foreign code.

An implementation that provides one or more of these Interfaces child packages
must successfully compile the Ada units of tests with actual foreign language
code. If the implementation does not support the actual binding of the foreign
language code to Ada, these tests may report binding errors, or may reject the
pragma Import, in which case they may be graded as inapplicable. If the
implementation supports the binding and an appropriate compiler is available,
the tests must execute and report "Passed". If the implementation supports the
binding, but it is not feasible to have an appropriate compiler available, then
the tests may be graded as inapplicable by demonstrating that they fail to
bind.

If one of the Interfaces child packages is not provided, then the corresponding
tests may be graded as inapplicable, provided they reject the corresponding
@key[with] clause.

The tests involving interfaces to foreign code are listed below.

The foreign language code included in ACATS tests uses no special or unique
features, and should be accepted by any standard (C, COBOL, or Fortran)
compiler. However, there may be dialect problems that prevent the code from
compiling correctly. Modifications to the foreign language code are allowable;
the modifications must follow the code as supplied as closely as possible and
the result must satisfy the requirements stated in the file header. Such
modifications must be approved in advance by the ACAL (and, if necessary, the
ACAA).

The method for compiling foreign code is implementation dependent and not
specified as part of the ACATS. Ada code in these tests must be compiled as
usual. The Ada code includes Pragma Import that references the foreign language
code. The link name of foreign language object code must be provided in ImpDef.
When all code has been compiled, the test must be bound (including the foreign
language object code) and run. The method for binding Ada and foreign language
code is implementation dependent and not specified as part of the ACATS. The
test must report "PASSED" when executed.


@Subheading{@Shrink{C Language Interface}}

If the implementation provides the package Interfaces.C, the tests identified
below must be satisfactorily processed as described above.

The starred tests contain C code that must be compiled and linked if possible,
as described above. The C code is easily identifiable because the file has the
extension @Exam{.C}. The C code may be modified to satisfy dialect requirements of
the C compiler. The C code files must be compiled through a C compiler, and the
resulting object code must be bound with the compiled Ada code. Pragma Import
will take the name of the C code from ImpDef.

@begin{FourCol}
cd30005*@*
cxb3001@*
cxb3002@*
cxb3003@*
cxb3004*@*
cxb3005@*
cxb3006*@*
cxb3007@*
cxb3008@*
cxb3009@*
cxb3010@*
cxb3011@*
cxb3012@*
cxb3013*@*
cxb3014@*
cxb3015@*
cxb3016@*
cxb3017*@*
cxb3018*
@end{FourCol}


@Subheading{@Shrink{COBOL Language Interface}}

If the implementation provides the package Interfaces.COBOL, the tests
identified below must be processed satisfactorily, as described above.

The starred test contains COBOL code that must be compiled and linked if
possible, as described above. The COBOL code is easily identifiable because the
file has the extension @exam{.CBL}. The COBOL code may be modified to satisfy
dialect requirements of the COBOL compiler. The COBOL code files must be
compiled through a COBOL compiler, and the resulting object code must be bound
with the compiled Ada code. Pragma Import will take the name of the COBOL code
from ImpDef.

@begin{FourCol}
cxb4001@*
cxb4002@*
cxb4003@*
cxb4004@*
cxb4005@*
cxb4006@*
cxb4007@*
cxb4008@*
cxb4009*
@end{FourCol}


@Subheading{@Shrink{Fortran Language Interface}}

If the implementation has a Fortran language interface, the tests identified
below must be processed satisfactorily, as described above.

The starred tests contain Fortran code that must be compiled and linked if
possible, as described above. The Fortran code is easily identifiable because
the file has the extension @exam{.FTN}. The Fortran code may be modified to
satisfy dialect requirements of the Fortran compiler. The Fortran code files
must be compiled through a Fortran compiler, and the resulting object code must
be bound with the compiled Ada code. Pragma Import will take the name of the
Fortran code from ImpDef.

@begin{FourCol}
cxb5001@*
cxb5002@*
cxb5003@*
cxb5004*@*
cxb5005*
@end{FourCol}


@LabeledSubSubClause{Tests for the Distributed Systems Annex}

The ACATS tests for the Distributed Systems Annex are applicable only to
implementations that wish to test this SNA. Not all of these tests apply to all
implementations, since the annex includes some implementation permissions that
affect the applicability of some tests.

@leading@;The principal factors affecting test applicability are:
@begin{enumerate}
whether the Remote_Call_Interface pragma is supported;

whether a Partition Communication System (PCS) is provided (i.e., whether a
body for System.RPC is provided by the implementation);

whether the implementation has taken advantage of the permission to change
the specification of System.RPC;

whether the Real-Time Annex is also supported.
@end{enumerate}

An implementation may test for the annex without providing a PCS. In order to
test for the Distributed Systems Annex, an implementation must allow a body for
System.RPC to be compiled.


@Subheading{@Shrink{Remote_Call_Interface pragma}}

@leading@;Ada allows explicit message-based communication between
active partitions as an alternative to RPC
[see Ada @URLLink{URL=[http://www.adaic.org/resources/add_content/standards/12rm/html/RM-E-2-3.html#p20],Text=[E.2.3(20)]}].
If an implementation does not support the
Remote_Call_Interface pragma then the following tests are not applicable:

@begin{FourCol}
bxe2009@*
bxe2010@*
bxe2011@*
bxe2013@*
bxe4001@*
cxe2001@*
cxe2002@*
cxe4001@*
cxe4002@*
cxe4003@*
cxe4004@*
cxe4005@*
cxe4006@*
cxe5002@*
cxe5003@*
lxe3001
@end{FourCol}


@Subheading{@Shrink{Partition Communication System}}

@leading@;An implementation is not required to provide a PCS
[see Ada @URLLink{URL=[http://www.adaic.org/resources/add_content/standards/12rm/html/RM-E-5.html#p27],Text=[E.5(27)]}]
in order to test the Distributed Systems Annex. If no PCS is provided then the
following tests are not applicable:

@begin{FourCol}
cxe1001@*
cxe2001@*
cxe4001@*
cxe4002@*
cxe4003@*
cxe4004@*
cxe4005@*
cxe4006@*
cxe5001
@end{FourCol}


@Subheading{@Shrink{System.RPC}}

@leading@;Two tests provide a body for System.RPC, and a third test
checks the specification of System.RPC. An alternative declaration
is allowed for package System.RPC
[see Ada @URLLink{URL=[http://www.adaic.org/resources/add_content/standards/12rm/html/RM-E-5.html#p27.1],Text=[E.5(27.1/2)]}].
If an alternative declaration is used
for System.RPC, the following tests are not applicable:

@begin{FourCol}
cxe5001@*
cxe5002@*
cxe5003
@end{FourCol}


@Subheading{@Shrink{Real-Time Annex Support}}

Many implementations that support the Distributed Systems Annex will also
support the Real-Time Annex. Test cxe4003 is designed to take advantage of
Real-Time Annex features in order to better test the Distributed Systems Annex.

For implementations that do not support the Real-Time Annex, test cxe4003 must
be modified. This modification consists of deleting all lines that end with the
comment @exam{--RT}.


@Subheading{@Shrink{Configuring Multi-Partition Tests}}

Some Distributed Systems Annex tests require multiple partitions to run the
test, but
no more than two partitions are required for running any of them. All
multi-partition tests contain a main procedure for each of the two partitions.
The two partitions are referred to as "A" and "B" and the main procedures for
these partitions are named <test_name>_A and <test_name>_B respectively. Each
test contains instructions naming the compilation units to be included in each
partition. Most implementations will be primarily concerned with the main
procedure and RCI packages that are to be assigned to each partition; the
remainder of the partition contents will be determined by the normal dependency
rules. The naming convention used in multi-partition tests aid in making the
partition assignments. If the name of a compilation unit ends in
"_A<optional_digit]>" then it should be assigned to partition A. Compilation
units with names ending in "_B<optional_digit>" should be assigned to partition
B.

@leading@;The following tests require that two partitions be available to run
the test:

@begin{FourCol}
cxe1001@*
cxe2001*@*
cxe2002@*
cxe4001@*
cxe4002@*
cxe4003@*
cxe4004@*
cxe4005@*
cxe4006@*
cxe5002@*
cxe5003@*
lxe3001@*
lxe3002*
@end{FourCol}

(*) Tests cxe2001 and lxe3002 contain a Shared_Passive package and two active
partitions. They may be configured with either two or three partitions. The
two-partition configuration must have two active partitions and the
Shared_Passive package may be assigned to either one of the active partitions.
The three-partition configuration consists of two active partitions and a
single passive partition, and the passive partition will contain the single
Shared_Passive package.


@Subheading{@Shrink{Running Multi-Partition Tests}}

All of the multi-partition tests include the package Report in both of the
active partitions. In order for the test to pass, both partitions must produce
a passed message (except for lxe3002 - see special instructions for that test).
If either partition produces a failed message, or if one or both partitions do
not produce a passed message, the test is graded "failed".

When running the multi-partition tests it is not important which partition is
started first. Generally, partition A acts as a server and partition B is a
client, so starting partition A first is usually best.

In the event a test fails due to the exception Communication_Error being
raised, it is permissible to rerun the test.


@LabeledSubSubClause{Tests for the Numerics Annex}

@leading@;Many of the tests for Annex G, Numerics, @i{must} be run in strict
mode. The method for selecting strict mode is implementation dependent and not
specified by the ACATS. (Note that the tests for numerical functions specified
in Annex A may, @i{but need not}, be run in strict mode.) The following tests
must be run in strict mode:

@begin{FourCol}
cxg2003@*
cxg2004@*
cxg2006@*
cxg2007@*
cxg2008@*
cxg2009@*
cxg2010@*
cxg2011@*
cxg2012@*
cxg2013@*
cxg2014@*
cxg2015@*
cxg2016@*
cxg2017@*
cxg2018@*
cxg2019@*
cxg2020@*
cxg2021
@end{FourCol}


@LabeledSubSubClause{Tests that use Configuration Pragmas}

@leading@;Several of the tests in Annex D, Real Time Systems, Annex E,
Distributed Systems, and Annex H, High Integrity Systems, use configuration
pragmas. The technique for applying a configuration pragma to a test composed
of multiple compilation units is implementation dependent and not specified by
the ACATS. Every implementation that uses any such test in a conformity
assessment must therefore take the appropriate steps, which may include
modifications to the test code and/or post-compilation processing, to ensure
that such a pragma is correctly applied. The following tests require special
processing of the configuration pragma:

@begin{FourCol}
ba15001@*
bxc5001@*
bxh4001@*
bxh4002@*
bxh4003@*
bxh4004@*
bxh4005@*
bxh4006@*
bxh4007@*
bxh4008@*
bxh4009@*
bxh4010@*
bxh4011@*
bxh4012@*
bxh4013@*
cxd1004@*
cxd1005@*
cxd2001@*
cxd2002@*
cxd2003@*
cxd2004@*
cxd2005@*
cxd2006@*
cxd2007@*
cxd2008@*
cxd3001@*
cxd3002@*
cxd3003@*
cxd4001@*
cxd4003@*
cxd4004@*
cxd4005@*
cxd4006@*
cxd4007@*
cxd4008@*
cxd4009@*
cxd4010@*
cxd5002@*
cxd6002@*
cxd6003@*
cxda003@*
cxdb005@*
cxh1001@*
cxh3001@*
cxh3003@*
lxd7001@*
lxd7003@*
lxd7004@*
lxd7005@*
lxd7006@*
lxd7007@*
lxd7008@*
lxd7009@*
lxh4001@*
lxh4002@*
lxh4003@*
lxh4004@*
lxh4005@*
lxh4006@*
lxh4007@*
lxh4008@*
lxh4009@*
lxh4010@*
lxh4011@*
lxh4012@*
lxh4013
@end{FourCol}


@LabeledSubClause{Focus on Specific Areas}

The ACATS test suite is structured to allow compiler developers and testers to
use parts of the suite to focus on specific compiler feature areas.

Both the legacy tests and the modern tests tend to focus on specific language
features in individual tests. The name of the test is generally a good
indicator of the primary feature content of the test, as explained in the
discussion of naming conventions. Beware that legacy test names have not
changed, but the Ada Reference Manual organization has changed from
@LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}} to
@LocalLink{Target=[Ada95],Sec=[References],Text={[Ada95]}}, so some legacy
test names point to the wrong clause of the Ada Standard.
Further, note that the general style and approach of the modern tests creates
user-oriented test situations by including a variety of features and
interactions. Only the primary test focus can be indicated in the test name.

ACATS 4.0 tests are divided into core tests and Specialized Needs Annex tests.
Recall that annexes A and B are part of the core language. All annex tests
(including those for annexes A and B) have an 'X' as the second character of
their name; Specialized Needs Annex tests have a letter between 'C' and 'H'
(inclusive) corresponding to the annex designation, as the third character of
the test name.


@LabeledClause{Grading Test Results}

Although a single test may examine multiple language issues, ACATS test results
are graded "passed", "failed", or "not applicable" as a whole.

All customized, applicable tests must be processed by an implementation.
Results must be evaluated against the expected results for each class of test.
Results that do not conform to expectations constitute failures. The only
exceptions allowed are discussed in @RefSecNum{Allowed Test Modifications};
in such cases, processing the approved modified test(s) must produce the
expected behavior. Any differences from the general discussion of expected
results below for executable or non-executable tests are included as explicit
test conditions in test prologues.

Warning or other informational messages do not affect the pass/fail status of
tests.

Expected results for executable and non-executable tests are discussed in
Sections @RefSecNum{Expected results for Executable Tests},
@RefSecNum{Expected Results for Class B Tests}, and
@RefSecNum{Expected Results for Class L Tests}.
Tests that are non-applicable for an implementation are
discussed in @RefSecNum{Inapplicable Tests}.
Withdrawn tests are discussed in @RefSecNum{Withdrawn Tests}.


@LabeledSubClause{Expected Results for Executable Tests}

Executable tests (classes A, C, D, E) must be processed by the compiler and any
post-compilation steps (e.g., binder, partitioner) without any errors. They
must be loaded into an execution target and run. Normal execution of tests
results in an introductory message that summarizes the test objective, possibly
some informative comments about the test progress, a final message giving pass
/ fail status, and graceful, silent termination. They may report "PASSED",
"TENTATIVELY PASSED", "FAILED", OR "NOT APPLICABLE".

A test that fails to compile and bind, including compiling and binding any
foundation code on which it depends is graded as "failed", unless the test
includes features that need not be supported by all implementations. For
example, an implementation may reject the declaration of a numeric type that it
does not support. Allowable cases are clearly stated in the Applicability
Criteria of tests. Annex M of the Ada Standard
requires implementations to document such implementation-defined
characteristics.

A test that reports "FAILED" is graded as "failed" unless the ACAL, and
possibly the ACAA, determine that the test is not applicable for the
implementation.

A test that reports "PASSED" is graded as "passed" unless the test produces the
pass message but fails to terminate gracefully (e.g., crashes, hangs, raises an
unexpected exception, produces an earlier or later "FAILED" message). This kind
of aberrant behavior may occur, for example, in certain tasking tests, where
there are multiple threads of control. A pass status message may be produced by
one thread, but another thread may  asynchronously crash or fail to terminate
properly.

A test that reports "NOT APPLICABLE" must be run by the implementation and is
graded as "not applicable" unless it produces the not-applicable message and
then fails to terminate gracefully.

A test that reports "TENTATIVELY PASSED" is graded as "passed" if the test
results satisfy the pass/fail criteria in the test. Normally, verification
requires manual inspection of the test output.

A test that fails to report, or produces only a partial report, will be graded
as "failed" unless the ACAL, and possibly the ACAA, determine that the test is
not applicable for the implementation.


@LabeledSubClause{Expected Results for Class B Tests}

Class B tests are expected to be compiled but are not subject to further
processing and are not intended to be executable. An implementation must
correctly report each clearly marked error (the notation @Exam{-- ERROR:}
occurs at the right hand side of the source). A multiple unit B test file
generally will have errors only in one compilation unit per source file.
Error messages must provide some
means of specifying the location of an error, but they are not required to be
in direct proximity with the @Exam{-- ERROR:} marking of the errors.

Some B-tests also include the notation @Exam{-- OK} to indicate constructs that
@i{must not} be identified as errors. Such constructs are typically similar to
illegal constructs and serve to ensure that implementations do not reject too
much. Not identifying @Exam{-- OK} constructs as errors is especially
important since some constructs that were errors
in @LocalLink{Target=[Ada83],Sec=[References],Text={[Ada83]}}
are now legal in later versions of Ada.

Some B-tests exercise constructs whose correctness depends on source code that
is textually separated (for example, a deferred constant and its full declaration). In
these cases, it may be reasonable to report an error at both locations. Such
cases are marked with @Exam{-- OPTIONAL ERROR}. These lines may be flagged as
errors by some, but not all, implementations. Unless an optional error is
marked as an error for the wrong reason, an error report (or lack of it) does
not affect the pass/fail status of the test.

Some B-tests contain constructs where it would be reasonable for a compiler
to report an error at one of several source locations. When it would not be
appropriate for the ACATS to insist on a particular source location, all such
source locations are marked with @exam{-- POSSIBLE ERROR:} and an indication of
which error set (if the test contains several) the location belongs to.
In such cases, an implementation is considered to have properly reported the
error if it reports an error at
any of the places marked @exam{-- POSSIBLE ERROR:} for a particular set.
The implementation may
flag more than one such place; this does not affect the pass/fail status
of the test. However, the test is graded "failed" if no error is reported
at any of the places marked @exam{-- POSSIBLE ERROR:} for an error set.

A test is graded as "passed" if it reports each error in the test. The content
of error messages is considered only to determine that they are indeed
indications of errors (as opposed to warnings) and that they refer to the
expected errors. The Reference Manual does not specify the form or content of
error messages. In particular, a test with just one expected error is graded as
"passed" if the test is rejected at compile time for any reason.

A test is graded as "failed" if it fails to report on each error in the test or
if it marks legal code as incorrect.


@LabeledSubClause{Expected Results for Class L Tests}

Class L tests are expected to be rejected before execution begins. They must be
submitted to the compiler and to the linker/binder. If an executable is
generated, then it must be submitted for execution. Unless otherwise
documented, the test is graded as "failed" if it begins execution, regardless
of whether any output is produced.. (Twenty-eight L tests contain documentation
indicating that they may execute. See below.)

In general, an L test is expected to be rejected at link/bind time. Some tests
contain @exam{-- ERROR:} indications; an implementation that reports an error
associated with one of these lines is judged to have passed the test (provided,
of course, that the link attempt fails).

@leading@;The following tests are exceptions to the general rule that an L test
must not execute:

@begin{indent}
@shrink{Test LXE3002, for the Distributed Systems Annex, is a test that
has two partitions, each of which may execute. As documented in the source
code, this test is graded "failed" if both partitions report "TENTATIVELY
PASSED". Other outcomes are graded as appropriate for Class L tests.}

@shrink{Tests LA14001..27 and LA20002 (twenty-seven core language
tests), as documented in the source code, may execute if automatic
recompilation is supported. These tests are graded as "passed" if they execute
and report "PASSED". Other outcomes are graded as appropriate for Class L
tests.}
@end{indent}


@LabeledSubClause{Inapplicable Tests}

Each ACATS test has a test objective that is described in the test prologue.
Some objectives address Ada language features that need not be supported by
every Ada implementation (e.g., "check floating-point operations for digits
18"). These test programs generally also contain an explicit indication of
their applicability and the expected behavior of an implementation for which
they do not apply. @RefSec{Test Applicability Criteria} lists common reasons
for a test to be inapplicable, and lists the
tests affected.@Defn{inapplicable test}@Defn2{Term=[test],Sec=[inapplicable]}

@leading@;A test may be inapplicable for an implementation given:
@begin{itemize}
appropriate ACATS grading criteria; or

an ACAA ruling on a petition to accept a deviation from expected results.
@end{itemize}

@leading@;Appropriate grading criteria include:
@begin{enumerate}
whether a test completes execution and reports "NOT APPLICABLE";

whether a test is rejected at compile or bind time for a reason that satisfies
grading criteria stated in the test program.
@end{enumerate}

All applicable test programs must be processed and passed.


@LabeledSubClause{Withdrawn Tests}

From time to time, the ACAA determines that one or more tests included in a
release of the ACATS should be withdrawn from the test suite. Tests that are
withdrawn are not processed during a conformity assessment and are not
considered when grading
an implementation.@Defn{withdrawn test}@Defn2{Term=[test],Sec=[withdrawn]}

Usually, a test is withdrawn because an error has been discovered in it. A
withdrawn test will not be reissued as a modified test, although it may be
revised and reissued as a new test in the future.

Withdrawn tests are listed in the ACATS Modification List, which is maintained
by the ACAA.


@LabeledClause{Addressing Problems or Issues}

After all tests have been processed and graded, any remaining problems should
be addressed. Test failures must be identified and resolved. This section
discusses issues that are not due to implementation errors (bugs).


@LabeledSubClause{Typical Issues}

@leading@;Here are some typical causes of unexpected ACATS test failures (often
resulting from clerical errors):
@begin{Itemize}
Processing a test that is withdrawn;

Processing the original version of a test that has been modified by the ACAA to
correct a test error;

Processing a test that is not applicable to the implementation (as explained in
Section @RefSecNum{Inapplicable Tests});

Processing files (or tests, see Section @RefSecNum{Dependencies}) in an incorrect order;

Processing tests when units required in the environment are not present.
@end{Itemize}

@leading@;Test result failures resulting from technical errors may include:
@begin{Itemize}
Incorrect values in ImpDef, which provide inappropriate values to tests at
run-time (refer to @RefSecNum{ImpDef Customization});

Incorrect values in @exam{macro.dfs}, which result in incorrectly customized
tests (refer to @RefSecNum{Macro Defs Customization});

Need to modify a test (e.g., split a B-test).
@end{Itemize}

Finally, occasionally a user discovers an error in a new ACATS test. More
rarely, errors are uncovered by compiler advances in tests that are apparently
stable. In either case, if users believe that a test is in error, they may file
a dispute with the ACAL. The dispute process is described in the next section.


@LabeledSubClause{Deviation from Expected Results - Petition & Review}

Each test indicates in its prologue what it expects from a conforming
implementation. The result of processing a test is acceptable if and only if
the result is explicitly allowed by the grading criteria for the test.

A user may challenge an ACATS test on the grounds of applicability or
correctness. A challenger should submit a petition against the test program to
an ACAL or to the ACAA, following the procedure and the format presented in
@LocalLink{Target=[Pro31],Sec=[References],Text={[Pro31]}}. A petition must
clearly state whether it is a claim that the test does not apply to the
implementation or that the test is incorrect. The petition must indicate the
specific section of code that is disputed and provide a full explanation of the
reason for the dispute.

@leading@;ACALs will forward petitions from their customers to the ACAA for
decisions. The ACAA will evaluate the petitioner's claims and decide whether:
@begin{itemize}
the test is applicable to the implementation (i.e., deviation @i{is not}
allowed);

the test is not applicable to the implementation (i.e., deviation @i{is} allowed);

the test should be repaired (deviation is allowed, and the modified test should
be used for determining conformity assessment results);

the test should be withdrawn (deviation is allowed and the test is not
considered in determining conformity assessment results).
@end{itemize}

A deviation is considered to be a test failure unless a petition to allow the
deviation has been accepted by the ACAA.


@LabeledClause{Reprocessing and Regrading}

After all problems have been resolved, tests that failed can be reprocessed and
regraded. This step completes the ACATS testing process.
