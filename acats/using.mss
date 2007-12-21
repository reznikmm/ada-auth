@Part(using, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/using.mss,v $}
@comment{$Revision: 1.2 $ $Date: 2007/12/20 07:43:55 $}

@LabeledSection{Using the ACATS}

There are eight major steps involved in using the ACATS test suite; two of them
are sometimes not required. The steps are: installing the software, tailoring
the software, processing the support files, establishing command scripts,
processing the ACATS tests, grading the test results, addressing problems (if
necessary), and reprocessing problem tests (if necessary). The first six of
these tasks must be completed successfully to accomplish a test run. The first
four normally need be completed only once for each ACATS release. Each step is
explained in the following sections. The flow from one to the next is
illustrated in following figures.@*@Comment{Extra blank line to provide some space above the picture}

@PictureAlone{Alignment=[Center],
Border=[None], Height=[508],Width=[363],Name=[Usage-1.png],
Descr=[Using the ACATS, part 1]}
@Center{@Shrink{Using the ACATS}}

@PictureAlone{Alignment=[Center],
Border=[None], Height=[372],Width=[313],Name=[Usage-2.png],
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
@LocalLink{Target=[Pro01],Sec=[References],Text={[Pro01]}} for details.

The ACAA also will issue new tests periodically. As with modified tests, new
tests must be available for a period of time before they are required in
conformity assessments.

These changes to the issued ACATS are documented in the ACATS Modification List
(AML).@Defn{ACATS Modification List}@Defn{AML} This list includes a list of all
new tests, all modified tests, and all withdrawn tests, and an indication as to
when each will be (or is) required for conformity assessments. Each version of
the modification list is given a suffix letter. An ZIP archive and tar file
containing the new and/or modified tests is available. The files are named
MOD_3_0x, where 'x' represents the suffix letter for the AML version.

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
@exam{@shrink{./acats3_0/a}}@*
@exam{@shrink{./acats3_0/b2}}@*
@exam{@shrink{./acats3_0/b3}}@*
@exam{@shrink{./acats3_0/b4}}@*
@exam{@shrink{./acats3_0/b5}}@*
@exam{@shrink{./acats3_0/b6}}@*
@exam{@shrink{./acats3_0/b7}}@*
@exam{@shrink{./acats3_0/b8}}@*
@exam{@shrink{./acats3_0/b9}}@*
@exam{@shrink{./acats3_0/ba}}@*
@exam{@shrink{./acats3_0/bb}}@*
@exam{@shrink{./acats3_0/bc}}@*
@exam{@shrink{./acats3_0/bd}}@*
@exam{@shrink{./acats3_0/be}}@*
@exam{@shrink{./acats3_0/bxa}}@*
@exam{@shrink{./acats3_0/c2}}@*
@exam{@shrink{./acats3_0/c3}}@*
@exam{@shrink{./acats3_0/c4}}@*
@exam{@shrink{./acats3_0/c5}}@*
@exam{@shrink{./acats3_0/c6}}@*
@exam{@shrink{./acats3_0/c7}}@*
@exam{@shrink{./acats3_0/c8}}@*
@exam{@shrink{./acats3_0/c9}}@*
@exam{@shrink{./acats3_0/ca}}@*
@exam{@shrink{./acats3_0/cb}}@*
@exam{@shrink{./acats3_0/cc}}@*
@exam{@shrink{./acats3_0/cd}}@*
@exam{@shrink{./acats3_0/ce}}@*
@exam{@shrink{./acats3_0/cxa}}@*
@exam{@shrink{./acats3_0/cxb}}@*
@exam{@shrink{./acats3_0/cz}}@*
@exam{@shrink{./acats3_0/d}}@*
@exam{@shrink{./acats3_0/e}}@*
@exam{@shrink{./acats3_0/l}}@*
@exam{@shrink{./acats3_0/bxc}}@*
@exam{@shrink{./acats3_0/bxd}}@*
@exam{@shrink{./acats3_0/bxe}}@*
@exam{@shrink{./acats3_0/bxf}}@*
@exam{@shrink{./acats3_0/bxh}}@*
@exam{@shrink{./acats3_0/cxc}}@*
@exam{@shrink{./acats3_0/cxd}}@*
@exam{@shrink{./acats3_0/cxe}}@*
@exam{@shrink{./acats3_0/cxf}}@*
@exam{@shrink{./acats3_0/cxg}}@*
@exam{@shrink{./acats3_0/cxh}}@*
@exam{@shrink{./acats3_0/lxd}}@*
@exam{@shrink{./acats3_0/lxe}}@*
@exam{@shrink{./acats3_0/lxh}}@*
@exam{@shrink{./acats3_0/docs}}@*
@exam{@shrink{./acats3_0/support}}@Comment{This one is too long for RTF otherwise}
@end{Fourcol}

Note that the names are given here in all lowercase; some systems may create
lowercase names. The path separator, shown here as '/', may also differ.


@LabeledSubSubClause{Decompressing Zipped Files}

All ACATS files have been compressed (zipped) into compressed archives
(zip-files) that have the MS-DOS file extension ".zip". A Windows command-line
utility was used
to compress them. They must be decompressed before they can be further
processed. A decompression utility is available from the source of the ACATS
distribution. All ACATS 3.0 files may be decompressed using the following
steps. Approximately 32 MB of free space on a Windows machine hard drive will
be required to accomplish the decompression using this technique.

Create a directory on the hard disk to contain ACATS. In these examples, we
assume the name is @Exam{acats3_0}, but any name can be used. Copy the archive
(file with .zip extension) to the hard disk in the new directory. Decompress it
insuring that directories are used. For the @Exam{unzip} program, this is the
default setting. For the @exam{pkunzip} program, this is the -d option. For the
@exam{winzip} program, ensure that "Use Directory Names" is checked. Also,
ensure that the files are decompressed into the proper directory. For command
line decompressors, this means ensuring that the current subdirectory is
acats3_0. For @exam{winzip}, this simply means selecting acats3_0 as the extract
path.

@leading@;For example, using unzip, and assuming that the archive name is ACATS30.zip,
type
@begin{Example}
cd acats3_0
@end{Example}
@leading@;to set the proper directory, and
@begin{Example}
unzip ACATS30
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
first copy the compressed files acats_30.tar.Z from the distribution source to
a hard drive. Uncompress the file with the Unix command
@begin{example}
uncompress acats_30.tar.Z
@end{example}
@leading@;(note that particular Unix implementations may have different formats
or require specific qualifiers.)  After the ACATS file has been uncompressed,
it must be untarred. Move to the directory where you want the acats3_0
directory to be created and then untar the ACATS files
@begin{example}
tar -xvf <path>/acats_30.tar
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


There are some files in the delivery that require modification before ACATS 3.0
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
generally can be used with ACATS 3.0, but users should ensure that neither
their requirements nor the underlying files have changed since the
customizations were made.


@LabeledSubClause{ImpDef Customization}

@begin{Itemize}
@Noprefix@red{@i{There was no change to Impdef or any of its children from
ACATS 2.6 to ACATS 3.0. A version of any of these packages that was tailored
for ACATS 2.6 should be valid for ACATS 3.0 unless some implementation
characteristics have changed.}}
@end{Itemize}

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

Tested depending on Impdef do not need customization (macro expansion).
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

@begin{Itemize}
@Noprefix@red{@i{All implementations must customize @exam{impdef.a} unless they
wish to rely on the defaults provided. ImpDef must be part of the environment
whenever a test that depends on it is processed. Similarly, the child of
Impdef corresponding to each Specialized Needs Annex that the implementer
intends to test during a conformity assessment must be customized and be
part of the environment when the Annex tests are processed.}}
@end{Itemize}


@LabeledSubClause{Macro Defs Customization}

@begin{Itemize}
@Noprefix@red{@i{
There was no change to the @exam{macro.dfs} file from ACATS 2.6 to ACATS 3.0. A
version of @exam{macro.dfs} that was tailored for ACATS 2.6 should be valid for ACATS
3.0 unless some implementation characteristics have changed.}}
@end{Itemize}

Tests in files with the extension @exam{.tst} contain symbols that represent
implementation dependent values. The symbols are identifiers with a initial
dollar sign ('$'). Each symbol must be replaced with an appropriate textual
value to make the tests compilable.

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


@LabeledSubClause{Package SPPRT13 and Function FcnDecl}

Package SPPRT13 declares six constants of type System.Address that are
primarily used by tests of Section 13 features. It is in the file @exam{spprt13s.tst}.
As distributed, the package uses macro symbols that must be replaced. In most
cases, the substitution can be accomplished by the macro substitution described
in the preceding section. If appropriate literals, constants, or predefined
function calls can be used to initialize these constants, they should be
supplied in @exam{macro.dfs}. Otherwise, the package FCNDECL must be modified.

@begin{Itemize}
@Noprefix@red{@i{All implementations should verify that package SPPRT13 can be
properly customized using the macro substitution technique. Note that a body
for SPPRT13 is illegal, nor is it allowed to add declarations to
package SPPRT13.}}
@end{Itemize}

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
