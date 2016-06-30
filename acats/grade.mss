@Part(using, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/grade.mss,v $}
@comment{$Revision: 1.1 $ $Date: 2016/05/26 05:36:18 $}

@LabeledSection{ACATS Grading using the Grading Tool}

ACATS 4.1 introduces an optional tool to (mostly) automate grading of ACATS
tests.

When the ACATS was designed (as the ACVC in the early 1980s), the intention
always was that running it would give a simple and clear Pass or Fail result.
However, grading of tests (particularly of B and L Tests) is somewhat
subjective and very time-consuming. (Test grading by formal testers
typically involves poring over compiler listings of the entire ACATS with a
large highlighter.)

The grading tool greatly reduces this effort and enforces both the processing
rules (as outlined in @RefSecnum{Dependencies}) and expected results for a
test (see @RefSecNum{Grading Test Results}).

@Red{Use of the grading tool is optional for ACATS 4.1. Manual test grading
is acceptable for formal conformity assessments; whether to use the grading
tool will be left to implementers and their ACAL. The ACAA will use experience
with the tool to inform whether using the tool should be required for future
ACATS versions.}


@LabeledClause{Using the Grading Tool}

The ACATS Grading Tool, "Grade" takes three files as input and produces
a grading report, with details for each test followed by a summary and
an overall Passed or Failed result.

The three files are:

@begin{Indent}
@begin{Description}
Event Trace File@\Includes each interesting event that occurs during the
processing of an ACATS test. Each ACATS User (or their implementor) needs
to provide a method of producing an event trace file for their implementation
in order to use the Grading Tools. See clause @RefSecNum{Event Trace Files}.
for more information.

Test Summary File@\A machine-readable distillation of one or more
ACATS tests. These are created by a tool included with the ACATS. See
clause @RefSecNum{Test Summary Files} for more information.

Manual Grading File@\A list of tests that may require manual grading. This
file can be created with any plain text editor, and may be empty.
See clause @RefSecNum{Manual Test Grading File} for more information.
@end{Description}
@end{Indent}


** TBD - Example here: Chapter 5 C-Tests for Janus/Ada.


@LabeledSubClause{Workflow using the Grading Tool}

** TBD.

@LabeledSubClause{Compiling the Grading Tool and the Test Summary Tool}

** TBD.

@LabeledSubClause{Grading Tool Reference}

** TBD.

@LabeledSubClause{Test Summary Tool Reference}

** TBD.

@LabeledClause{Event Trace Files}

An event trace file includes each (interesting) event that occurs during the
compilation, binding/linking, and execution of one or more ACATS tests.

An event trace file provides a way to present the implementation-specific
format of events to the Grading Tool in a common format. In order for an
ACATS user to use the Grading Tool, they will need to provide a method to
get an event trace file from the implementation's processing of ACATS tests.
There are a number of ways to accomplish that; several are outlined in
following subclauses. No matter what method is selected, it should be possible
to use the same options/tools for future ACATS tests (creating event trace
files should be a one-time cost).

The events of an event trace file are intended to be abstract representations
of the processes of an implementation. It should be possible to map the
processes of any Ada implementation into an event trace file.

The event trace file was selected as the method of abstracting implementation
processing in order to avoid the ACATS Grading Tool from providing an
disincentive to innovation in error handling by Ada implementations. The files
are not intended to be useful (directly) to a human user, so their details
should have little effect on the human error handling interface for an
implementation. Moreover, while the event trace files provide values for
error messages and positioning, neither of these is required for formal
grading (they're provided to making easier for an ACATS user to figure out
why a test is reported as failing). As noted in
@RefSecNum{Expected Results for Class B Tests}, the actual text of an error
message is not used to determine pass or fail for grading purposing; only
the specified location of the error is used.


@LabeledSubClause{Event Trace File Reference}

An event trace file is a CSV (Comma Separated Value) file of event records.
See @RefSec{CSV File Reference} for the general rules for constructing
a CSV file.

@leading@;An event trace file contains the following comma-separated fields:

@begin{Indent}
@begin{Description}
Event@\One of UNKN (Unknown), CSTART (Compilation_Start),
CEND (Compilation_End), CERR (Compile_Error),
CWARN (Compile_Warning),
BSTART (Binder_Start), BEND (Binder_End),
BERR (Binder_Error), BWARN (Binder_Warning),
EXSTART (Execution_Start),
EXEND (Execution_End), EXFAIL (Execution_Failure),
EXNA (Execution_Not_Applicable, EXSACT (Execution_Special_Action),
EVENT (See below). These values are case-insensitive.
"EVENT" is treated as specifying a comment; it usually appears in column
headers.

Timestamp@\The timestamp, double quoted, in the format specified by
Ada.Calendar.Formatting.Image.

Name@\The double quoted name of the source file, main subprogram, or test.
For Compilation events, this is the simple name of the source file.
For Binder events, this is the name of the main subprogram.
For Execution events, this is the name of the test as passed to Report.Test.

Line@\For Compilation_Start, the first line of the current compilation unit.
(Usually 1, unless there are multiple compilation units in a single file.)
For Compile_Error or Compile_Warning, the line number that that error or
warning is reported. (This is critical to the correct operation of the
grading tool.) Otherwise, it is not used and can be omitted other than the
comma separator.

Position@\For Compile_Error or Compile_Warning, the position within the
line that on which the error is reported. An implementation does not have
to provide a meaningful Position for errors (use the -No_Position option
on the Grading Tool if this is true for your implementation).
Otherwise, it is not used and can be omitted other than the comma separator.

Message@\The double quoted message (make sure to replace any double quotes,
as they are not allowed in double quoted strings). For Compile and Binder
Errors and Warnings, this is the message emitted by the appropriate tool.
For Execution events, this is the message passed to Report. For End events,
this is an implementation-defined result of the operation (OK, with Errors,
Passed, Failed, and so on). If there is no appropriate message, nothing need
be written for this field (as it is last, there is no trailing comma).
@end{Description}
@end{Indent}

There is an example of writing an event trace file in the file Report.A, in
procedure Put_Event_Trace. Most of the code involves limiting the length of, and
removing any double quotes from, the (quoted) message string. Note that
Put_Event_Trace writes column headers into a new file, so that headers exist
if the file is loaded into a spreadsheet or database. This is recommended for
any tool that creates an event trace.

The Grading Tool treats any record that starts with EVENT as a comment; this
skips any headers and allows event trace files to be concatenated together
for combined processing.

Not all of the information in an event trace is currently used by the Grading
Tool. We included additional information (like warnings) in part because future
versions of the ACATS tools might need them and changing the format in the
future could be very disruptive. In addition, it's possible that this
compiler-independent event format could be useful to other future ACATS
tools or even third-party tools having nothing to do with the ACATS. As such,
we included all of the information that seemed potentially useful.


@LabeledSubClause{Creating an Event Trace directly by the implementation}

** TBD.

@LabeledSubClause{Creating an Event Trace from Listings}

** TBD.

@LabeledClause{Test Summary Files}

A test summary file is a machine-readable distillation of one or more
ACATS tests. The summary includes the information about the test that is
needed by the Grading Tool. Test summaries are created with the Summary
tool (see @RefSecNum{Workflow using the Grading Tool} and
@RefSecNum{Test Summary Tool Reference}).

@LabeledSubClause{Test Summary File Reference}

** TBD.

@LabeledSubClause{Range Indicators}

Optional range indicators (sometimes known as location indicators)
can appear after the various markers in an ACATS
test source file. These describe the exact range of the reported location
of an expected error. Range indicators are usually used to expand the range
of an error beyond the same line as the ERROR: or other marker.

The format of a range indicator is:
@begin{Example}
   {[sl:]sp[;[el:]ep]}
@end{Example}

@Leading@;In the above, '{' and '}' are literal, while '[' and ']' indicate
optionality. Each of the four values is relative, so it is one or two digits,
with an optional minus sign for @Exam{el}. Omitted values are assumed to be
zero. Specifically:
@begin{Indent}
@begin{Description}
@Exam{sl}@\Start Line @en offset @i<before> the current line for the
start of the error range.

@Exam{sp}@\Start Position @en position offset in the line indicated by
@Exam{sl}, relative to the start of the line.

@Exam{el}@\End_Line @en offset @i<before> the current line for the end of
the error range. Can be negative if the end of the error
range follows the error tag. But almost always should be zero.

@Exam{ep}@\End Position @en position offset from the last significant
character in the line indication by End_Line. (The last
significant character is the last non-white-space character
not including any comment.)
@end{Description}
@end{Indent}

This compact representation was chosen because of the limited space given
the ACATS line length limit (see @RefSecNum{General Standards}) and a desire
to avoid unnecessarily cluttering tests with extraneous information.


@LabeledClause{Manual Test Grading Files}

The ACATS Grading Tool takes a file containing a list of tests that may
require manual grading.

The file is just a list of ACATS test names (7 characters each), one per line.
Ada comments (anywhere on a line) and blank lines are also allowed.

If the Grading Tool encounters a failure for one of the tests in the manual
grading file for that grading run, it will report that the test needs manual
grading rather than that it failed. The manual grading list has no effect
on tests that are graded as Passed or Not Applicable.

An ACATS user can add tests to this file as needed. Some of the reasons that
one might want to do this are discussed in
@RefSecNum{Workflow using the Grading Tool}.

@Red{The ACAA would like to know which tests require manual grading for your
implementation. Please send manual grading files annotated with comments as
to why they require manual grading to the ACAA Technical Agent,
agent@ada-auth.org. The ACAA will use this information to determine which,
if any tests, require repair to better provide error range information
(potentially including alternative error locations.)}

For formal testing, the ACAL should be aware of all tests included in any
manual grading file used.


@LabeledClause{CSV File Reference}

Several of the files used by the Grading Tool are .CSV (Comma Separated Value)
files. This format was chosen as it is:

@begin{itemize}
is a pure text format that is easy to write in Ada. Records can be appended to
an existing file by opening the file with Text_IO in Append_Mode, Put_Line the
line of values for the record, then closing the file. No end markers need
to be moved.

is reasonably easy to read in Ada using Text_IO. Get_Line and some string
operations are sufficient.

can be read by most spreadsheet and database programs. Thus, we don't need
to provide tools to inspect, check, or edit the contents of these files.
@end{itemize}

A CSV consists of list of records, one record per line. Each line is a set
of values, separated by commas. Each line should have the same number of
values, so when loaded in a spreadsheet it becomes a series of columns
with each Ada component associated with a column.

There are a number of varieties of CSV files, so we have adopted one of the
simplest in order to have the widest applicability.

There are only a few restrictions on the values in our CSV files. Unquoted
values must not contain commas, spaces, tabs, or semicolons. A value can be
quoted with double quotes, in which case commas, spaces, tabs, and semicolons
are allowed, but double quotes are @b{not} allowed in quoted strings.

We require that values are limited to 150 characters (primarily to prevent
excessively long messages from making reports hard to read).

There is an example of writing a CSV file (an event trace - see
@RefSecNum{Event Trace Files}) in the file Report.A, in procedure
Put_Event_Trace. Most of the code involves limiting the length of, and removing
any double quotes from, the (quoted) message string.
