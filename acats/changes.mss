@Part(changes, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/changes.mss,v $}
@comment{$Revision: 1.7 $ $Date: 2016/07/01 01:43:25 $}

@LabeledSection{Changes for ACATS 4.1}

@Comment{The following list was updated April 27, 2016, and it should be
correct for ACATS 4.1.}

Version 4.1 of the ACATS updates version 4.0 with additional tests for
features defined in @LocalLink{Target=[Ada2012],Sec=[References],Text={[Ada2012]}}.
It includes 134 new tests to check features including type invariants,
subtype predicates, extended return statements, new iterator forms, array
aggregates, generalized indexing, conditional expressions, aspect
specifications, unchecked unions, Ada.Directories,
Ada.Generic_Dispatching_Constructor,
Ada.Environment_Variables, Ada.Text_IO.Bounded_IO and Unbounded_IO, and
Ada.Containers.

In addition, 16 additional tests were corrected or removed to reflect
changes in Ada made by @LocalLink{Target=[Ada2012],Sec=[References],Text={[Ada2012]}},
as well as in response to
test disputes and ARG issue resolutions.

Finally, ACATS 4.1 introduces a tool to simplify and mostly automate test
grading (see @RefSec{ACATS Grading using the Grading Tool}). This tool and
a supporting tool are provided in 6 Ada source files.

See @RefSec{Version Description} for lists of added, deleted and modified
tests, documentation, and support files.



