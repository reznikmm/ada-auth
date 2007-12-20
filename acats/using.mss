@Part(using, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/using.mss,v $}
@comment{$Revision: 1.1 $ $Date: 2007/12/19 01:09:29 $}

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
