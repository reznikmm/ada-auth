@Part(params, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/params.mss,v $}
@comment{$Revision: 1.2 $ $Date: 2007/12/19 01:09:29 $}

@LabeledAnnex{Parameterization Files}

In ACATS 3.0, two methods are used to account for the use of
implementation-dependent values in the tests.@Defn{implementation-dependent values}

For legacy tests, a "macro" substitution technique is used.  Legacy tests
requiring implementation-specific values contain symbols beginning with the '$'
character; for example, the symbol @Exam{$INTEGER_LAST} is used where the code
expects the implementation-specific integer literal representing the largest
integer. For each implementation, these symbols must be systematically replaced
with the appropriate values.  A data file, @Exam{MACRO.DFS}, and an Ada
program, @Exam{Macrosub}, are provided to facilitate this
substitution.@Defn{macro substitition}

For tests written since the introduction of Ada 95, a hierearchy of packages is
provided that contain constants and functions that provide the desired
implementation-specific values. These packages ("ImpDef" and its children)
should be modified for each implementation to provide the needed values.

Information regarding the macro substitution technique is presented in Sections
@RefSecNum{Macro Substitution File} and @RefSecNum{Macro Substitution Tests}.
Section @RefSecNum{Package ImpDef and Its Children} describes the ImpDef
package hierarchy.


@LabeledClause{Macro Substitution File}

The support file @Exam{MACRO.DFS} provides substitutions for special symbols
that appear in certain ACATS 3.0 tests (indicated by the three-letter file
type (extension) ".TST" and listed in
Section @RefSecNum{Macro Substitution Tests}). The support program
@Exam{Macrosub} may be used to insert these implementation-specific values
in place of the special symbols in the test. The following excerpt
from the file describes the file and its use.

@begin{Example}
-- MACRO.DFS
-- THIS FILE CONTAINS THE MACRO DEFINITIONS USED IN THE ACVC TESTS.
-- THESE DEFINITIONS ARE USED BY THE ACVC TEST PRE-PROCESSOR,
-- MACROSUB. MACROSUB WILL CALCULATE VALUES FOR THOSE MACRO SYMBOLS
-- WHOSE DEFINITIONS DEPEND ON THE VALUE OF MAX_IN_LEN (NAMELY, THE
-- VALUES OF THE MACRO SYMBOLS BIG_ID1, BIG_ID2, BIG_ID3, BIG_ID4,
-- BIG_STRING1, BIG_STRING2, MAX_STRING_LITERAL, BIG_INT_LIT, BIG_REAL_LIT,
-- AND BLANKS).  THEREFORE, ANY VALUES GIVEN IN THIS FILE FOR THOSE
-- MACRO SYMBOLS WILL BE IGNORED BY MACROSUB.

-- NOTE: AS REQUIRED BY THE MACROSUB PROGRAM, THE FIRST MACRO DEFINED
-- IN THIS FILE IS $MAX_IN_LEN.  THE NEXT 5 MACRO DEFINITIONS
-- ARE FOR THOSE MACRO SYMBOLS THAT DEPEND ON THE VALUE OF
-- MAX_IN_LEN.  THESE ARE IN ALPHABETIC ORDER.  FOLLOWING THESE
-- ARE 36 MORE DEFINITIONS, ALSO IN ALPHABETIC ORDER.

-- EACH DEFINITION IS ACCORDING TO THE FOLLOWING FORMAT:

-- A. A NUMBER OF LINES PRECEDED BY THE ADA COMMENT DELIMITER, --.
--    THE FIRST OF THESE LINES CONTAINS THE MACRO SYMBOL AS IT APPEARS
--    IN THE TEST FILES (WITH THE DOLLAR SIGN).  THE NEXT FEW "COMMENT"
--    LINES CONTAIN A DESCRIPTION OF THE VALUE TO BE SUBSTITUTED.
--    THE REMAINING "COMMENT" LINES, THE FIRST OF WHICH BEGINS WITH THE
--    WORDS "USED IN:  " (NO QUOTES), CONTAIN A LIST OF THE TEST FILES
--    (WITHOUT THE .TST EXTENSION) IN WHICH THE MACRO SYMBOL APPEARS.
--    EACH TEST FILE NAME IS PRECEDED BY ONE OR MORE BLANKS.
-- B. A LINE, WITHOUT THE COMMENT DELIMITER, CONSISTING OF THE
--    IDENTIFIER (WITHOUT THE DOLLAR SIGN) OF THE MACRO SYMBOL,
--    FOLLOWED BY A SPACE OR TAB, FOLLOWED BY THE VALUE TO BE
--    SUBSTITUTED.  IN THE DISTRIBUTION FILE, A SAMPLE VALUE IS
--    PROVIDED; THIS VALUE MUST BE REPLACED BY A VALUE APPROPRIATE TO
--    THE IMPLEMENTATION.

-- DEFINITIONS ARE SEPARATED BY ONE OR MORE EMPTY LINES.
-- THE LIST OF DEFINITIONS BEGINS AFTER THE FOLLOWING EMPTY LINE.

-- $MAX_IN_LEN
-- AN INTEGER LITERAL GIVING THE MAXIMUM LENGTH PERMITTED BY THE
-- COMPILER FOR A LINE OF ADA SOURCE CODE (NOT INCLUDING AN END-OF-LINE
-- CHARACTER).
-- USED IN:  A26007A
MAX_IN_LEN                60

@Examcom{...}
@end{Example}


@LabeledClause{Macro Substitution Tests}

The following test files contain the special symbols used for substituting
implementation-specific values, as described in
Section @RefSecNum{Macro Substitution File}. This list also
appears in the ACATS 3.0 "support" directory as @Exam{TSTTESTS.DAT}.

@begin{FourCol}
A26007A.TST@*
AD8011A.TST@*
B22001A.TST@*
B22001B.TST@*
B22001C.TST@*
B22001D.TST@*
B22001E.TST@*
B22001F.TST@*
B22001G.TST@*
B22001I.TST@*
B22001J.TST@*
B22001K.TST@*
B22001L.TST@*
B22001M.TST@*
B22001N.TST@*
B54B01B.TST@*
BD2A02A.TST@*
BD2C01D.TST@*
BD2C02A.TST@*
BD2C03A.TST@*
BD4006A.TST@*
BD8001A.TST@*
BD8002A.TST@*
BD8003A.TST@*
BD8004A.TST@*
BD8004B.TST@*
BD8004C.TST@*
C23003A.TST@*
C23003B.TST@*
C23003G.TST@*
C23003I.TST@*
C35502D.TST@*
C35502F.TST@*
C35503D.TST@*
C35503F.TST@*
C45231D.TST@*
C4A007A.TST@*
C87B62D.TST@*
C96005B.TST@*
CC1225A.TST@*
CD1009K.TST@*
CD1009T.TST@*
CD1009U.TST@*
CD1C03E.TST@*
CD1C06A.TST@*
CD2A83C.TST@*
CD2A91C.TST@*
CD2C11A.TST@*
CD2C11D.TST@*
CD4041A.TST@*
CD7101G.TST@*
CE2102C.TST@*
CE2102H.TST@*
CE2103A.TST@*
CE2103B.TST@*
CE2203A.TST@*
CE2403A.TST@*
CE3002B.TST@*
CE3002C.TST@*
CE3102B.TST@*
CE3107A.TST@*
CE3304A.TST@*
SPPRT13S.TST
@end{FourCol}


@LabeledClause{Package ImpDef and Its Children}

The package ImpDef (for "Implementation Definitions") provides constants and
functions for producing implementation-specific values required by certain test
programs. This package resides in the file @Exam{ImpDef.a} in the "support"
directory. Four child packages are also included in the "support" directory,
each providing the means for producing implementation-specific values required
by certain test programs for a particular Specialized Needs Annex. These
packages have names of the form ImpDef.Annex_X, and reside in files with names
of the form @Exam{ImpDefX.a}, where 'X' is replaced by the letter designating
the relevant Annex.

The ImpDef package and each of its children should be modified for each
implementation as described in the source code. The following excerpt from the
"ImpDef.a" file illustrates how these modification points are indicated in
the packages.

@begin{Example}
@key[package] ImpDef @key[is]

--=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====--

   -- The following boolean constants indicate whether this validation will
   -- include any of annexes C-H. The values of these booleans affect the
   -- behavior of the test result reporting software.
   --
   --    True  means the associated annex IS included in the validation.
   --    False means the associated annex is NOT included.

   Validating_Annex_C : @key[constant] Boolean := False;
   --                                       ^^^^^ --- MODIFY HERE AS NEEDED

   Validating_Annex_D : @key[constant] Boolean := False;
   --                                       ^^^^^ --- MODIFY HERE AS NEEDED

   Validating_Annex_E : @key[constant] Boolean := False;
   --                                       ^^^^^ --- MODIFY HERE AS NEEDED

   Validating_Annex_F : @key[constant] Boolean := False;
   --                                       ^^^^^ --- MODIFY HERE AS NEEDED

   Validating_Annex_G : @key[constant] Boolean := False;
   --                                       ^^^^^ --- MODIFY HERE AS NEEDED

   Validating_Annex_H : @key[constant] Boolean := False;
   --                                       ^^^^^ --- MODIFY HERE AS NEEDED

--=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====-=====--

   -- This is the minimum time required to allow another task to get
   -- control.  It is expected that the task is on the Ready queue.
   -- A duration of 0.0 would normally be sufficient but some number
   -- greater than that is expected.

   Minimum_Task_Switch : @key[constant] Duration := 0.1;
   --                                         ^^^ --- MODIFY HERE AS NEEDED

@Examcom{...}
@end{Example}

