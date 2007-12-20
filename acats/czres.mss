@Part(czres, Root="acats.msm")

@comment{$Source: e:\\cvsroot/ARM/ACATS/czres.mss,v $}
@comment{$Revision: 1.2 $ $Date: 2007/12/19 01:09:29 $}

@LabeledAnnex{Results of CZ Tests}

The "CZ" tests are executed before any other ACATS tests to ensure that the
ImpDef packages have been properly customized and that certain support units
are working properly. These tests are not considered as Passed or Failed. If
they do not perform as expected, the problems must be identified and resolved
before conformity testing can continue.

This Appendix presents sample results from executing the "CZ" tests. The
actual output will differ, at least in the time-stamp information.

@LabeledClause{Sample Output From CZ0004}

The following is the output from an execution of CZ0004 with a specific
implementation. Note that it contains failure messages (indicated by '*') that
are expected, as is the final FAILED report. Note also that certain report
lines depend on the customization of the ImpDef package, and will vary with the
implementation.

@begin{Example}
,.,. CZ00004 ACATS 3.0 07-12-14 14:46:13
---- CZ00004 Check that Impdef values have been supplied for the special
                needs annexes. Check that the routines in TCTouch work
                correctly.
   - CZ00004 TCTouch ACATS 2.5.
   * CZ00004 Assertion failed: Assertion Failed is expected.
   * CZ00004 Assertion failed: Assertion Failed is expected.
   * CZ00004 z should not equal Z Expecting: z Got: Z.
   - CZ00004 Three failure messages should have occurred so far.
   * CZ00004 Trace Overflow:
                xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
                xxxxxxxxxxxxxxxxxxxxxxxx.
   - CZ00004 A Trace Overflow message should have just occurred.
   - CZ00004 <><><><><> ANNEX VALIDATION STATUS <><><><><>.
   + CZ00004 Annex C validation: Annex C not supported.
   + CZ00004 Annex D validation: Annex D not supported.
   + CZ00004 Annex E validation: Annex E not supported.
   + CZ00004 Annex F validation: Annex F not supported.
   + CZ00004 Annex G validation: Annex G not supported.
   + CZ00004 Annex H validation: Annex H not supported.
   - CZ00004 <><><><><> IMPDEF <><><><><>.
   - CZ00004 Validating_Annex_C : FALSE.
   - CZ00004 Validating_Annex_D : FALSE.
   - CZ00004 Validating_Annex_E : FALSE.
   - CZ00004 Validating_Annex_F : FALSE.
   - CZ00004 Validating_Annex_G : FALSE.
   - CZ00004 Validating_Annex_H : FALSE.
   - CZ00004 Minimum_Task_Switch:           0.100000000
   - CZ00004 Switch_To_New_Task:           1.000000000
   - CZ00004 Clear_Ready_Queue:           5.000000000
   - CZ00004 Delay_For_Time_Past:           0.100000000
   - CZ00004 Time_Dependent_Reset:           0.300000000
   - CZ00004 Delay_Per_Random_Test:           1.000000000
   - CZ00004 Exceed_Time_Slice.
   - CZ00004 Non_State_String: By No Means A State.
   - CZ00004 External_Tag_Value: implementation_defined.
   - CZ00004 CD30005_1_Foreign_Address: present.
   - CZ00004 CD30005_1_External_Name: CD30005_1.
   - CZ00004 Max_Default_Alignment:  1.
   - CZ00004 Max_Linker_Alignment:  1.
   - CZ00004 CXB30130_External_Name: CXB30130.
   - CZ00004 CXB30131_External_Name: CXB30131.
**** CZ00004 FAILED ****************************.
@end{Example}


@LabeledClause{Sample Output From CZ1101A}

The following is the output of CZ1101A from a particular implementation. The
output of this test should have the same messages (except for the time-stamp)
and reported results as indicated here (including failure reports), and the
format of the output lines should be as described in the output text.

@begin{Example}
   - NO_NAME (CZ1101A) CHECK REPORT ROUTINES.
   - NO_NAME    INITIAL VALUES SHOULD BE 'NO_NAME' AND 'FAILED'.
**** NO_NAME FAILED ****************************.

,.,. PASS_TEST ACATS 3.0 07-12-14 14:47:42
---- PASS_TEST CHECKING 'TEST' AND 'RESULT' FOR 'PASSED'.
   - PASS_TEST THIS LINE IS EXACTLY 'MAX_LEN' LONG. ...5...60....5...70.
   - PASS_TEST THIS COMMENT HAS A WORD THAT SPANS THE FOLD POINT. THIS
                  COMMENT FITS EXACTLY ON TWO LINES. ..5...60....5...70.
   - PASS_TEST
                  THIS_COMMENT_IS_ONE_VERY_LONG_WORD_AND_SO_IT_SHOULD_BE
                  _SPLIT_AT_THE_FOLD_POINT.
==== PASS_TEST PASSED ============================.
   - NO_NAME CHECK THAT 'RESULT' RESETS VALUES TO 'NO_NAME' AND
                'FAILED'.
**** NO_NAME FAILED ****************************.

,.,. FAIL_TEST ACATS 3.0 07-12-14 14:47:42
---- FAIL_TEST CHECKING 'FAILED' AND 'RESULT' FOR 'FAILED'.
   * FAIL_TEST 'RESULT' SHOULD NOW BE 'FAILED'.
**** FAIL_TEST FAILED ****************************.

,.,. NA_TEST ACATS 3.0 07-12-14 14:47:42
---- NA_TEST CHECKING 'NOT-APPLICABLE'.
   + NA_TEST 'RESULT' SHOULD NOW BE 'NOT-APPLICABLE'.
++++ NA_TEST NOT-APPLICABLE ++++++++++++++++++++.

,.,. FAIL_NA_TEST ACATS 3.0 07-12-14 14:47:42
---- FAIL_NA_TEST CHECKING 'NOT_APPLICABLE', 'FAILED', 'NOT_APPLICABLE'.
   + FAIL_NA_TEST 'RESULT' BECOMES 'NOT-APPLICABLE'.
   * FAIL_NA_TEST 'RESULT' BECOMES 'FAILED'.
   + FAIL_NA_TEST CALLING 'NOT_APPLICABLE' DOESN'T CHANGE 'RESULT'.
**** FAIL_NA_TEST FAILED ****************************.

,.,. SPEC_NA_TEST ACATS 3.0 07-12-14 14:47:42
---- SPEC_NA_TEST CHECKING 'SPEC_ACT', 'NOT_APPLICABLE', 'SPEC_ACT'.
   ! SPEC_NA_TEST 'RESULT' BECOMES 'TENTATIVELY PASSED'.
   + SPEC_NA_TEST 'RESULT' BECOMES 'NOT APPLICABLE'.
   ! SPEC_NA_TEST CALLING 'SPECIAL_ACTION' DOESN'T CHANGE 'RESULT'.
++++ SPEC_NA_TEST NOT-APPLICABLE ++++++++++++++++++++.

,.,. SPEC_FAIL_TEST ACATS 3.0 07-12-14 14:47:42
---- SPEC_FAIL_TEST CHECKING 'SPEC_ACT', 'FAILED', 'SPEC_ACT'.
   ! SPEC_FAIL_TEST 'RESULT' BECOMES 'TENTATIVELY PASSED'.
   * SPEC_FAIL_TEST 'RESULT' BECOMES 'FAILED'.
   ! SPEC_FAIL_TEST CALLING 'SPECIAL_ACTION' DOESN'T CHANGE 'RESULT'.
**** SPEC_FAIL_TEST FAILED ****************************.

,.,. CZ1101A ACATS 3.0 07-12-14 14:47:42
---- CZ1101A CHECKING 'SPECIAL_ACTION' ALONE.
   ! CZ1101A 'RESULT' BECOMES 'TENTATIVELY PASSED'.
!!!! CZ1101A TENTATIVELY PASSED !!!!!!!!!!!!!!!!.
!!!!         SEE '!' COMMENTS FOR SPECIAL NOTES!!
@end{Example}


@LabeledClause{Sample Output From CZ1102A}

Test CZ1102A should execute and report PASSED as illustrated below. Only the
time-stamp should differ.

@begin{Example}
,.,. CZ1102A ACATS 3.0 07-12-14 14:47:57
---- CZ1102A CHECK THAT THE DYNAMIC VALUE ROUTINES OF THE REPORT PACKAGE
                WORK CORRECTLY.
==== CZ1102A PASSED ============================.
@end{Example}


@LabeledClause{Sample Output From CZ1103A}

Test CZ1103A may produce two different forms of output, depending on whether
the implementation supports external files.

@LabeledSubClause{Output When External Files Are Supported}

If the implementation under test supports the creation and use of external text
files, then test CZ1103A should produce the following report (except for
differences in the time stamp). Note that failure messages are expected.

@begin{Example}
,.,. CZ1103A ACATS 3.0 07-12-14 14:48:10
---- CZ1103A CHECK THAT PROCEDURE CHECK_FILE WORKS.
   - CZ1103A BEGIN TEST WITH AN EMPTY FILE.
   - CZ1103A BEGIN TEST WITH A FILE WITH BLANK LINES.
   - CZ1103A BEGIN TEST WITH A FILE WITH BLANK LINES AND PAGES.
   - CZ1103A BEGIN TEST WITH A FILE WITH TRAILING BLANKS.
   - CZ1103A FROM CHECK_FILE: THIS IMPLEMENTATION PADS LINES WITH
                BLANKS.
   - CZ1103A BEGIN TEST WITH A FILE WITHOUT TRAILING BLANKS.
   - CZ1103A BEGIN TEST WITH A FILE WITH AN END OF LINE ERROR.
   * CZ1103A FROM CHECK_FILE: END OF LINE EXPECTED - E ENCOUNTERED.
   - CZ1103A FROM CHECK_FILE: LAST CHARACTER IN FOLLOWING STRING
                REVEALED ERROR: THIS LINE WILL CONTAIN AN #.
   - CZ1103A BEGIN TEST WITH FILE WITH END OF PAGE ERROR.
   * CZ1103A FROM CHECK_FILE: END_OF_PAGE NOT WHERE EXPECTED.
   - CZ1103A FROM CHECK_FILE: LAST CHARACTER IN FOLLOWING STRING
                REVEALED ERROR: THIS LINE WILL CONTAIN AN @@.
   - CZ1103A BEGIN TEST WITH FILE WITH END OF FILE ERROR.
   * CZ1103A FROM CHECK_FILE: END_OF_FILE NOT WHERE EXPECTED.
   - CZ1103A FROM CHECK_FILE: LAST CHARACTER IN FOLLOWING STRING
                REVEALED ERROR: THIS LINE WILL CONTAIN AN %.
   - CZ1103A BEGIN TEST WITH FILE WITH INCORRECT DATA.
   * CZ1103A FROM CHECK_FILE: FILE DOES NOT CONTAIN CORRECT OUTPUT -
                EXPECTED C - GOT I.
   - CZ1103A FROM CHECK_FILE: LAST CHARACTER IN FOLLOWING STRING
                REVEALED ERROR: LINE WITH C.
**** CZ1103A FAILED ****************************.

,.,. CZ1103A ACATS 3.0 07-12-14 14:48:10
---- CZ1103A THE LINE ABOVE SHOULD REPORT FAILURE.
   ! CZ1103A COMPARE THIS OUTPUT TO THE EXPECTED RESULT.
!!!! CZ1103A TENTATIVELY PASSED !!!!!!!!!!!!!!!!.
!!!!         SEE '!' COMMENTS FOR SPECIAL NOTES!!
@end{Example}


@LabeledSubClause{Output When External Files Are Not Supported}

If the implementation under test does not support external text files, then
CZ1103A produces different output, as illustrated below.

@begin{Example}
,.,. CZ1103A ACATS 3.0 07-12-14 14:49:00
---- CZ1103A CHECK THAT PROCEDURE CHECK_FILE WORKS.
   - CZ1103A BEGIN TEST WITH AN EMPTY FILE.
   * CZ1103A TEST WITH EMPTY FILE INCOMPLETE.
   - CZ1103A BEGIN TEST WITH A FILE WITH BLANK LINES.
   * CZ1103A TEST WITH FILE WITH BLANK LINES INCOMPLETE.
   - CZ1103A BEGIN TEST WITH A FILE WITH BLANK LINES AND PAGES.
   * CZ1103A TEST WITH FILE WITH BLANK PAGES INCOMPLETE.
   - CZ1103A BEGIN TEST WITH A FILE WITH TRAILING BLANKS.
   * CZ1103A TEST WITH FILE WITH TRAILING BLANKS INCOMPLETE.
   - CZ1103A BEGIN TEST WITH A FILE WITHOUT TRAILING BLANKS.
   * CZ1103A TEST WITH FILE WITHOUT TRAILING BLANKS INCOMPLETE.
   - CZ1103A BEGIN TEST WITH A FILE WITH AN END OF LINE ERROR.
   * CZ1103A TEST WITH END_OF_LINE ERROR INCOMPLETE.
   - CZ1103A BEGIN TEST WITH FILE WITH END OF PAGE ERROR.
   * CZ1103A TEST WITH END_OF_PAGE ERROR INCOMPLETE.
   - CZ1103A BEGIN TEST WITH FILE WITH END OF FILE ERROR.
   * CZ1103A TEST WITH END_OF_FILE ERROR INCOMPLETE.
   - CZ1103A BEGIN TEST WITH FILE WITH INCORRECT DATA.
   * CZ1103A TEST WITH INCORRECT DATA INCOMPLETE.
**** CZ1103A FAILED ****************************.

,.,. CZ1103A ACATS 3.0 07-12-14 14:49:00
---- CZ1103A THE LINE ABOVE SHOULD REPORT FAILURE.
   ! CZ1103A COMPARE THIS OUTPUT TO THE EXPECTED RESULT.
!!!! CZ1103A TENTATIVELY PASSED !!!!!!!!!!!!!!!!.
!!!!         SEE '!' COMMENTS FOR SPECIAL NOTES!!
@end{Example}
