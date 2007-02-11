@Part(glossary, Root="asis.msm")

@Comment{$Date: 2007/02/11 07:19:09 $}
@LabeledAddedNormativeAnnex{Version=[2],Name=[Obsolescent Features]}

@comment{$Source: e:\\cvsroot/ARM/ASIS/obsolescent.mss,v $}
@comment{$Revision: 1.2 $}

@LabeledAddedClause{Version=[2],Name=[Annex Contents]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[@Defn{obsolescent feature}
This Annex contains descriptions of features of ASIS whose functionality is
largely redundant with other features
defined by this International Standard.
Use of these features is not recommended in newly written programs.]}
@end{Intro}

@LabeledAddedClause{Version=[2],Name=[Obsolescent Features in package Asis.Declarations]}

@LabeledAddedSubclause{Version=[2],Name=[Introduction for Obsolescent Features in package Asis.Declarations]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[The library package Asis.Declaration also shall
provide interfaces equivalent to the obsolescent ones described in the
following subclauses.]}

@LabeledAddedSubclause{Version=[2],Name=[function Body_Block_Statement]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[Function Body_Block_Statement is a query that
supplies the equivalent combined functionality of the replaced queries from an
earlier edition of this standard: Subprogram_Body_Block, Package_Body_Block,
and Task_Body_Block. Use of the query Body_Block_Statement is not recommended
in new programs. This functionality is redundant with the queries
Body_Declarative_Items, Body_Statements, and Body_Exception_Handlers.]}

@begin{DescribeCode}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[@key[function] @AdaSubDefn{Body_Block_Statement} (Declaration : @key[in] Asis.Declaration)
                              @key[return] Asis.Statement;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[Declaration specifies the program unit body to query.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[Returns a block statement that is the structural
equivalent of the body. The block statement is not Is_Part_Of_Implicit. The
block includes the declarative part, the sequence of statements, and any
exception handlers.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Expected Declaration_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Function_Body_Declaration
A_Procedure_Body_Declaration
A_Package_Body_Declaration
A_Task_Body_Declaration
An_Entry_Body_Declaration]}
@end{Display}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[Returns Statement_Kinds:]}
@begin{Display}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[A_Block_Statement]}
@end{Display}
@end{DescribeCode}

@begin{SingleNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[This function is never called by Traverse_Element.]}
@end{SingleNote}

@begin{UsageNote}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0027-1]}
@ChgAdded{Version=[2],Text=[This function is intended for compatibility with
ASIS 83.]}
@end{UsageNote}
