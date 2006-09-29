@Part(statements, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/statements.mss,v $}
@comment{$Revision: 1.1 $ $Date: 2006/09/28 05:12:00 $}


@LabeledSection{package Asis.Statements}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Statements]}Asis.Statements
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Statements]}Asis.Statements @key[is]}]}

Asis.Statements encapsulates a set of queries that operate on A_Statement,
A_Path, and An_Exception_Handler elements.

@LabeledClause{function Label_Names}


    @key[function] @AdaSubDefn{Label_Names} (Statement : @key[in] Asis.Statement)
                         @key[return] Asis.Defining_Name_List;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the statement to query

Returns label_statement_identifier elements (A_Defining_Name elements) that
define the labels attached to the statement, in their order of appearance.

Returns a Nil_Element_List if there are no labels attached to the statement.

The Enclosing_Element of the A_Defining_Name elements is the statement.

Appropriate Element_Kinds:
     A_Statement

Returns Defining_Name_Kinds:
     A_Defining_Identifier

@LabeledClause{function Assignment_Variable_Name}


    @key[function] @AdaSubDefn{Assignment_Variable_Name} (Statement : @key[in] Asis.Statement)
                                      @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the assignment statement to query

Returns the expression that names the left hand side of the assignment.

Appropriate Element_Kinds:
     A_Statement

Appropriate Statement_Kinds:
     An_Assignment_Statement

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Assignment_Expression}


    @key[function] @AdaSubDefn{Assignment_Expression} (Statement : @key[in] Asis.Statement)
                                   @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the assignment statement to query

Returns the expression from the right hand side of the assignment.

Appropriate Element_Kinds:
     A_Statement

Appropriate Statement_Kinds:
     An_Assignment_Statement

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Statement_Paths}


    @key[function] @AdaSubDefn{Statement_Paths} (Statement : @key[in] Asis.Statement;
                              Include_Pragmas : @key[in] Boolean := False)
                              @key[return] Asis.Path_List;

Statement       @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the statement to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of the execution paths of the statement, in
their order of appearance.

The only pragmas returned are those preceding the first alternative in
a case statement.

Appropriate Statement_Kinds:
     An_If_Statement
     A_Case_Statement
     A_Selective_Accept_Statement
     A_Timed_Entry_Call_Statement
     A_Conditional_Entry_Call_Statement
     An_Asynchronous_Select_Statement

Returns Element_Kinds:
     A_Path
     A_Pragma

@LabeledClause{function Condition_Expression}


    @key[function] @AdaSubDefn{Condition_Expression} (Path : @key[in] Asis.Path)
                                  @key[return] Asis.Expression;

Path @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the execution path to query

Returns the condition expression for an IF path or an ELSIF path.

Appropriate Path_Kinds:
     An_If_Path
     An_Elsif_Path

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Sequence_Of_Statements}


    @key[function] @AdaSubDefn{Sequence_Of_Statements} (Path            : @key[in] Asis.Path;
                                     Include_Pragmas : @key[in] Boolean := False)
                                     @key[return] Asis.Statement_List;

Path            @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the execution path to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of the statements and pragmas from an execution path,
in their order of appearance.

Appropriate Element_Kinds:
     A_Path

Returns Element_Kinds:
     A_Statement
     A_Pragma

@LabeledClause{function Case_Expression}


    @key[function] @AdaSubDefn{Case_Expression} (Statement : @key[in] Asis.Statement)
                              @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the case statement to query

Returns the expression of the case statement that determines which
execution path is taken.

Appropriate Element_Kinds:
     A_Statement

Appropriate Statement_Kinds:
     A_Case_Statement

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Case_Statement_Alternative_Choices}


    @key[function] @AdaSubDefn{Case_Statement_Alternative_Choices} (Path : @key[in] Asis.Path)
                                                 @key[return] Asis.Element_List;

Path @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the case_statement_alternative execution path to query

Returns a list of the ‘when <choice> | <choice>' elements, in their
order of appearance.

Appropriate Path_Kinds:
     A_Case_Path

Returns Element_Kinds:
     An_Expression
     A_Definition

Returns Definition_Kinds:
     A_Discrete_Range
     An_Others_Choice

@LabeledClause{function Statement_Identifier}


    @key[function] @AdaSubDefn{Statement_Identifier} (Statement : @key[in] Asis.Statement)
                                   @key[return] Asis.Defining_Name;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the statement to query

Returns the identifier for the loop_statement or block_statement.

Returns a Nil_Element if the loop has no identifier.

The Enclosing_Element of the name is the statement.

Appropriate Statement_Kinds:
     A_Loop_Statement
     A_While_Loop_Statement
     A_For_Loop_Statement
     A_Block_Statement

Returns Defining_Name_Kinds:
     Not_A_Defining_Name
     A_Defining_Identifier

@LabeledClause{function Is_Name_Repeated (statement)}


    @key[function] @AdaSubDefn{Is_Name_Repeated} (Statement : @key[in] Asis.Statement) @key[return] Boolean;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the statement to query

Returns True if the name of the accept, loop, or block is repeated after
the end of the statement. Always returns True for loop or block
statements since the name is required.

Returns False for any unexpected Element.

Expected Statement_Kinds:
     A_Block_Statement
     A_Loop_Statement
     An_Accept_Statement

@LabeledClause{function While_Condition}


    @key[function] @AdaSubDefn{While_Condition} (Statement : @key[in] Asis.Statement)
                              @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the loop statement to query

Returns the condition expression associated with the while loop.

Appropriate Element_Kinds:
     A_Statement

Appropriate Statement_Kinds:
     A_While_Loop_Statement

Returns Element_Kinds:
     An_Expression

@LabeledClause{function For_Loop_Parameter_Specification}


    @key[function] @AdaSubDefn{For_Loop_Parameter_Specification} (Statement : @key[in] Asis.Statement)
                                               @key[return] Asis.Declaration;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the loop statement to query

Returns the declaration of the A_Loop_Parameter_Specification.

Appropriate Statement_Kinds:
     A_For_Loop_Statement

Returns Declaration_Kinds:
     A_Loop_Parameter_Specification


@LabeledClause{function Loop_Statements}


    @key[function] @AdaSubDefn{Loop_Statements} (Statement       : @key[in] Asis.Statement;
                              Include_Pragmas : @key[in] Boolean := False)
                              @key[return] Asis.Statement_List;

Statement       @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the loop statement to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns the sequence_of_statements and any pragmas from the loop_statement,
in their order of appearance.

Appropriate Statement_Kinds:
     A_Loop_Statement
     A_While_Loop_Statement
     A_For_Loop_Statement

Returns Element_Kinds:
     A_Pragma
     A_Statement

@LabeledClause{function Is_Declare_Block}


    @key[function] @AdaSubDefn{Is_Declare_Block} (Statement : @key[in] Asis.Statement) @key[return] Boolean;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the statement to query

Returns True if the statement is a block_statement and it was created with
the use of the "declare" reserved word. The presence or absence of any
declarative_item elements is not relevant.

Returns False if the "declare" reserved word does not appear in the
block_statement, or for any unexpected Element.

Expected Statement_Kinds::
     A_Block_Statement

@LabeledClause{function Block_Declarative_Items}


    @key[function] @AdaSubDefn{Block_Declarative_Items}
            (Statement       : @key[in] Asis.Statement;
                Include_Pragmas : @key[in] Boolean := False)
                @key[return] Asis.Declarative_Item_List;

Statement       @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the block statement to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of the declarations, representation_clause elements, pragmas,
and use_clause elements in the declarative_part of the block_statement, in their
order of appearance.

Returns a Nil_Element_List if there are no declarative items.

Appropriate Statement_Kinds:
     A_Block_Statement

Returns Element_Kinds:
     A_Declaration
     A_Pragma
     A_Clause


@LabeledClause{function Block_Statements}


    @key[function] @AdaSubDefn{Block_Statements} (Statement       : @key[in] Asis.Statement;
                               Include_Pragmas : @key[in] Boolean := False)
                               @key[return] Asis.Statement_List;

Statement       @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the block statement to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of the statements and pragmas for the block_statement, in
their order of appearance.

Returns a Nil_Element_List if there are no statements or pragmas. This
can only occur for a block_statement obtained from the obsolescent query
Body_Block_Statement when its argument is a package_body
that has no sequence_of_statements.

Appropriate Statement_Kinds:
     A_Block_Statement

Returns Element_Kinds:
     A_Pragma
     A_Statement

@LabeledClause{function Block_Exception_Handlers}


    @key[function] @AdaSubDefn{Block_Exception_Handlers} (Statement : @key[in] Asis.Statement;
                                       Include_Pragmas : @key[in] Boolean := False)
                                       @key[return] Asis.Exception_Handler_List;

Statement       @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the block statement to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of the exception_handler elements of the block_statement, in their
order of appearance.

The only pragmas returned are those following the reserved word "exception"
and preceding the reserved word “when” of first exception handler.

Returns a Nil_Element_List if there are no exception_handler elements.

Appropriate Statement_Kinds:
     A_Block_Statement

Returns Element_Kinds:
     An_Exception_Handler
     A_Pragma

@LabeledClause{function Exit_Loop_Name}


    @key[function] @AdaSubDefn{Exit_Loop_Name} (Statement : @key[in] Asis.Statement)
                             @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the exit statement to query

Returns the name of the exited loop.

Returns a Nil_Element if no loop name is present.

Appropriate Statement_Kinds:
     An_Exit_Statement

Returns Expression_Kinds:
     Not_An_Expression
     An_Identifier
     A_Selected_Component

@LabeledClause{function Exit_Condition}


    @key[function] @AdaSubDefn{Exit_Condition} (Statement : @key[in] Asis.Statement)
                             @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the exit statement to query

Returns the "when" condition of the exit statement.

Returns a Nil_Element if no condition is present.

Appropriate Statement_Kinds:
     An_Exit_Statement

Returns Element_Kinds:
     Not_An_Element
     An_Expression

@LabeledClause{function Corresponding_Loop_Exited}


    @key[function] @AdaSubDefn{Corresponding_Loop_Exited} (Statement : @key[in] Asis.Statement)
                                        @key[return] Asis.Statement;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the exit statement to query

Returns the loop statement exited by the exit statement.

Appropriate Statement_Kinds:
     An_Exit_Statement

Returns Element_Kinds:
     A_Loop_Statement
     A_While_Loop_Statement
     A_For_Loop_Statement

@LabeledClause{function Return_Expression}

    @key[function] @AdaSubDefn{Return_Expression} (Statement : @key[in] Asis.Statement)
                                @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the return statement to query

Returns the expression in the return statement.

Returns a Nil_Element if no expression is present.

Appropriate Statement_Kinds:
     A_Return_Statement

Returns Element_Kinds:
     Not_An_Element
     An_Expression


@LabeledClause{function Goto_Label}


    @key[function] @AdaSubDefn{Goto_Label} (Statement : @key[in] Asis.Statement)
                         @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the goto statement to query

Returns the expression reference for the label, as specified by the goto
statement.

Appropriate Statement_Kinds:
     A_Goto_Statement

Returns Expression_Kinds:
     An_Identifier

@LabeledClause{function Corresponding_Destination_Statement}


    @key[function] @AdaSubDefn{Corresponding_Destination_Statement}
       (Statement : @key[in] Asis.Statement)
        @key[return] Asis.Statement;

Statement  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the goto statement to query

Returns the target statement specified by the goto statement.

Appropriate Statement_Kinds:
     A_Goto_Statement

Returns Element_Kinds:
     A_Statement

@b{APPLICATION NOTE}

The Reference Manual allows a pragma between a statement and a label attached
to it. If so, when the label is passed as an actual parameter to
this query, the query returns the statement, but not the label. The only way
for an application to know that there are any pragmas between a statement
and its label is to get the spans of these program elements and analyze the
corresponding positions in the source text.

@LabeledClause{function Called_Name}


    @key[function] @AdaSubDefn{Called_Name} (Statement : @key[in] Asis.Statement)
                          @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the procedure call or entry call statement to query

Returns the name of the called procedure or entry. The name of an entry
family takes the form of An_Indexed_Component.

Appropriate Statement_Kinds:
     An_Entry_Call_Statement
     A_Procedure_Call_Statement

Returns Element_Kinds:
     An_Expression


@LabeledClause{function Corresponding_Called_Entity}


    @key[function] @AdaSubDefn{Corresponding_Called_Entity} (Statement : @key[in] Asis.Statement)
                                          @key[return] Asis.Declaration;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the procedure_call_statement or
              entry_call_statement to query

Returns the declaration of the procedure or entry denoted by the call.

Returns a Nil_Element if the:

- prefix of the call denotes an access to a procedure implicit
  or explicit dereference,

- argument is a dispatching call,

- argument is a call to a dispatching operation of a tagged type which
  is not statically determined.

If procedure_prefix denotes an attribute_reference, and if the corresponding
attribute is (re)defined by an attribute definition clause, an implementation
is encouraged, but not required, to return the definition of the corresponding
subprogram whose name is used after “use” in this attribute definition
clause. If an implementation cannot return such a subprogram definition, a
Nil_Element should be returned. For an attribute reference which is not
(re)defined by an attribute definition clause, a Nil_Element should be returned.

Appropriate Statement_Kinds:
     An_Entry_Call_Statement
     A_Procedure_Call_Statement

Returns Declaration_Kinds:
     Not_A_Declaration
     A_Procedure_Declaration
     A_Procedure_Body_Declaration
     A_Procedure_Body_Stub
     A_Procedure_Renaming_Declaration
     A_Procedure_Instantiation
     A_Formal_Procedure_Declaration
     An_Entry_Declaration
     A_Generic_Procedure_Declaration

@b{Implementation Permissions}

An implementation may choose to return any part of multi-part
declarations and definitions. Multi-part declaration/definitions
can occur for:

   - Subprogram specification in package specification, package body,
     and subunits (is separate);
   - Entries in package specification, package body, and subunits
     (is separate);
   - Private type and full type declarations;
   - Incomplete type and full type declarations; and
   - Deferred constant and full constant declarations.

No guarantee is made that the element will be the first part or
that the determination will be made due to any visibility rules.
An application should make its own analysis for each case based
on which part is returned.


@LabeledClause{function Call_Statement_Parameters}


    @key[function] @AdaSubDefn{Call_Statement_Parameters} (Statement  : @key[in] Asis.Statement;
                                        Normalized : @key[in] Boolean := False)
                                        @key[return] Asis.Association_List;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the procedure_call_statement or
              entry_call_statement to query
Normalized  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether the normalized form is desired

Returns a list of parameter_association elements of the call.

Returns a Nil_Element_List if there are no parameter_association elements.

An unnormalized list contains only explicit associations ordered as they
appear in the program text. Each unnormalized association has an optional
formal_parameter_selector_name and an explicit_actual_parameter component.

A normalized list contains artificial associations representing all
explicit and default associations. It has a length equal to the number of
parameter_specification elements of the formal_part of the
parameter_and_result_profile. The order of normalized associations matches
the order of parameter_specification elements.

Each normalized association represents a one on one mapping of a
parameter_specification elements to the explicit or default expression.
A normalized association has one A_Defining_Name component that denotes the
parameter_specification, and one An_Expression component that is either the
explicit_actual_parameter or a default_expression.

If the prefix of the call denotes an access to a procedure implicit or
explicit deference, normalized associations are constructed on the basis
of the formal_part of the parameter_profile from the corresponding
access_to_subprogram definition.

Returns Nil_Element for normalized associations in the case where
the called procedure can be determined only dynamically (dispatching
calls). ASIS cannot produce any meaningful result in this case.

The exception ASIS_Inappropriate_Element is raised when the procedure
call is an attribute reference and Is_Normalized is True.

Appropriate Statement_Kinds:
     An_Entry_Call_Statement
     A_Procedure_Call_Statement

Returns Element_Kinds:
     A_Parameter_Association

@b{Implementation Requirement}s

Normalized associations are Is_Normalized and Is_Part_Of_Implicit.
Normalized associations provided by default are Is_Defaulted_Association.
Normalized associations are never Is_Equal to unnormalized associations.

@b{Implementation Permissions}

An implementation may choose to always include default parameters in its
internal representation.

An implementation may also choose to normalize its representation
to use defining_identifier elements rather than formal_parameter_selector_name
elements.

In either case, this query will return Is_Normalized associations even if
Normalized is False, and the query Call_Statement_Parameters_Normalized
will return True.


@LabeledClause{function Accept_Entry_Index}


    @key[function] @AdaSubDefn{Accept_Entry_Index} (Statement : @key[in] Asis.Statement)
                                 @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the accept statement to query

Returns the entry index expression in the accept statement.

Returns a Nil_Element if the statement has no explicit entry index,

Appropriate Statement_Kinds:
     An_Accept_Statement

Returns Element_Kinds:
     Not_An_Element
     An_Expression

@LabeledClause{function Accept_Entry_Direct_Name}


    @key[function] @AdaSubDefn{Accept_Entry_Direct_Name} (Statement : @key[in] Asis.Statement)
                                       @key[return] Asis.Name;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the accept statement to query

Returns the direct name of the entry. The name follows the reserved word "accept".

Appropriate Statement_Kinds:
     An_Accept_Statement

Returns Expression_Kinds:
     An_Identifier

@LabeledClause{function Accept_Parameters}


    @key[function] @AdaSubDefn{Accept_Parameters} (Statement : @key[in] Asis.Statement)
                                @key[return] Asis.Parameter_Specification_List;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the accept statement to query

Returns a list of parameter specifications in the formal part of the accept
statement, in their order of appearance.

Returns a Nil_Element_List if the accept_statement has no parameters.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multiple name parameter specifications into an
equivalent sequence of corresponding single name parameter specifications.
See Reference Manual 3.3.1(7).

Appropriate Statement_Kinds:
     An_Accept_Statement

Returns Declaration_Kinds:
     A_Parameter_Specification


@LabeledClause{function Accept_Body_Statements}

    @key[function] @AdaSubDefn{Accept_Body_Statements} (Statement       : @key[in] Asis.Statement;
                                     Include_Pragmas : @key[in] Boolean := False)
                                     @key[return] Asis.Statement_List;

Statement       @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the accept statement to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns the list of statements and pragmas from the body of the accept
statement, in their order of appearance.

Appropriate Statement_Kinds:
     An_Accept_Statement

Returns Element_Kinds:
     A_Pragma
     A_Statement

@LabeledClause{function Accept_Body_Exception_Handlers}


    @key[function] @AdaSubDefn{Accept_Body_Exception_Handlers}
                 (Statement       : @key[in] Asis.Statement;
                  Include_Pragmas : @key[in] Boolean := False)
                  @key[return] Asis.Statement_List;

Statement       @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the accept statement to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns the list of exception handlers and pragmas from the body of the
accept statement, in their order of appearance.

Appropriate Statement_Kinds:
     An_Accept_Statement

Returns Element_Kinds:
     A_Pragma
     An_Exception_Handler

@LabeledClause{function Corresponding_Entry}


    @key[function] @AdaSubDefn{Corresponding_Entry} (Statement : @key[in] Asis.Statement)
                                  @key[return] Asis.Declaration;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the accept statement to query

Returns the declaration of the entry accepted in this statement.

Appropriate Statement_Kinds:
     An_Accept_Statement

Returns Declaration_Kinds:
     An_Entry_Declaration


@LabeledClause{function Requeue_Entry_Name}


    @key[function] @AdaSubDefn{Requeue_Entry_Name} (Statement : @key[in] Asis.Statement)
                                 @key[return] Asis.Name;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the requeue statement to query

Returns the name of the entry requeued by the statement.
The name follows the reserved word "requeue".

Appropriate Statement_Kinds:
     A_Requeue_Statement
     A_Requeue_Statement_With_Abort

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Delay_Expression}


    @key[function] @AdaSubDefn{Delay_Expression} (Statement : @key[in] Asis.Statement)
                               @key[return] Asis.Expression;

Statement   @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the delay statement to query

Returns the expression for the duration of the delay.

Appropriate Statement_Kinds:
     A_Delay_Until_Statement
     A_Delay_Relative_Statement

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Guard}


    @key[function] @AdaSubDefn{Guard} (Path : @key[in] Asis.Path)
                    @key[return] Asis.Expression;

Path @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the select statement execution path to query

Returns the conditional expression guard for the path.

Returns a Nil_Element if there is no guard, or if the path is from a
timed_entry_call, a conditional_entry_call, or an asynchronous_select
statement where a guard is not legal.

Appropriate Path_Kinds:
     A_Select_Path
     An_Or_Path

Returns Element_Kinds:
     Not_An_Element
     An_Expression


@LabeledClause{function Aborted_Tasks}


    @key[function] @AdaSubDefn{Aborted_Tasks} (Statement : @key[in] Asis.Statement)
                            @key[return] Asis.Expression_List;

Statement    @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the abort statement to query

Returns a list of the task names from the ABORT statement, in their order
of appearance.

Appropriate Statement_Kinds:
     An_Abort_Statement

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Choice_Parameter_Specification}


    @key[function] @AdaSubDefn{Choice_Parameter_Specification}
       (Handler : @key[in] Asis.Exception_Handler)
        @key[return] Asis.Declaration;

Handler @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the exception handler to query

Returns the choice parameter specification following the reserved word "when"
in the exception handler.

Returns a Nil_Element if there is no explicit choice parameter.

Appropriate Element_Kinds:
   An_Exception_Handler

Returns Declaration_Kinds:
     Not_A_Declaration
     A_Choice_Parameter_Specification

@LabeledClause{function Exception_Choices}


    @key[function] @AdaSubDefn{Exception_Choices} (Handler : @key[in] Asis.Exception_Handler)
                                @key[return] Asis.Element_List;

Handler @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the exception handler to query

Returns a list of the 'when <choice> | <choice>' elements, in their
order of appearance. Choices are either the exception name expression or
an others choice.

Appropriate Element_Kinds:
   An_Exception_Handler

Returns Expression_Kinds:
     An_Identifier
     A_Selected_Component

Returns Definition_Kinds:
     An_Others_Choice


@LabeledClause{function Handler_Statements}


    @key[function] @AdaSubDefn{Handler_Statements} (Handler         : @key[in] Asis.Exception_Handler;
                                 Include_Pragmas : @key[in] Boolean := False)
                                 @key[return] Asis.Statement_List;

Handler         @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the exception handler to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns the list of statements and pragmas from the body of the
exception handler, in their order of appearance.

Appropriate Element_Kinds:
   An_Exception_Handler

Returns Element_Kinds:
     A_Pragma
     A_Statement

@LabeledClause{function Raised_Exception}


    @key[function] @AdaSubDefn{Raised_Exception} (Statement : @key[in] Asis.Statement)
                               @key[return] Asis.Expression;

Statement @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the raise statement to query

Returns the expression that names the raised exception.

Returns a Nil_Element if there is no explicitly named exception.

Appropriate Statement_Kinds:
     A_Raise_Statement

Returns Expression_Kinds:
     Not_An_Expression
     An_Identifier
     A_Selected_Component

@LabeledClause{function Qualified_Expression}


    @key[function] @AdaSubDefn{Qualified_Expression} (Statement : @key[in] Asis.Statement)
                                   @key[return] Asis.Expression;

Statement  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the code statement to query

Returns the qualified aggregate expression representing the code statement.

Appropriate Statement_Kinds:
     A_Code_Statement

Returns Expression_Kinds:
     A_Qualified_Expression


@LabeledClause{function Is_Dispatching_Call}


    @key[function] @AdaSubDefn{Is_Dispatching_Call} (Call : @key[in] Asis.Element) @key[return] Boolean;

Call @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.

Returns True if the controlling tag of Call is dynamically determined.

This function shall always return False when pragma Restrictions(No_Dispatch)
applies.

Returns False for any unexpected Element.

Expected Expression_Kinds:
   A_Function_Call

Expected Statement_Kinds:
   A_Procedure_Call_Statement

@LabeledClause{function Is_Call_On_Dispatching_Operation}


    @key[function] @AdaSubDefn{Is_Call_On_Dispatching_Operation} (Call : @key[in] Asis.Element)
                                              @key[return] Boolean;

Call @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the element to query.

Returns True if the name or prefix of Call denotes the declaration of a
primitive operation of a tagged type.

Returns False for any unexpected Element.

Expected Element_Kinds:
   A_Function_Call
   A_Procedure_Call_Statement

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Statements;]}
@end{Example}

