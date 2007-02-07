@Part(definitions, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/definitions.mss,v $}
@comment{$Revision: 1.3 $ $Date: 2007/02/06 06:21:04 $}


@LabeledSection{package Asis.Definitions}


@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis],Child=[Definitions]}Asis.Definitions
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis],Child=[Definitions]}Asis.Definitions @key[is]}]}


Asis.Definitions encapsulates a set of queries that operate on A_Definition
and An_Association elements.

@LabeledClause{function Corresponding_Type_Operators}

Element Reference -A_Type_Definition - 3.2.1
    @key[function] @AdaSubDefn{Corresponding_Type_Operators}
            (Type_Definition : @key[in] Asis.Type_Definition)
                @key[return] Asis.Declaration_List;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the type to query

Returns a list of operators. These include all predefined operators, and
all user-defined operator overloads, that have been implicitly or
explicitly declared for the type. (Reference Manual 7.3.1(2))

This list includes only operators appropriate for the type, from the set:
     and or xor = /= < <= > >= + - & * / mod rem ** abs not

Returns a Nil_Element_List if there are no predefined or overloaded
operators for the type.

Returns a Nil_Element_List if the implementation does not provide
such implicit declarations.

The Enclosing_Element for each implicit declaration is the declaration (type
or object) that declared the type.

For limited private types, if a user-defined equality operator has
been defined, an Ada implementation has two choices when dealing with an
instance of the "/=" operator. a) treat A/=B as NOT(A=B), b) implicitly
create a "/=" operator. Implementations that take the second alternative
will include this implicit inequality operation in their result.
Implementations that choose the first alternative are encouraged to hide
this choice beneath the ASIS interface and to "fake" an inequality
operation. Failing that, the function call, representing the NOT
operation, must have Is_Part_Of_Implicit = True so that an ASIS application
can tell the  difference between a user-specified NOT(A=B) and an
implementation-specific A/=B transformation.

Appropriate Definition_Kinds:
     A_Type_Definition
     A_Formal_Type_Definition

Returns Declaration_Kinds:
     A_Function_Declaration
     A_Function_Body_Declaration
     A_Function_Body_Stub
     A_Function_Renaming_Declaration
     A_Function_Instantiation
     A_Formal_Function_Declaration

@b{Implementation Permissions}

The result may or may not include language defined operators that have
been overridden by user-defined overloads. Operators that are totally
hidden, in all contexts, by user-defined operators may be omitted from
the list.

Some implementations do not represent all forms of implicit
declarations such that elements representing them can be easily
provided. An implementation can choose whether or not to construct
and provide artificial declarations for implicitly declared elements.
Element Reference -A_Derived_Type_Definition - 3.4

Child Elements returned by
   function Parent_Subtype_Indication

Element Reference -A_Derived_Record_Extension_Definition - 3.4

Child Elements returned by
   function Parent_Subtype_Indication
   function Record_Definition

@LabeledClause{function Parent_Subtype_Indication}


    @key[function] @AdaSubDefn{Parent_Subtype_Indication}
                  (Type_Definition : @key[in] Asis.Type_Definition)
                        @key[return] Asis.Subtype_Indication;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the derived_type_definition to query

Returns the parent_subtype_indication following the reserved word "new".

Appropriate Type_Kinds:
     A_Derived_Type_Definition
     A_Derived_Record_Extension_Definition

Returns Definition_Kinds:
     A_Subtype_Indication

@LabeledClause{function Record_Definition}


    @key[function] @AdaSubDefn{Record_Definition} (Type_Definition : @key[in] Asis.Type_Definition)
                               @key[return] Asis.Definition;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns the record definition of the type_definition.

Appropriate Type_Kinds:
     A_Derived_Record_Extension_Definition
     A_Record_Type_Definition
     A_Tagged_Record_Type_Definition

Returns Definition_Kinds:
     A_Record_Definition
     A_Null_Record_Definition

@LabeledClause{function Implicit_Inherited_Declarations}


    @key[function] @AdaSubDefn{Implicit_Inherited_Declarations}
                  (Definition : @key[in] Asis.Definition)
                      @key[return] Asis.Declaration_List;

Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the derived type to query

Returns a list of Is_Part_Of_Implicit inherited enumeration literals,
discriminants, components, protected subprograms, or entries of a
derived_type_definition whose parent type is an enumeration type, or a
composite type other than an array type. See Reference Manual 3.4(10-14).

Returns a Nil_Element_List if the root type of derived_type_definition is
not an enumeration, record, task, or protected type.

Returns a Nil_Element_List if the implementation does not provide
such implicit declarations.

The Enclosing_Element for each of the implicit declarations is the
Declaration argument.

Appropriate Definition_Kinds:
     A_Type_Definition
     A_Private_Extension_Definition
     A_Formal_Type_Definition

Appropriate Type_Kinds:
     A_Derived_Type_Definition
     A_Derived_Record_Extension_Definition

Appropriate Formal_Type_Kinds:
     A_Formal_Derived_Type_Definition

Returns Declaration_Kinds:

     An_Enumeration_Literal_Specification
     A_Discriminant_Specification
     A_Component_Declaration
     A_Procedure_Declaration
     A_Function_Declaration
     An_Entry_Declaration

@b{Implementation Permissions}

Some implementations do not represent all forms of implicit
declarations such that elements representing them can be easily
provided. An implementation can choose whether or not to construct
and provide artificial declarations for implicitly declared elements.

@begin{UsageNote}
This query returns only implicit inherited entry declarations for
derived task types. All representation clauses and pragmas associated
with the entries of the original task type (the root type of the
derived task type) apply to the inherited entries. Those are available
by examining the original type or by calling Corresponding_Pragmas and
Corresponding_Representation_Clauses. These functions will return the
pragmas and clauses from the original type.
@end{UsageNote}

@LabeledClause{function Implicit_Inherited_Subprograms}


    @key[function] @AdaSubDefn{Implicit_Inherited_Subprograms}
                (Definition : @key[in] Asis.Definition)
                @key[return] Asis.Declaration_List;

Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the derived type to query

Returns the list of user-defined inherited primitive subprograms that have
been implicitly declared for the derived_type_definition.

The list result does not include hidden inherited subprograms (Reference Manual 8.3).

Returns a Nil_Element_List if there are no inherited subprograms for the
derived type.

Returns a Nil_Element_List if the implementation does not provide
such implicit declarations.

The Enclosing_Element for each of the subprogram declarations is the
Definition argument.

Appropriate Definition_Kinds:
     A_Type_Definition
     A_Private_Extension_Definition
     A_Formal_Type_Definition

Appropriate Type_Kinds:
     A_Derived_Type_Definition
     A_Derived_Record_Extension_Definition

Appropriate Formal_Type_Kinds:
     A_Formal_Derived_Type_Definition

Returns Declaration_Kinds:
     A_Function_Declaration
     A_Procedure_Declaration

@b{Implementation Permissions}

Some implementations do not represent all forms of implicit
declarations such that elements representing them can be easily
provided. An implementation can choose whether or not to construct
and provide artificial declarations for implicitly declared elements.

@LabeledClause{function Corresponding_Parent_Subtype}


     @key[function] @AdaSubDefn{Corresponding_Parent_Subtype}
                  (Type_Definition : @key[in] Asis.Type_Definition)
                      @key[return] Asis.Declaration;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the derived_type_definition to query

Returns the parent subtype declaration of the derived_type_definition.
The parent subtype is defined by the parent_subtype_indication.

Appropriate Type_Kinds:
     A_Derived_Type_Definition
     A_Derived_Record_Extension_Definition

Returns Declaration_Kinds:
     An_Ordinary_Type_Declaration
     A_Task_Type_Declaration
     A_Protected_Type_Declaration
     A_Subtype_Declaration
     A_Formal_Type_Declaration
     An_Incomplete_Type_Declaration
     A_Private_Type_Declaration
     A_Private_Extension_Declaration

@LabeledClause{function Corresponding_Root_Type}


    @key[function] @AdaSubDefn{Corresponding_Root_Type}
                     (Type_Definition : @key[in] Asis.Type_Definition)
                      @key[return] Asis.Declaration;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the derived_type_definition to query

This function recursively unwinds all type derivations and subtyping to
arrive at a full_type_declaration that is neither a derived type nor a
subtype.

In case of numeric types, this function always returns some user-defined
type, not an implicitly defined root type corresponding to
A_Root_Type_Definition. The only ways to get implicitly declared numeric
root or universal types are to ask for the type of a universal expression
or from the parameter and result profile of a predefined operation working
with numeric types.

Appropriate Type_Kinds:
     A_Derived_Type_Definition
     A_Derived_Record_Extension_Definition

Returns Declaration_Kinds:
     An_Ordinary_Type_Declaration
     A_Task_Type_Declaration
     A_Protected_Type_Declaration
     A_Formal_Type_Declaration
     A_Private_Type_Declaration
     A_Private_Extension_Declaration

@LabeledClause{function Corresponding_Type_Structure}


    @key[function] @AdaSubDefn{Corresponding_Type_Structure}
                  (Type_Definition : @key[in] Asis.Type_Definition)
                      @key[return] Asis.Declaration;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the derived_type_definition to query

Returns the type structure from which the specified type definition has
been derived. This function will recursively unwind derivations and
subtyping until the type_declaration derives a change of representation or
is no longer derived. See Reference Manual 13.6.

Appropriate Type_Kinds:
     A_Derived_Type_Definition
     A_Derived_Record_Extension_Definition

Returns Declaration_Kinds:
     An_Ordinary_Type_Declaration
     A_Task_Type_Declaration
     A_Protected_Type_Declaration
     A_Formal_Type_Declaration
     A_Private_Type_Declaration
     A_Private_Extension_Declaration
Element Reference -An_Enumeration_Type_Definition - 3.5.1

Child Elements returned by
   function Enumeration_Literal_Declarations

@LabeledClause{function Enumeration_Literal_Declarations}


    @key[function] @AdaSubDefn{Enumeration_Literal_Declarations}
                (Type_Definition : @key[in] Asis.Type_Definition)
                @key[return] Asis.Declaration_List;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the enumeration type definition to query

Returns a list of the literals declared in an enumeration_type_definition,
in their order of appearance.

Appropriate Type_Kinds:
     An_Enumeration_Type_Definition

Returns Declaration_Kinds:
     An_Enumeration_Literal_Specification
Element Reference -A_Signed_Integer_Type_Definition - 3.5.4

Child Elements returned by
   function Integer_Constraint


@LabeledClause{function Integer_Constraint}


    @key[function] @AdaSubDefn{Integer_Constraint}
                (Type_Definition : @key[in] Asis.Type_Definition)
                @key[return] Asis.Range_Constraint;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the signed_integer_type_definition to query

Returns the range_constraint of the signed_integer_type_definition.

Appropriate Type_Kinds:
     A_Signed_Integer_Type_Definition

Returns Constraint_Kinds:
     A_Simple_Expression_Range
Element Reference -A_Modular_Type_Definition - 3.5.4

Child Elements returned by
   function Mod_Static_Expression

@LabeledClause{function Mod_Static_Expression}


    @key[function] @AdaSubDefn{Mod_Static_Expression}
                (Type_Definition : @key[in] Asis.Type_Definition)
                @key[return] Asis.Expression;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the modular_type_definition to query

Returns the static_expression following the reserved word "mod".

Appropriate Type_Kinds:
     A_Modular_Type_Definition

Returns Element_Kinds:
     An_Expression
Element Reference -A_Floating_Point_Definition - 3.5.7

Child Elements returned by
   functions Digits_Expression and Real_Range_Constraint
Element Reference -A_Decimal_Fixed_Point_Definition - 3.5.9

Child Elements returned by
   functions Digits_Expression, Delta_Expression, and Real_Range_Constraint

@LabeledClause{function Digits_Expression}


    @key[function] @AdaSubDefn{Digits_Expression} (Definition : @key[in] Asis.Definition)
                               @key[return] Asis.Expression;

Definition  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns the static_expression following the reserved word "digits".

Appropriate Type_Kinds:
     A_Floating_Point_Definition
     A_Decimal_Fixed_Point_Definition

 Appropriate Definition_Kinds:
     A_Constraint
         Appropriate Constraint_Kinds:
              A_Digits_Constraint

Returns Element_Kinds:
     An_Expression
Element Reference -An_Ordinary_Fixed_Point_Definition - 3.5.9

Child Elements returned by
   function Delta_Expression

@LabeledClause{function Delta_Expression}


    @key[function] @AdaSubDefn{Delta_Expression} (Definition : @key[in] Asis.Definition)
                              @key[return] Asis.Expression;

Definition  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns the static_expression following the reserved word "delta".

Appropriate Type_Kinds:
     An_Ordinary_Fixed_Point_Definition
     A_Decimal_Fixed_Point_Definition

Appropriate Definition_Kinds:
     A_Constraint
         Appropriate Constraint_Kinds:
              A_Delta_Constraint

Returns Element_Kinds:
     An_Expression

@LabeledClause{function Real_Range_Constraint}


    @key[function] @AdaSubDefn{Real_Range_Constraint}
             (Definition : @key[in] Asis.Definition) @key[return] Asis.Range_Constraint;

Definition  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns the real_range_specification range_constraint of the definition.

Returns a Nil_Element if there is no explicit range_constraint.

Appropriate Type_Kinds:
     A_Floating_Point_Definition
     An_Ordinary_Fixed_Point_Definition
     A_Decimal_Fixed_Point_Definition

Appropriate Definition_Kinds:
     A_Constraint
         Appropriate Constraint_Kinds:
              A_Digits_Constraint
              A_Delta_Constraint

Returns Constraint_Kinds:
     Not_A_Constraint
     A_Simple_Expression_Range
Element Reference -An_Unconstrained_Array_Definition 3.6

Child Elements returned by
   functions Index_Subtype_Definitions and Array_Component_Definition


@LabeledClause{function Index_Subtype_Definitions}


    @key[function] @AdaSubDefn{Index_Subtype_Definitions}
                (Type_Definition : @key[in] Asis.Type_Definition)
                @key[return] Asis.Expression_List;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the array_type_definition to query

Returns a list of the index_subtype_definition subtype mark names for
an unconstrained_array_definition, in their order of appearance.

Appropriate Type_Kinds:
     An_Unconstrained_Array_Definition

Appropriate Formal_Type_Kinds:
     A_Formal_Unconstrained_Array_Definition

Returns Expression_Kinds:
     An_Identifier
     A_Selected_Component
Element Reference -A_Constrained_Array_Definition 3.6

Child Elements returned by
   function Discrete_Subtype_Definitions
   function Array_Component_Definition

@LabeledClause{function Discrete_Subtype_Definitions}


    @key[function] @AdaSubDefn{Discrete_Subtype_Definitions}
                        (Type_Definition : @key[in] Asis.Type_Definition)
                              @key[return] Asis.Definition_List;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the array_type_definition to query

Returns the list of Discrete_Subtype_Definition elements of a
constrained_array_definition, in their order of appearance.

Appropriate Type_Kinds:
     A_Constrained_Array_Definition

Appropriate Formal_Type_Kinds:
     A_Formal_Constrained_Array_Definition

Returns Definition_Kinds:
     A_Discrete_Subtype_Definition

@LabeledClause{function Array_Component_Definition}


    @key[function] @AdaSubDefn{Array_Component_Definition}
                (Type_Definition : @key[in] Asis.Type_Definition)
                @key[return] Asis.Component_Definition;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the array_type_definition to query

Returns the Component_Definition of the array_type_definition.

Appropriate Type_Kinds:
     An_Unconstrained_Array_Definition
     A_Constrained_Array_Definition

Appropriate Formal_Type_Kinds:
     A_Formal_Unconstrained_Array_Definition
     A_Formal_Constrained_Array_Definition

Returns Definition_Kinds:
     A_Component_Definition
Element Reference -A_Record_Type_Definition - 3.8
Element Reference -A_Tagged_Record_Type_Definition - 3.8

Child Elements returned by
   function Record_Definition

Element Reference -An_Access_Type_Definition - 3.10

Child Elements returned by
   function Access_To_Object_Definition
   function Access_To_Subprogram_Parameter_Profile
   function Access_To_Function_Result_Profile

@LabeledClause{function Access_To_Object_Definition}


    @key[function] @AdaSubDefn{Access_To_Object_Definition}
                   (Type_Definition : @key[in] Asis.Type_Definition)
                         @key[return] Asis.Subtype_Indication;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Access_Type_Definition to query

Returns the subtype_indication following the reserved word "access".

Appropriate Type_Kinds:
     An_Access_Type_Definition.

Appropriate Formal_Type_Kinds:
     A_Formal_Access_Type_Definition

Appropriate Access_Type_Kinds:
     A_Pool_Specific_Access_To_Variable
     An_Access_To_Variable
     An_Access_To_Constant

Returns Element_Kinds:
     A_Subtype_Indication

@LabeledClause{function Access_To_Subprogram_Parameter_Profile}

    @key[function] @AdaSubDefn{Access_To_Subprogram_Parameter_Profile}
                   (Type_Definition : @key[in] Asis.Type_Definition)
                         @key[return] Asis.Parameter_Specification_List;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Access_Type_Definition to query

Returns a list of parameter_specification elements in the formal part of the
parameter_profile in the access_to_subprogram_definition.

Returns a Nil_Element_List if the parameter_profile has no formal part.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multiple name parameter_specification elements into
an equivalent sequence of corresponding single name
parameter_specification elements. See Reference Manual 3.3.1(7).

Appropriate Type_Kinds:
     An_Access_Type_Definition.
     A_Formal_Access_Type_Definition.

Appropriate Access_Type_Kinds:
     An_Access_To_Procedure
     An_Access_To_Protected_Procedure
     An_Access_To_Function
     An_Access_To_Protected_Function

Returns Declaration_Kinds:
     A_Parameter_Specification

@LabeledClause{function Access_To_Function_Result_Profile}


    @key[function] @AdaSubDefn{Access_To_Function_Result_Profile}
                   (Type_Definition : @key[in] Asis.Type_Definition)
                         @key[return] Asis.Expression;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Access_Type_Definition to query

Returns the subtype_mark expression for the return type for the access
function.

Appropriate Type_Kinds:
     An_Access_Type_Definition
     A_Formal_Access_Type_Definition

Appropriate Access_Type_Kinds:
     An_Access_To_Function
     An_Access_To_Protected_Function

Returns Expression_Kinds:
     An_Identifier
     A_Selected_Component
Element Reference -A_Root_Type_Definition - 3.5.4(9), 3.5.6(2) - No child elements

Element Reference -A_Subtype_Indication - 3.3.2

Child Elements returned by
   function Subtype_Mark
   function Subtype_Constraint

@LabeledClause{function Subtype_Mark}



    @key[function] @AdaSubDefn{Subtype_Mark} (Definition : @key[in] Asis.Definition)
                          @key[return] Asis.Expression;

Definition  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns the subtype_mark expression of the definition.

Appropriate Definition_Kinds:
     A_Subtype_Indication
     A_Discrete_Subtype_Definition
         Appropriate Discrete_Range_Kinds:
              A_Discrete_Subtype_Indication
     A_Discrete_Range
         Appropriate Discrete_Range_Kinds:
              A_Discrete_Subtype_Indication
     A_Formal_Type_Definition
         Appropriate Formal_Type_Kinds:
              A_Formal_Derived_Type_Definition

Returns Expression_Kinds:
     An_Identifier
     A_Selected_Component
     An_Attribute_Reference


@LabeledClause{function Subtype_Constraint}


    @key[function] @AdaSubDefn{Subtype_Constraint} (Definition : @key[in] Asis.Definition)
                                @key[return] Asis.Constraint;

Definition  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns the constraint of the subtype_indication.

Returns a Nil_Element if no explicit constraint is present.

Appropriate Definition_Kinds:
     A_Subtype_Indication
     A_Discrete_Subtype_Definition
         Appropriate Discrete_Range_Kinds:
              A_Discrete_Subtype_Indication
     A_Discrete_Range
         Appropriate Discrete_Range_Kinds:
              A_Discrete_Subtype_Indication

Returns Definition_Kinds:
     Not_A_Definition
     A_Constraint

@begin{UsageNote}
When an unconstrained subtype indication for a type having
discriminants with default values is used, a Nil_Element is
returned by this function. Use the queries Subtype_Mark, and
Corresponding_Name_Declaration [, and Corresponding_First_Subtype]
to obtain the declaration defining the defaults.
@end{UsageNote}

@LabeledClause{function Lower_Bound}

@begin{ElementRef}
A_Constraint @em 3.2.2@*
A_Simple_Expression_Range @em 3.5
@end{ElementRef}
@begin{ChildRef}@ @;
@begin{Display}
function Lower_Bound
function Upper_Bound
@end{Display}
@end{ChildRef}

    @key[function] @AdaSubDefn{Lower_Bound} (Constraint : @key[in] Asis.Range_Constraint)
                         @key[return] Asis.Expression;

Constraint  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the range_constraint or discrete_range to query

Returns the simple_expression for the lower bound of the range.

Appropriate Constraint_Kinds:
     A_Simple_Expression_Range

Appropriate Discrete_Range_Kinds:
     A_Discrete_Simple_Expression_Range

Returns Element_Kinds:
     An_Expression


@LabeledClause{function Upper_Bound}


    @key[function] @AdaSubDefn{Upper_Bound} (Constraint : @key[in] Asis.Range_Constraint)
                         @key[return] Asis.Expression;

Constraint  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the range_constraint or discrete_range to query

Returns the simple_expression for the upper bound of the range.

Appropriate Constraint_Kinds:
     A_Simple_Expression_Range

Appropriate Discrete_Range_Kinds:
     A_Discrete_Simple_Expression_Range

Returns Element_Kinds:
     An_Expression


@LabeledClause{function Range_Attribute}

@begin{ElementRef}
A_Range_Attribute_Reference @em 3.5
@end{ElementRef}
@begin{ChildRef}@ @;
@begin{Display}
function Range_Attribute
@end{Display}
@end{ChildRef}


    @key[function] @AdaSubDefn{Range_Attribute} (Constraint : @key[in] Asis.Range_Constraint)
                             @key[return] Asis.Expression;

Constraint  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the range_attribute_reference or
              discrete_range attribute_reference to query

Returns the range_attribute_reference expression of the range.

Appropriate Constraint_Kinds:
     A_Range_Attribute_Reference

Appropriate Discrete_Range_Kinds:
     A_Discrete_Range_Attribute_Reference

Returns Expression_Kinds:
     An_Attribute_Reference
Element Reference -A_Digits_Constraint - 3.5.9

Child Elements returned by
   function Digits_Expression
   function Real_Range_Constraint

Element Reference -A_Delta_Constraint - J.3

Child Elements returned by
   function Delta_Expression
   function Real_Range_Constraint

Element Reference -An_Index_Constraint - 3.6.1

Child Elements returned by
   function Discrete_Ranges


@LabeledClause{function Discrete_Ranges}


    @key[function] @AdaSubDefn{Discrete_Ranges} (Constraint : @key[in] Asis.Constraint)
                             @key[return] Asis.Discrete_Range_List;

Constraint  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the array index_constraint to query

Returns the list of discrete_range components for an index_constraint,
in their order of appearance.

Appropriate Constraint_Kinds:
     An_Index_Constraint

Returns Definition_Kinds:
     A_Discrete_Range
Element Reference -A_Discriminant_Constraint - 3.7.1

Child Elements returned by
   function Discriminant_Associations

@LabeledClause{function Discriminant_Associations}


    @key[function] @AdaSubDefn{Discriminant_Associations}
                (Constraint : @key[in] Asis.Constraint;
                 Normalized : @key[in] Boolean := False)
                @key[return] Asis.Discriminant_Association_List;

Constraint  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the discriminant_constraint to query
Normalized  @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether the normalized form is desired

Returns a list of the discriminant_association elements of the
discriminant_constraint.

Returns a Nil_Element_List if there are no discriminant_association elements.

An unnormalized list contains only explicit associations ordered as they
appear in the program text. Each unnormalized association has a list of
discriminant_selector_name elements and an explicit expression.

A normalized list contains artificial associations representing all
explicit associations. It has a length equal to the number of
discriminant_specification elements of the known_discriminant_part. The order
of normalized associations matches the order of discriminant_specification elements.

Each normalized association represents a one on one mapping of a
discriminant_specification to the explicit expression. A normalized
association has one A_Defining_Name component that denotes the
discriminant_specification, and one An_Expression component that is the
explicit expression.

Appropriate Constraint_Kinds:
     A_Discriminant_Constraint

Returns Association_Kinds:
     A_Discriminant_Association

@b{Implementation Requirement}s

Normalized associations are Is_Normalized and Is_Part_Of_Implicit.
Normalized associations are never Is_Equal to unnormalized associations.


@b{Implementation Permissions}

An implementation may choose to normalize its internal representation
to use the defining_identifier element instead of the
discriminant_selector_name element.

If so, this query will return Is_Normalized associations even if
Normalized is False, and the query Discriminant_Associations_Normalized
will return True.

@begin{UsageNote}
It is not possible to obtain either a normalized or unnormalized
Discriminant_Association list for an unconstrained record or derived
subtype_indication where the discriminant_association elements are supplied
by default; there is no constraint to query, and a Nil_Element is
returned from the query Subtype_Constraint.
@end{UsageNote}

Element Reference -A_Component_Definition - 3.6

Child Elements returned by
   function Component_Subtype_Indication

@LabeledClause{function Component_Subtype_Indication}


    @key[function] @AdaSubDefn{Component_Subtype_Indication}
            (Component_Definition : @key[in] Asis.Component_Definition)
                @key[return] Asis.Subtype_Indication;

Component_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the Component_Definition to query

Returns the subtype_indication of the Component_Definition.

Appropriate Definition_Kinds:
     A_Component_Definition

Returns Definition_Kinds:
     A_Subtype_Indication
Element Reference -A_Discrete_Subtype_Definition - 3.6
Element Reference -A_Discrete_Range - 3.6.1

Element Reference -A_Discrete_Subtype_Indication

Child Elements returned by
   function Subtype_Mark
   function Subtype_Constraint

A_Discrete_Simple_Expression_Range

Child Elements returned by
   function Lower_Bound
   function Upper_Bound
Element Reference -A_Discrete_Range_Attribute_Reference - 3.5

Child Elements returned by
   function Range_Attribute

Element Reference -An_Unknown_Discriminant_Part - 3.7 - No child elements

Element Reference -A_Known_Discriminant_Part - 3.7

Child Elements returned by
   function Discriminants


@LabeledClause{function Discriminants}


    @key[function] @AdaSubDefn{Discriminants} (Definition : @key[in] Asis.Definition)
                           @key[return] Asis.Discriminant_Specification_List;

Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the known_discriminant_part to query

Returns a list of discriminant_specification elements, in their order of appearance.

Results of this query may vary across ASIS implementations. Some
implementations normalize all multi-name discriminant_specification elements into
an equivalent sequence of single name discriminant_specification elements.
See Reference Manual 3.3.1(7).

Appropriate Definition_Kinds:
     A_Known_Discriminant_Part

Returns Declaration_Kinds:
     A_Discriminant_Specification
Element Reference -A_Record_Definition - 3.8

Child Elements returned by
   function Record_Components
   function Implicit_Components

@LabeledClause{function Record_Components (definition)}


@key[function] @AdaSubDefn{Record_Components} (Definition : @key[in] Asis.Definition;
                                Include_Pragmas : @key[in] Boolean := False)
                                @key[return] Asis.Record_Component_List;

Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record_definition or variant to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of the components and pragmas of the record_definition or
variant, in their order of appearance.

Declarations are not returned for implementation-defined components of the
record_definition. See Reference Manual 13.5.1 (15). These components are not
normally visible to the ASIS application. However, they can be obtained
with the query Implicit_Components.

Appropriate Definition_Kinds:
     A_Record_Definition
     A_Variant

Returns Element_Kinds:
     A_Pragma
     A_Declaration
     A_Definition
     A_Clause

Returns Declaration_Kinds:
     A_Component_Declaration

Returns Definition_Kinds:
     A_Null_Component
     A_Variant_Part

Returns Representation_Clause_Kinds:
     An_Attribute_Definition_Clause


@LabeledClause{function Implicit_Components}


    @key[function] @AdaSubDefn{Implicit_Components}
                    (Definition : @key[in] Asis.Definition)
                        @key[return] Asis.Record_Component_List;

Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the record_definition or variant to query

Returns a list of all implicit implementation-defined components of the
record_definition or variant. The Enclosing_Element of each component is
the Definition argument. Each component is Is_Part_Of_Implicit.

Returns a Nil_Element_List if there are no implicit implementation-defined
components or if the ASIS implementation does not support such
implicit declarations.

Appropriate Definition_Kinds:
     A_Record_Definition
     A_Variant

Returns Element_Kinds:
     A_Declaration

Returns Declaration_Kinds:
     A_Component_Declaration

@b{Implementation Permissions}

Some implementations do not represent all forms of implicit
declarations such that elements representing them can be easily
provided. An implementation can choose whether or not to construct
and provide artificial declarations for implicitly declared elements.

Use the query Implicit_Components_Supported to determine if the
implementation provides implicit record components.
Element Reference -A_Null_Record_Definition - 3.8 - No child elements

Element Reference -A_Variant_Part - 3.8.1

Child Elements returned by
   function Discriminant_Direct_Name
   function Variants

@LabeledClause{function Discriminant_Direct_Name}


    @key[function] @AdaSubDefn{Discriminant_Direct_Name}
                (Variant_Part : @key[in] Asis.Record_Component)
                @key[return] Asis.Name;

Variant_Part    @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the variant_part to query

Returns the Discriminant_Direct_Name of the variant_part.

Appropriate Definition_Kinds:
     A_Variant_Part

Returns Expression_Kinds:
     An_Identifier


@LabeledClause{function Variants}


    @key[function] @AdaSubDefn{Variants} (Variant_Part    : @key[in] Asis.Record_Component;
                       Include_Pragmas : @key[in] Boolean := False)
                       @key[return] Asis.Variant_List;

Variant_Part    @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the variant_part to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of variants that make up the record component, in their
order of appearance.

The only pragmas returned are those following the reserved word "is"
and preceding the reserved word "when" of first variant, and those between
following variants.

Appropriate Definition_Kinds:
     A_Variant_Part

Returns Element_Kinds:
     A_Pragma
     A_Definition

Returns Definition_Kinds:
     A_Variant
Element Reference -A_Variant - 3.8.1

Child Elements returned by
   function Variant_Choices
   function Record_Components
   function Implicit_Components

@LabeledClause{function Variant_Choices}


    @key[function] @AdaSubDefn{Variant_Choices} (Variant : @key[in] Asis.Variant)
                             @key[return] Asis.Element_List;

Variant @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the variant to query

Returns the discrete_choice_list elements, in their order of appearance.
Choices are either an expression, a discrete range, or an others choice.

Appropriate Definition_Kinds:
     A_Variant

Returns Element_Kinds:
     An_Expression
     A_Definition

Returns Definition_Kinds:
     A_Discrete_Range
     An_Others_Choice
Element Reference -A_Private_Type_Definition - 7.3 - No child elements
Element Reference -A_Tagged_Private_Type_Definition - 7.3 - No child elements

Element Reference -A_Private_Extension_Definition - 7.3

Child Elements returned by
   function Ancestor_Subtype_Indication


@LabeledClause{function Ancestor_Subtype_Indication}


    @key[function] @AdaSubDefn{Ancestor_Subtype_Indication}
                   (Definition : @key[in] Asis.Definition)
                         @key[return] Asis.Subtype_Indication;

Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns the ancestor_subtype_indication following the reserved word "new"
in the private_extension_declaration.

Appropriate Definition_Kinds:
     A_Private_Extension_Definition

Returns Definition_Kinds:
     A_Subtype_Indication
Element Reference -A_Task_Definition - 9.1
Element Reference -A_Protected_Definition - 9.4

Child Elements returned by
   functions Visible_Part_Items and Private_Part_Items

@LabeledClause{function Visible_Part_Items}


    @key[function] @AdaSubDefn{Visible_Part_Items}
                (Definition : @key[in] Asis.Definition;
                 Include_Pragmas : @key[in] Boolean := False)
                @key[return] Asis.Declarative_Item_List;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the type_definition to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of declarations, representation clauses, and pragmas
in the visible part of the task or protected definition, in their order
of appearance. The list does not include discriminant_specification elements of
the known_discriminant_part, if any, of the protected type or task type
declaration.

Returns a Nil_Element_List if there are no items.

Appropriate Definition_Kinds:
     A_Task_Definition
     A_Protected_Definition

Returns Element_Kinds:
     A_Pragma
     A_Declaration
     A_Clause

@LabeledClause{function Private_Part_Items}


    @key[function] @AdaSubDefn{Private_Part_Items}
                (Definition : @key[in] Asis.Definition;
                 Include_Pragmas : @key[in] Boolean := False)
                 @key[return] Asis.Declarative_Item_List;

Type_Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the task type definition to query
Include_Pragmas @chg{Version=[1],New=[specifies],Old=[@en Specifies]} whether pragmas are to be returned

Returns a list of declarations, representation clauses, and pragmas in the private
part of the task or protected definition, in their order of appearance.

Returns a Nil_Element_List if there are no items.

Appropriate Definition_Kinds:
     A_Task_Definition
     A_Protected_Definition

Returns Element_Kinds:
     A_Pragma
     A_Declaration
     A_Clause

@LabeledClause{function Is_Private_Present (definition)}


    @key[function] @AdaSubDefn{Is_Private_Present}
                (Definition : @key[in] Asis.Definition) @key[return] Boolean;

Definition @chg{Version=[1],New=[specifies],Old=[@en Specifies]} the definition to query

Returns True if the argument is a task_definition or a protected_definition
that has a reserved word "private" marking the beginning of a (possibly empty)
private part.

Returns False for any definition without a private part.
Returns False for any unexpected Element.

Expected Definition_Kinds:
     A_Task_Definition
     A_Protected_Definition
Element Reference -A_Formal_Type_Definition - 12.5

Element Reference -A_Formal_Private_Type_Definition - 12.5.1 - No child elements
Element Reference -A_Formal_Tagged_Private_Type_Definition  - 12.5.1 - No child elements

Element Reference -A_Formal_Derived_Type_Definition
Child Elements returned by
   function Subtype_Mark
Element Reference -A_Formal_Discrete_Type_Definition        - 12.5.2 - No child elements
Element Reference -A_Formal_Signed_Integer_Type_Definition  - 12.5.2 - No child elements
Element Reference -A_Formal_Modular_Type_Definition         - 12.5.2 - No child elements
Element Reference -A_Formal_Floating_Point_Definition       - 12.5.2 - No child elements
Element Reference -A_Formal_Ordinary_Fixed_Point_Definition - 12.5.2 - No child elements
Element Reference -A_Formal_Decimal_Fixed_Point_Definition  - 12.5.2 - No child elements

Element Reference -A_Formal_Unconstrained_Array_Definition  - 12.5.3

Child Elements returned by
   function Index_Subtype_Definitions
   function Array_Component_Definition

Element Reference -A_Formal_Constrained_Array_Definition    - 12.5.3

Child Elements returned by
   function Discrete_Subtype_Definitions
   function Array_Component_Definition

Element Reference -A_Formal_Access_Type_Definition          - 12.5.4

Child Elements returned by
   function Access_To_Object_Definition
   function Access_To_Subprogram_Parameter_Profile
   function Access_To_Function_Result_Profile

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Definitions;]}
@end{Example}


