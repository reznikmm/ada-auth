@Part(12, Root="ada.mss")

@Comment{$Date: 2000/05/26 05:03:27 $}
@LabeledSection{Generic Units}

@Comment{$Source: e:\\cvsroot/ARM/Source/12.mss,v $}
@Comment{$Revision: 1.13 $}

@begin{Intro}
@Defn{generic unit}
A @i{generic unit} is a program unit that is either a generic subprogram
or a generic package.
@Defn{template}
A generic unit is a @i{template}@Redundant[, which can be parameterized,
and from which corresponding (nongeneric) subprograms or packages can be
obtained].
The resulting program units are said to be @i{instances} of the original
generic unit.
@IndexSee{Term=[template],See=(generic unit)}
@IndexSee{Term=[macro],See=(generic unit)}
@IndexSee{Term=[parameter],See=[generic formal parameter]}
@ToGlossary{Term=<Generic unit>,
  Text=<A generic unit is a template for a (nongeneric) program unit;
  the template can be parameterized by objects, types, subprograms, and
  packages.  An instance of a generic unit is created by a
  @nt(generic_instantiation).
  The rules of the language are enforced when a generic unit is compiled,
  using a generic contract model; additional checks are
  performed upon instantiation to verify the contract is met.
  That is, the declaration of a generic unit represents a contract
  between the body of the generic and instances of the generic.
  Generic units can be used to perform the role that macros
  sometimed play in other languages.>}

@redundant[
A generic unit is declared by a @nt{generic_declaration}.  This form
of declaration has a @nt{generic_formal_part} declaring any generic
formal parameters.  An instance of a generic unit is obtained as the
result of a @nt{generic_instantiation} with appropriate
generic actual parameters for the generic formal parameters.  An
instance of a generic subprogram is a subprogram.  An instance of a
generic package is a package.

Generic units are templates.  As templates they do not have the
properties that are specific to their nongeneric counterparts.  For
example, a generic subprogram can be instantiated but it cannot be
called.  In contrast, an instance of a generic subprogram is a
(nongeneric) subprogram; hence, this instance can be called but it
cannot be used to produce further instances.
]
@end{Intro}

@LabeledClause{Generic Declarations}

@begin{Intro}
@redundant[
A @nt{generic_declaration} declares a generic unit, which is either a
generic subprogram or a generic package.  A @nt{generic_declaration}
includes a @nt{generic_formal_part} declaring any generic formal
parameters.  A generic formal parameter can be an object;
alternatively (unlike a parameter of a subprogram), it can be a type,
a subprogram, or a package.
]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<generic_declaration>,rhs="@Syn2{generic_subprogram_declaration} | @Syn2{generic_package_declaration}"}
@Hinge{}

@Syn{lhs=<generic_subprogram_declaration>,rhs="
     @Syn2{generic_formal_part}  @Syn2{subprogram_specification};"}

@Syn{lhs=<generic_package_declaration>,rhs="
     @Syn2{generic_formal_part}  @Syn2{package_specification};"}

@Syn{lhs=<generic_formal_part>,rhs="@key{generic} {@Syn2{generic_formal_parameter_declaration} | @Syn2{use_clause}}"}

@Syn{lhs=<generic_formal_parameter_declaration>,rhs="
      @Syn2{formal_object_declaration}
    | @Syn2{formal_type_declaration}
    | @Syn2{formal_subprogram_declaration}
    | @Syn2{formal_package_declaration}"}
@begin{SyntaxText}
The only form of @nt{subtype_indication} allowed within a
@nt{generic_formal_part} is a @nt{subtype_mark}
@Redundant[(that is, the @nt{subtype_indication} shall not include an
explicit @nt{constraint})].
The defining name of a generic subprogram shall be an @nt{identifier}
@Redundant[(not an @nt{operator_symbol})].
@begin{Reason}
The reason for forbidding @nt{constraint}s in
@nt{subtype_indication}s is that it simplifies the elaboration of
@nt{generic_declaration}s (since there is nothing to evaluate),
and that it simplifies the matching rules,
and makes them more checkable at compile time.
@end{Reason}
@end{SyntaxText}
@end{Syntax}

@begin{StaticSem}
@Defn{generic package}
@Defn{generic subprogram}
@Defn{generic procedure}
@Defn{generic function}
A @nt{generic_declaration} declares a generic unit @em a
generic package, generic procedure or generic function,
as appropriate.

@Defn{generic formal}
An entity is a @i{generic formal} entity if it is declared
by a @nt<generic_formal_parameter_declaration>.  ``Generic formal,''
or simply ``formal,'' is used as a prefix in referring
to objects, subtypes (and types), functions, procedures and packages,
that are generic formal entities, as well as to their respective
declarations.
@Redundant[Examples: ``generic formal procedure''
or a ``formal integer type declaration.'']
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(generic_declaration)}
The elaboration of a @nt{generic_declaration} has no effect.
@end{RunTime}

@begin{Notes}
Outside a generic unit
a @nt{name} that denotes the @nt{generic_declaration} denotes the
generic unit.
In contrast, within the declarative region of the generic unit,
a @nt{name} that denotes the @nt{generic_declaration} denotes the
current instance.
@begin{TheProof}
This is stated officially as part of the ``current instance''
rule in @RefSec{The Context of Overload Resolution}.
See also @RefSec{Generic Instantiation}.
@end{TheProof}

Within a generic @nt{subprogram_body}, the name of this program unit
acts as the name of a subprogram.  Hence this name can be overloaded,
and it can appear in a recursive call of the current instance.  For the
same reason, this name cannot appear after the reserved word @key{new}
in a (recursive) @nt{generic_instantiation}.

A @nt{default_expression} or @nt{default_name} appearing in a
@nt{generic_formal_part} is not evaluated during elaboration of the
@nt{generic_formal_part}; instead, it is evaluated when used.
(The usual visibility rules apply to any @nt{name} used in a default:
the denoted declaration therefore has to be visible at the place of the
expression.)
@end{Notes}

@begin{Examples}
@i{Examples of generic formal parts:}
@begin{Example}
@key[generic]     --@i{  parameterless }

@key[generic]
   Size : Natural;  --@i{  formal object }

@key[generic]
   Length : Integer := 200;          --@i{ formal object with a default expression}

   Area   : Integer := Length*Length; --@i{ formal object with a default expression}

@key[generic]
   @key[type] Item  @key[is] @key[private];                       --@i{ formal type}
   @key[type] Index @key[is] (<>);                          --@i{ formal type}
   @key[type] Row   @key[is] @key[array](Index @key[range] <>) @key[of] Item; --@i{ formal type}
   @key[with] @key[function] "<"(X, Y : Item) @key[return] Boolean;    --@i{ formal subprogram }
@end{Example}

@i{Examples of generic declarations declaring generic subprograms
Exchange and Squaring:}
@begin{Example}
@key[generic]
   @key[type] Elem @key[is] @key[private];
@key[procedure] Exchange(U, V : @key[in] @key[out] Elem);

@key[generic]
   @key[type] Item @key[is] @key[private];
   @key[with] @key[function] "*"(U, V : Item) @key[return] Item @key[is] <>;
@key[function] Squaring(X : Item) @key[return] Item;
@end{Example}

@i{Example of a generic declaration declaring a generic package:}
@begin{Example}
@key[generic]
   @key[type] Item   @key[is] @key[private];
   @key[type] Vector @key[is] @key[array] (Positive @key[range] <>) @key[of] Item;
   @key[with] @key[function] Sum(X, Y : Item) @key[return] Item;
@key[package] On_Vectors @key[is]
   @key[function] Sum  (A, B : Vector) @key[return] Vector;
   @key[function] Sigma(A    : Vector) @key[return] Item;
   Length_Error : @key[exception];
@key[end] On_Vectors;
@end{Example}
@end{Examples}

@begin{Extend83}
The syntax rule for @nt{generic_formal_parameter_declaration} is modified
to allow the reserved words @key{tagged} and @key{abstract}, to allow
formal derived types, and to allow formal packages.

@nt{Use_clause}s are allowed in @nt{generic_formal_part}s.
This is necessary in order to allow a @nt{use_clause} within a formal
part to provide direct visibility of declarations within a generic
formal package.
@end{Extend83}

@begin{DiffWord83}
The syntax for @nt{generic_formal_parameter_declaration} and
@nt{formal_type_definition} is split up into more named categories.
The rules for these categories are moved to the appropriate clauses and
subclauses.
The names of the categories are changed to be more intuitive and uniform.
For example, we changed @nt{generic_parameter_declaration} to
@nt{generic_formal_parameter_declaration}, because the thing it declares
is a generic formal, not a generic.
In the others, we abbreviate ``generic_formal'' to just ``formal''.
We can't do that for @nt{generic_formal_parameter_declaration},
because of confusion with normal formal parameters of subprograms.
@end{DiffWord83}

@LabeledClause{Generic Bodies}

@begin{Intro}
@Defn{generic body}
The body of a generic unit (a @i{generic body})
@Redundant[is a template for the instance bodies.
The syntax of a generic body is identical to that of a nongeneric body].
@begin{Ramification}
We also use terms like ``generic function body'' and
``nongeneric package body.''
@end{Ramification}
@end{Intro}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(generic body)}
The elaboration of a generic body has no other effect than to
establish that the generic unit can from then on be instantiated without
failing the Elaboration_Check.

If the generic body is a child of a generic package,
then its elaboration establishes that each corresponding
declaration nested in an instance of the parent
(see @RefSecNum{Compilation Units - Library Units})
can from then on be instantiated without failing the Elaboration_Check.

@end{RunTime}

@begin{Notes}
The syntax of generic subprograms implies that a generic subprogram body
is always the completion of a declaration.
@end{Notes}

@begin{Examples}
@i{Example of a generic procedure body:}
@begin{Example}
@key[procedure] Exchange(U, V : @key[in] @key[out] Elem) @key[is]  --@i{ see @RefSecNum{Generic Declarations}}
   T : Elem;  --@i{  the generic formal type}
@key[begin]
   T := U;
   U := V;
   V := T;
@key[end] Exchange;
@end{Example}

@i{Example of a generic function body:}
@begin{Example}
@key[function] Squaring(X : Item) @key[return] Item @key[is]  --@i{  see @RefSecNum{Generic Declarations}}
@key[begin]
   @key[return] X*X;  --@i{  the formal operator "*"}
@key[end] Squaring;
@end{Example}

@i{Example of a generic package body:}
@begin{Example}
@key[package] @key[body] On_Vectors @key[is]  --@i{  see @RefSecNum{Generic Declarations}}

   @key[function] Sum(A, B : Vector) @key[return] Vector @key[is]
      Result : Vector(A'Range); --@i{  the formal type Vector}
      Bias   : @key[constant] Integer := B'First - A'First;
   @key[begin]
      @key[if] A'Length /= B'Length @key[then]
         @key[raise] Length_Error;
      @key[end] @key[if];

      @key[for] N @key[in] A'Range @key[loop]
         Result(N) := Sum(A(N), B(N + Bias)); --@i{ the formal function Sum}
      @key[end] @key[loop];
      @key[return] Result;
   @key[end] Sum;

   @key[function] Sigma(A : Vector) @key[return] Item @key[is]
      Total : Item := A(A'First); --@i{  the formal type Item}
   @key[begin]
      @key[for] N @key[in] A'First + 1 .. A'Last @key[loop]
         Total := Sum(Total, A(N)); --@i{  the formal function Sum}
      @key[end] @key[loop];
      @key[return] Total;
   @key[end] Sigma;
@key[end] On_Vectors;
@end{Example}
@end{Examples}

@LabeledClause{Generic Instantiation}

@begin{Intro}
@redundant[
@Defn2{Term=[instance], Sec=(of a generic unit)}
An instance of a generic unit is declared by a
@nt{generic_instantiation}.
]
@end{Intro}

@begin{MetaRules}
@Defn{generic contract model}
@Defn{contract model of generics}
The legality of an instance should be determinable without looking at
the generic body.
Likewise, the legality of a generic body should be determinable without
looking at any instances.
Thus, the @nt{generic_declaration} forms a contract between the body and
the instances; if each obeys the rules with respect to the
@nt{generic_declaration}, then no legality problems will arise.
This is really a special case of the
``legality determinable via semantic dependences''
@MetaRulesName (see Section 10),
given that a @nt{generic_instantiation} does not depend semantically
upon the generic body, nor vice-versa.

Run-time issues are another story.
For example, whether parameter passing is by copy or by reference is
determined in part by the properties of the generic actuals,
and thus cannot be determined at compile time of the generic body.
Similarly, the contract model does not apply to @LinkTimeTitle.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<generic_instantiation>,rhs="
     @key{package} @Syn2{defining_program_unit_name} @key{is}
         @key{new} @SynI{generic_package_}@Syn2{name} [@Syn2{generic_actual_part}];
   | @key{procedure} @Syn2{defining_program_unit_name} @key{is}
         @key{new} @SynI{generic_procedure_}@Syn2{name} [@Syn2{generic_actual_part}];
   | @key{function} @Syn2{defining_designator} @key{is}
         @key{new} @SynI{generic_function_}@Syn2{name} [@Syn2{generic_actual_part}];"}
@Hinge{}

@Syn{lhs=<generic_actual_part>,rhs="
   (@Syn2{generic_association} {, @Syn2{generic_association}})"}

@Syn{lhs=<generic_association>,rhs="
   [@SynI{generic_formal_parameter_}@Syn2{selector_name} =>] @Syn2{explicit_generic_actual_parameter}"}

@Syn{lhs=<explicit_generic_actual_parameter>,rhs="@Syn2{expression} | @SynI{variable_}@Syn2{name}
   | @SynI{subprogram_}@Syn2{name} | @SynI{entry_}@Syn2{name} | @Syn2{subtype_mark}
   | @SynI{package_instance_}@Syn2{name}"}

@begin{SyntaxText}
@Defn{named association}
@Defn{positional association}
A @nt{generic_association} is @i{named} or @i{positional}
according to whether or not the @i{generic_formal_parameter_}@nt<selector_name>
is specified.  Any positional associations shall precede any
named associations.
@end{SyntaxText}
@end{Syntax}

@begin{Intro}
@Defn{generic actual parameter}
@Defn{generic actual}
@Defn{actual}
The @i{generic actual parameter} is either the
@nt{explicit_generic_actual_parameter} given in a
@nt{generic_parameter_association} for each formal,
or the corresponding @nt{default_expression} or @nt{default_name} if
no @nt{generic_parameter_association} is given for the formal.
When the meaning is clear from context,
the term ``generic actual,'' or simply ``actual,'' is used as a synonym for
``generic actual parameter''
and also for the view denoted by one, or the value of one.
@end{Intro}

@begin{Legality}
In a @nt<generic_instantiation> for a particular kind of program
unit @Redundant[(package, procedure, or function)],
the @nt<name> shall denote a generic
unit of the corresponding kind @Redundant[(generic package,
generic procedure, or generic
function, respectively)].

The @SynI{generic_formal_parameter_}@nt{selector_name} of a
@nt{generic_association} shall denote a
@nt{generic_formal_parameter_declaration}
of the generic unit being instantiated.
If two or more formal subprograms have the same defining name, then
named associations are not allowed for the corresponding
actuals.

A @nt{generic_instantiation} shall contain at
most one @nt<generic_association>
for each formal.
Each formal without an association shall have a
@nt{default_expression} or @nt{subprogram_default}.

In a generic unit @LegalityName@;s
are enforced at compile time of the
@nt{generic_declaration} and generic body,
given the properties of the formals.
In the visible part and formal part of an instance,
@LegalityName@;s are enforced at
compile time of the @nt{generic_instantiation},
given the properties of the actuals.
In other parts of an instance, @LegalityName@;s
are not enforced;
this rule does not apply when a given rule
explicitly specifies otherwise.
@begin{Reason}
Since rules are checked using the properties of the formals,
and since these properties do not always carry over to the actuals,
we need to check the rules again in the visible part of the instance.
For example, only if a tagged type is limited may
an extension of it have limited components in
the @nt<extension_part>.  A formal tagged limited type
is limited, but the actual might be nonlimited.  Hence
any rule that requires a tagged type to be limited
runs into this problem.
Such rules are rare; in most cases, the rules for matching of formals
and actuals guarantee that if the rule is obeyed in the generic unit,
then it has to be obeyed in the instance.
@end{Reason}
@begin{Ramification}
The ``properties'' of the formals are determined without knowing
anything about the actuals:
@begin{Itemize}
A formal derived subtype is constrained if and only if the ancestor
subtype is constrained.
A formal array type is constrained if and only if the declarations
says so.
Other formal subtypes are unconstrained,
even though they might be constrained in an instance.

A formal subtype can be indefinite,
even though the copy might be definite in an instance.

A formal object of mode @key[in] is not a static constant;
in an instance, the copy is static if the actual is.

A formal subtype is not static,
even though the actual might be.

Formal types are specific,
even though the actual can be class-wide.

The subtype of a formal object of mode @key[in out]
is not static.
(This covers the case of AI-00878.)

The subtype of a formal parameter of
a formal subprogram does not
provide an applicable index constraint.

The profile of a formal subprogram is not subtype-conformant
with any other profile.
@Defn{subtype conformance}

A generic formal function is not static.
@end{Itemize}
@end{Ramification}
@begin{Ramification}
The exceptions to the above rule about when legality rules are
enforced fall into these categories:
@begin{Itemize}
Some rules are checked in the generic declaration,
and then again
in both the visible and private parts of the instance:
@begin{InnerItemize}
The parent type of a record extension has to be specific
(see @RefSecNum{Type Extensions}).
This rule is not checked in the instance body.

The parent type of a private extension has to be specific
(see @RefSecNum{Private Types and Private Extensions}).
This rule is not checked in the instance body.

A type with an access discriminant has to be a descendant of
a type declared with @key[limited], or be a task or protected type.
This rule is irrelevant in the instance body.

In the declaration of a record extension,
if the parent type is nonlimited, then each of the
components of the @nt{record_extension_part} have to be nonlimited
(see @RefSecNum{Type Extensions}).
In the generic body, this rule is checked in an assume-the-worst
manner.

A preelaborated library unit has to be preelaborable
(see @RefSecNum{Elaboration Control}).
In the generic body, this rule is checked in an assume-the-worst
manner.
@end{InnerItemize}

@PDefn2{Term=[accessibility rule],Sec=(checking in generic units)}
For the accessibility rules,
the formals have nothing to say about the property in question.
Like the above rules, these rules are checked in the generic declaration,
and then again in both the visible and private parts of
the instance.
In the generic body, we have explicit rules that essentially
assume the worst (in the cases of type extensions and
access-to-subprogram types),
and we have run-time checks
(in the case of access-to-object types).
See @RefSecNum{Type Extensions},
@RefSecNum{Operations of Access Types},
and @RefSecNum{Type Conversions}.

@NoPrefix@;We considered run-time checks for access-to-subprogram types as well.
However, this would present difficulties for
implementations that share generic bodies.

The rules requiring ``reasonable'' values for static expressions are
ignored when the expected type for the expression is a descendant of a
generic formal type other than a generic formal derived type,
and do not apply in an instance.

The rule forbidding two explicit homographs in the same declarative
region does not apply in an instance of a generic unit,
except that it @i{does} apply in the declaration of a record extension
that appears in the visible part of an instance.

Some rules do not apply at all in an instance,
not even in the visible part:
@begin{InnerItemize}
@nt{Body_stub}s are not normally allowed to be multiply nested,
but they can be in instances.
@end{InnerItemize}
@end{Itemize}

@RootDefn{generic contract issue}
Each rule that is an exception is marked with
``generic contract issue;'' look that up in the index to find them all.
@end{Ramification}
@begin{Ramification}
The @LegalityName@;s are the ones labeled @LegalityTitle.
We are talking about all @LegalityName@;s in the entire language here.
Note that, with some exceptions,
the legality of a generic unit is checked even if there are no
instantiations of the generic unit.
@end{Ramification}
@begin{Ramification}
The @LegalityName@;s are described here, and
the overloading rules were described earlier in this clause.
Presumably, every @StaticSemName is sucked in by one of those.
Thus, we have covered all the compile-time rules of the language.
There is no need to say anything special about the @LinkTimeName@;s
or the @RunTimeName@;s.
@end{Ramification}
@begin{Discussion}
Here is an example illustrating how this rule is checked:
``In the declaration of a record extension,
if the parent type is nonlimited, then each of the
components of the @nt{record_extension_part} shall be nonlimited.''
@begin{Example}
@key[generic]
    @key[type] Parent @key[is] @key[tagged] @key[private];
    @key[type] Comp @key[is] @key[limited] @key[private];
@key[package] G1 @key[is]
    @key[type] Extension @key[is] @key[new] Parent @key[with]
        @key[record]
            C : Comp; --@i{ Illegal!}
        @key[end] @key[record];
@key[end] G1;
@end{Example}

The parent type is nonlimited,
and the component type is limited,
which is illegal.
It doesn't matter that an one could imagine writing an instantiation
with the actual for Comp being nonlimited @em we never get to the
instance, because the generic itself is illegal.

On the other hand:
@begin{Example}
@key[generic]
    @key[type] Parent @key[is] @key[tagged] @key[limited] @key[private]; --@i{ Parent is limited.}
    @key[type] Comp @key[is] @key[limited] @key[private];
@key[package] G2 @key[is]
    @key[type] Extension @key[is] @key[new] Parent @key[with]
        @key[record]
            C : Comp; --@i{ OK.}
        @key[end] @key[record];
@key[end] G2;

@key[type] Limited_Tagged @key[is] @key[tagged] @key[limited] @key[null] @key[record];
@key[type] Non_Limited_Tagged @key[is] @key[tagged] @key[null] @key[record];

@key[type] Limited_Untagged @key[is] @key[limited] @key[null] @key[record];
@key[type] Non_Limited_Untagged @key[is] @key[null] @key[record];

@key[package] Good_1 @key[is] @key[new] G2(Parent => Limited_Tagged,
                         Comp => Limited_Untagged);
@key[package] Good_2 @key[is] @key[new] G2(Parent => Non_Limited_Tagged,
                         Comp => Non_Limited_Untagged);
@key[package] Bad  @key[is] @key[new] G2(Parent => Non_Limited_Tagged,
                         Comp => Limited_Untagged); --@i{ Illegal!}
@end{Example}

The first instantiation is legal,
because in the instance the parent is limited,
so the rule is not violated.
Likewise, in the second instantiation,
the rule is not violated in the instance.
However, in the Bad instance,
the parent type is nonlimited,
and the component type is limited,
so this instantiation is illegal.
@end{Discussion}
@end{Legality}

@begin{StaticSem}
A @nt{generic_instantiation} declares an instance;
it is equivalent to the instance declaration
(a @nt{package_declaration} or @nt{subprogram_declaration})
immediately followed by the instance body,
both at the place of the instantiation.
@begin{Ramification}
The declaration and the body of the instance are not ``implicit''
in the technical sense, even though you can't see them in the program
text.
Nor are declarations within an instance ``implicit''
(unless they are implicit by other rules).
This is necessary because implicit declarations have special semantics
that should not be attached to instances.
For a generic subprogram,
the profile of a @nt{generic_instantiation} is that of the instance
declaration, by the stated equivalence.
@end{Ramification}
@begin{Ramification}
@PDefn2{Term=[visible part], Sec=(of an instance)}
@PDefn2{Term=[private part], Sec=(of a package)}
The visible and private parts of a package instance are defined in
@RefSec{Package Specifications and Declarations}
and @RefSec{Formal Packages}.
The visible and private parts of a subprogram instance are defined in
@RefSec{Scope of Declarations}.
@end{Ramification}

The instance is a copy of the text of the template.
@Redundant[Each use of a formal parameter
becomes (in the copy) a use of the actual, as explained below.]
@Defn{package instance}
@Defn{subprogram instance}
@Defn{procedure instance}
@Defn{function instance}
@Defn2{Term=[instance], Sec=(of a generic package)}
@Defn2{Term=[instance], Sec=(of a generic subprogram)}
@Defn2{Term=[instance], Sec=(of a generic procedure)}
@Defn2{Term=[instance], Sec=(of a generic function)}
An instance of a generic package is a package,
that of a generic procedure is a procedure, and that of a generic
function is a function.
@begin{Ramification}
An instance is a package or subprogram
(because we say so),
even though it contains a copy of the @nt{generic_formal_part},
and therefore doesn't look like one.
This is strange, but it's OK, since the syntax rules are overloading
rules, and therefore do not apply in an instance.
@end{Ramification}
@begin{Discussion}
We use a macro-expansion model, with some explicitly-stated
exceptions (see below).
The main exception is that the interpretation of each construct in a
generic unit
(especially including the denotation of each name)
is determined when the declaration and body of the generic unit
(as opposed to the instance) are compiled,
and in each instance this interpretation is (a copy of) the template
interpretation.
In other words, if a construct is interpreted as a @nt{name} denoting
a declaration D, then in an instance, the copy of the construct will
still be a name, and will still denote D (or a copy of D).
From an implementation point of view,
overload resolution is performed on the template,
and not on each copy.

We describe the substitution of generic actual parameters by saying
(in most cases) that the copy of each generic formal parameter
declares a view of the actual.
Suppose a name in a generic unit denotes a
@nt{generic_formal_parameter_declaration}.
The copy of that name in an instance will denote the copy of that
@nt{generic_formal_parameter_declaration} in the instance.
Since the @nt{generic_formal_parameter_declaration} in the instance
declares a view of the actual,
the name will denote a view of the actual.

Other properties of the copy (for example, staticness, classes to
which types belong) are recalculated for each instance;
this is implied by the fact that it's a copy.

Although the @nt{generic_formal_part} is included in an instance,
the declarations in the @nt{generic_formal_part} are only visible
outside the instance in the case of a generic formal package whose
@nt{formal_package_actual_part} is (<>)
@em see @RefSecNum{Formal Packages}.
@end{Discussion}

The interpretation
of each construct within a generic declaration or body is determined
using the overloading rules
when that generic declaration or body is compiled.
In an instance, the interpretation of each (copied) construct is the
same,
except in the case of a name that denotes the
@nt{generic_declaration}
or some declaration within the generic unit;
the corresponding name in the instance then denotes the corresponding copy
of the denoted declaration.
The overloading rules do not apply in the instance.
@begin{Ramification}
See @RefSec{The Context of Overload Resolution} for definitions of
``interpretation'' and ``overloading rule.''

Even the @nt{generic_formal_parameter_declaration}s have corresponding
declarations in the instance,
which declare views of the actuals.

Although the declarations in the instance are copies of those in the
generic unit, they often have quite different properties,
as explained below.
For example a constant declaration in the generic unit might declare
a nonstatic constant, whereas the copy of that declaration might
declare a static constant.
This can happen when the staticness depends on some generic formal.

This rule is partly a ramification of the ``current instance''
rule in @RefSec{The Context of Overload Resolution}.
Note that that rule doesn't cover the @nt{generic_formal_part}.

Although the overloading rules are not observed in the instance,
they are, of course, observed in the @nt{_instantiation} in order to
determine the interpretation of the constituents of the
@nt{_instantiation}.

Since children are considered to occur within their parent's
declarative region, the above rule applies to a name that denotes a
child of a generic unit, or a declaration inside such a child.

Since the @SyntaxName@;s are overloading rules,
it is possible (legal) to violate them in an instance.
For example, it is possible for an instance body to occur in a
@nt{package_specification}, even though the @SyntaxName@;s forbid bodies
in @nt{package_specification}s.
@end{Ramification}

In an instance,
a @nt{generic_formal_parameter_declaration} declares a view
whose properties are identical to those of the actual,
except as specified in
@RefSec{Formal Objects} and @RefSec{Formal Subprograms}.
Similarly, for a declaration within
a @nt{generic_formal_parameter_declaration},
the corresponding declaration in an instance declares a view whose
properties are identical to the corresponding declaration within the
declaration of the actual.
@begin{Ramification}
  In an instance,
  there are no ``properties'' of types and subtypes that come from
  the formal.
  The primitive operations of the type come from the
  formal, but these are declarations in their own right,
  and are therefore handled separately.

  Note that certain properties that come from the actuals are
  irrelevant in the instance.
  For example, if an actual type is of a class deeper in the
  derived-type hierarchy than the formal,
  it is impossible to call the additional operations of the deeper class
  in the instance,
  because any such call would have to be a copy of some corresponding call
  in the generic unit,
  which would have been illegal.
  However, it is sometimes possible to reach into the specification of
  the instance from outside, and notice such properties.
  For example, one could pass an object declared in the instance
  specification to one of the additional operations of the deeper type.

  A @nt{formal_type_declaration} can contain
  @nt{discriminant_specification}s,
  a @nt{formal_subprogram_declaration} can contain
  @nt{formal_parameter_specification}s, and
  a @nt{formal_package_declaration} can contain many kinds of
  declarations.
  These are all inside the generic unit, and have corresponding
  declarations in the instance.

  This rule implies, for example, that if a subtype in a generic unit
  is a subtype of a generic formal subtype,
  then the corresponding subtype in the instance is a subtype of the
  corresponding actual subtype.

  For a @nt{generic_instantiation},
  if a generic actual is a static @Redundant[(scalar or string)] subtype,
  then each use of the corresponding formal parameter within the
  specification of the instance is considered to be static.
  (See AI-00409.)

  Similarly, if a generic actual is a static expression and
  the corresponding formal parameter has a
  static @Redundant[(scalar or string)] subtype,
  then each use of the formal parameter in the specification of the
  instance is considered to be static.
  (See AI-00505.)

  If a primitive subprogram of a type derived from a generic formal
  derived tagged type is not overriding (that is, it is a new
  subprogram), it is possible for the copy of that subprogram in
  an instance to override a subprogram inherited from the actual.
  For example:
@begin{Example}
@key[type] T1 @key[is] @key[tagged] @key[record] ... @key[end] @key[record];

@key[generic]
    @key[type] Formal @key[is] @key[new] T1;
@key[package] G @key[is]
    @key[type] Derived_From_Formal @key[is] @key[new] Formal @key[with] @key[record] ... @key[end] @key[record];
    @key[procedure] Foo(X : @key[in] Derived_From_Formal); --@i{ Does not override anything.}
@key[end] G;

@key[type] T2 @key[is] @key[new] T1 @key[with] @key[record] ... @key[end] @key[record];
@key[procedure] Foo(X : @key[in] T2);

@key[package] Inst @key[is] @key[new] G(Formal => T2);
@end{Example}

In the instance Inst,
the declaration of Foo for Derived_From_Formal
overrides the Foo inherited from T2.
@end{Ramification}
@begin{ImplNote}
For formal types,
an implementation that shares the code among multiple instances of the
same generic unit needs to beware that things like parameter passing
mechanisms (by-copy vs. by-reference) and @nt{representation_clause}s are
determined by the actual.
@end{ImplNote}

@redundant[
Implicit declarations are also copied,
and a name that denotes an implicit declaration in the generic
denotes the corresponding copy in the instance.
However, for a type declared within the visible part of the
generic, a whole new set of primitive subprograms
is implicitly declared for use outside the instance,
and may differ from the copied set if the properties of the
type in some way depend on the properties of some actual type
specified in the instantiation.
For example, if the type in the generic is derived
from a formal private type, then in the instance the type
will inherit subprograms from the corresponding actual type.

@Defn{override}
These new implicit declarations occur immediately after the type
declaration in the instance, and override
the copied ones.  The copied ones
can be called only from within the instance; the new
ones can be called only from outside the instance, although
for tagged types, the
body of a new one can be executed by a call to an old one.
]
@begin{TheProof}
This rule is stated officially in @RefSec{Visibility}.
@end{TheProof}
@begin{Ramification}
The new ones follow from the class(es) of the formal types.
For example, for a type T derived from a generic formal private type,
if the actual is Integer, then the copy of T in the instance has a
"+" primitive operator,
which can be called from outside the instance
(assuming T is declared in the visible part of the instance).

AI-00398.

Since an actual type is always in the class determined for the formal,
the new subprograms hide all of the copied ones,
except for a declaration of "/=" that corresponds to an explicit
declaration of "=".
Such "/=" operators are special, because unlike other implicit
declarations of primitive subprograms, they do not appear by virtue
of the class, but because of an explicit declaration of "=".
If the declaration of "=" is implicit (and therefore overridden
in the instance), then a corresponding implicitly declared "/="
is also overridden.
But if the declaration of "=" is explicit (and therefore not overridden
in the instance), then a corresponding implicitly declared "/="
is not overridden either,
even though it's implicit.

Note that the copied ones can be called from inside the instance, even
though they are hidden from all visibility,
because the names are resolved in the generic unit @em visibility
is irrelevant for calls in the instance.
@end{Ramification}

@Redundant[In the visible part of an instance, an explicit declaration
overrides an implicit declaration if they are homographs,
as described in @RefSecNum{Visibility}.]
On the other hand, an explicit declaration in
the private part of an instance overrides an implicit
declaration in the instance, only if the corresponding explicit
declaration in the generic overrides a corresponding
implicit declaration in the generic.
Corresponding rules apply to the other kinds of overriding
described in @RefSecNum{Visibility}.
@begin{Ramification}
For example:
@begin{Example}
@key[type] Ancestor @key[is] @key[tagged] @key[null] @key[record];

@key[generic]
    @key[type] Formal @key[is] @key[new] Ancestor @key[with] @key[private];
@key[package] G @key[is]
    @key[type] T @key[is] @key[new] Formal @key[with] @key[null] @key[record];
    @key[procedure] P(X : @key[in] T); --@i{ (1)}
@key[private]
    @key[procedure] Q(X : @key[in] T); --@i{ (2)}
@key[end] G;

@key[type] Actual @key[is] @key[new] Ancestor @key[with] @key[null] @key[record];
@key[procedure] P(X : @key[in] Actual);
@key[procedure] Q(X : @key[in] Actual);

@key[package] Instance @key[is] @key[new] G(Formal => Actual);
@end{Example}

In the instance, the copy of P at (1) overrides Actual's P,
whereas the copy of Q at (2) does not override anything;
in implementation terms, it occupies a separate slot in
the type descriptor.
@end{Ramification}
@begin{Reason}
The reason for this rule is so a programmer writing an
@nt{_instantiation} need not look at the private part of the generic in
order to determine which subprograms will be overridden.
@end{Reason}
@end{StaticSem}

@begin{LinkTime}
Recursive generic instantiation is not allowed in the following
sense: if a given generic unit includes an instantiation of a second
generic unit, then the instance generated by this instantiation shall
not include an instance of the first generic unit
@Redundant[(whether this instance is generated directly, or indirectly
by intermediate instantiations)].
@begin{Discussion}
Note that this rule is not a violation of the generic contract model,
because it is not a @LegalityName.
Some implementations may be able to check this rule at compile time,
but that requires access to all the bodies,
so we allow implementations to check the rule at link time.
@end{Discussion}
@end{LinkTime}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(generic_instantiation)}
For the elaboration of a @nt{generic_instantiation},
each @nt{generic_association} is first evaluated.
If a default is used,
an implicit @nt{generic_association} is assumed for this rule.
These evaluations are done in an arbitrary order, except that the
evaluation for a default actual takes place after the evaluation
for another actual if the default includes a @nt{name}
that denotes the other one.
Finally, the instance declaration and body are elaborated.
@begin{Ramification}
Note that if the evaluation of a default depends on some side-effect
of some other evaluation,
the order is still arbitrary.
@end{Ramification}

@PDefn2{Term=[evaluation], Sec=(generic_association)}
For the evaluation of a @nt{generic_association}
the generic actual parameter is evaluated.
Additional actions are performed in the case of a formal object of
mode @key{in} (see @RefSecNum{Formal Objects}).
@begin{Honest}
Actually, the actual is evaluated only if evaluation is defined for that
kind of construct @em we don't actually ``evaluate'' @nt{subtype_mark}s.
@end{Honest}
@end{RunTime}

@begin{Notes}
If a formal type is not tagged, then the type is treated as
an untagged type within the generic body.
Deriving from such a type in a generic body is permitted;
the new type does not get a new tag value,
even if the actual is tagged.
Overriding operations for such a derived type cannot be dispatched to
from outside the instance.
@begin{Ramification}
If two overloaded subprograms declared in a generic package
specification differ only by the (formal) type of their parameters and
results, then there exist legal instantiations for which all calls of
these subprograms from outside the instance are ambiguous.  For example:
@begin{Example}
@key[generic]
   @key[type] A @key[is] (<>);
   @key[type] B @key[is] @key[private];
@key[package] G @key[is]
   @key[function] Next(X : A) @key[return] A;
   @key[function] Next(X : B) @key[return] B;
@key[end] G;

@key[package] P @key[is] @key[new] G(A => Boolean, B => Boolean);
--@i{ All calls of P.Next are ambiguous.}
@end{Example}
@end{Ramification}
@begin{Ramification}
The following example illustrates some of the subtleties of the
substitution of formals and actuals:
@begin{Example}
@key[generic]
    @key[type] T1 @key[is] @key[private];
    --@i{ A predefined "=" operator is implicitly declared here:}
    --@i{ function "="(Left, Right : T1) return Boolean;}
    --@i{ Call this "="@-{1}.}
@key[package] G @key[is]
    @key[subtype] S1 @key[is] T1; --@i{ So we can get our hands on the type from}
                      --@i{ outside an instance.}
    @key[type] T2 @key[is] @key[new] T1;
    --@i{ An inherited "=" operator is implicitly declared here:}
    --@i{ function "="(Left, Right : T2) return Boolean;}
    --@i{ Call this "="@-{2}.}

    T1_Obj : T1 := ...;
    Bool_1 : Boolean := T1_Obj = T1_Obj;

    T2_Obj : T2 := ...;
    Bool_2 : Boolean := T2_Obj = T2_Obj;
@key[end] G;
...

@key[package] P @key[is]
    @key[type] My_Int @key[is] @key[new] Integer;
    --@i{ A predefined "=" operator is implicitly declared here:}
    --@i{ function "="(Left, Right : My_Int) return Boolean;}
    --@i{ Call this "="@-{3}.}
    @key[function] "="(X, Y : My_Int) @key[return] Boolean;
    --@i{ Call this "="@-{4}.}
    --@i{ "="@-{3} is hidden from all visibility by "="@-{4}.}
    --@i{ Nonetheless, "="@-{3} can ``reemerge'' in certain circumstances.}
@key[end] P;
@key[use] P;
...
@key[package] I @key[is] @key[new] G(T1 => My_Int); --@i{ "="@-{5} is declared in I (see below).}
@key[use] I;

Another_T1_Obj : S1 := 13; --@i{ Can't denote T1, but S1 will do.}
Bool_3 : Boolean := Another_T1_Obj = Another_T1_Obj;

Another_T2_Obj : T2 := 45;
Bool_4 : Boolean := Another_T2_Obj = Another_T2_Obj;

Double : T2 := T2_Obj + Another_T2_Obj;
@end{Example}

In the instance I, there is a copy of "="@-{1} (call it "="@-{1i}) and
"="@-{2} (call it "="@-{2i}).
The "="@-{1i} and "="@-{2i} declare views of the predefined "=" of My_Int
(that is, "="@-{3}).
In the initialization of Bool_1 and Bool_2 in the generic unit G,
the names "=" denote "="@-{1} and "="@-{2}, respectively.
Therefore, the copies of these names in the instances
denote "="@-{1i} and "="@-{2i}, respectively.
Thus, the initialization of I.Bool_1 and I.Bool_2 call the predefined
equality operator of My_Int;
they will not call "="@-{4}.

The declarations "="@-{1i} and "="@-{2i} are hidden from all
visibility.
This prevents them from being called from outside the instance.

The declaration of Bool_3
calls "="@-{4}.

The instance I also contains implicit declarations of the primitive
operators of T2, such as "=" (call it "="@-{5}) and "+".
These operations cannot be called from within the instance,
but the declaration of Bool_4 calls "="@-{5}.
@end{Ramification}
@end{Notes}

@begin{Examples}
@i{Examples of generic instantiations (see @RefSecNum{Generic Declarations}):}
@begin{Example}
@tabclear()@tabset(P49)
@key[procedure] Swap @key[is] @key[new] Exchange(Elem => Integer);
@key[procedure] Swap @key[is] @key[new] Exchange(Character); @\--@i{  Swap is overloaded }
@key[function] Square @key[is] @key[new] Squaring(Integer); @\--@i{  "*" of Integer used by default}
@key[function] Square @key[is] @key[new] Squaring(Item => Matrix, "*" => Matrix_Product);
@key[function] Square @key[is] @key[new] Squaring(Matrix, Matrix_Product); --@i{ same as previous    }

@key[package] Int_Vectors @key[is] @key[new] On_Vectors(Integer, Table, "+");
@end{Example}

@i{Examples of uses of instantiated units:}
@begin{Example}
Swap(A, B);
A := Square(A);

T : Table(1 .. 5) := (10, 20, 30, 40, 50);
N : Integer := Int_Vectors.Sigma(T);  --@i{  150 (see @RefSec{Generic Bodies} for the body of Sigma)}

@key[use] Int_Vectors;
M : Integer := Sigma(T);  --@i{  150}
@end{Example}
@end{Examples}

@begin{Inconsistent83}
In Ada 83, all explicit actuals are evaluated before all defaults,
and the defaults are evaluated in the order of the formal
declarations.
This ordering requirement is relaxed in Ada 9X.
@end{Inconsistent83}

@begin{Incompatible83}
We have attempted to remove every violation of the contract model.
Any remaining contract model violations should be considered bugs in
the RM9X.
The unfortunate property of reverting to the predefined operators of
the actual types is retained for upward compatibility.
(Note that fixing this would require subtype conformance rules.)
However, tagged types do not revert in this sense.
@end{Incompatible83}

@begin{Extend83}
The syntax rule for @nt{explicit_generic_actual_parameter} is modified to allow a
@SynI{package_instance_}@nt{name}.
@end{Extend83}

@begin{DiffWord83}
The fact that named associations cannot be used for two formal
subprograms with the same defining name is moved to AARM-only material,
because it is a ramification of other rules, and because it is not of
interest to the average user.

The rule that
``An explicit @nt{explicit_generic_actual_parameter} shall not be supplied more
than once for a given @nt{generic_formal_parameter}''
seems to be missing from RM83, although it was clearly the intent.

In the explanation that the instance is a copy of the template,
we have left out RM83-12.3(5)'s ``apart from the generic formal
part'', because it seems that things in the formal part still need to
exist in instances.
This is particularly true for generic formal packages,
where you're sometimes allowed to reach in and denote the formals of
the formal package from outside it.
This simplifies the explanation of what each name in an instance
denotes: there are just two cases: the declaration can be inside or
outside (where inside needs to include the generic unit itself).
Note that the RM83 approach of listing many cases (see RM83-12.5(5-14))
would have become even more unwieldy with the addition of generic formal
packages, and the declarations that occur therein.

We have corrected the definition of the elaboration of a
@nt{generic_instantiation} (RM83-12.3(17)); we don't elaborate
entities, and the instance is not ``implicit.''

In RM83, there is a rule saying the formal and actual shall match, and
then there is much text defining what it means to match.
Here, we simply state all the latter text as rules.
For example, ``A formal foo is matched by an actual greenish bar''
becomes ``For a formal foo, the actual shall be a greenish bar.''
This is necessary to split the @ResolutionName@;s
from the @LegalityName@;s.
Besides, there's really no need to define the concept of matching for
generic parameters.
@end{DiffWord83}

@LabeledClause{Formal Objects}

@begin{Intro}
@redundant[
@Defn{generic formal object}
@Defn{formal object, generic}
A generic formal object can be used to pass a value or variable
to a generic unit.
]
@end{Intro}

@begin{MetaRules}
A generic formal object of mode @key{in} is like a constant
initialized to the value of the @nt{explicit_generic_actual_parameter}.

A generic formal object of mode @key{in out} is like a renaming
of the @nt{explicit_generic_actual_parameter}.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<formal_object_declaration>,rhs="
    @Syn2{defining_identifier_list} : @Syn2{mode} @Syn2{subtype_mark} [:= @Syn2{default_expression}];"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(generic formal object default_expression)}
The expected type for the @nt{default_expression}, if any, of a formal
object is the type of the formal object.


@PDefn2{Term=[expected type], Sec=(generic formal in object actual)}
For a generic formal object of mode @key[in],
the expected type for the actual is the type of the formal.

For a generic formal object of mode @key[in out],
the type of the actual shall resolve to the type of the formal.
@begin{Reason}
See the corresponding rule for @nt{object_renaming_declaration}s for a
discussion of the reason for this rule.
@end{Reason}

@end{Resolution}

@begin{Legality}
If a generic formal object has a @nt{default_expression},
then the mode shall be @key{in}
@Redundant[(either explicitly or by default)];
otherwise, its mode shall be either @key{in} or @key{in out}.
@begin{Ramification}
Mode @key{out} is not allowed for generic formal objects.
@end{Ramification}

For a generic formal object of mode @key{in}, the actual shall be an
@nt{expression}.
For a generic formal object of mode @key{in out}, the actual shall be
a @nt{name} that denotes a variable for which renaming is allowed
(see @RefSecNum{Object Renaming Declarations}).
@begin{Honest}
The part of this that requires an @nt{expression} or @nt{name} is a
@ResolutionName,
but that's too pedantic to worry about.
(The part about denoting a variable, and renaming being allowed,
is most certainly @i{not} a @ResolutionName.)
@end{Honest}

The type of a generic formal object of mode @key{in} shall be
nonlimited.
@begin{Reason}
Since a generic formal object is like a constant of mode
@key{in} initialized to the value of the actual,
a limited type would not make sense, since initializing a constant is
not allowed for a limited type.
That is, generic formal objects of mode @key{in} are passed by copy,
and limited types are not supposed to be copied.
@end{Reason}
@end{Legality}

@begin{StaticSem}
A @nt{formal_object_declaration} declares a generic formal object.
The default mode is @key{in}.
@PDefn2{Term=[nominal subtype], Sec=(of a generic formal object)}
For a formal object of mode @key{in},
the nominal subtype is the one denoted by the
@nt{subtype_mark} in the declaration of the formal.
@PDefn2{Term=static, Sec=(subtype)}
For a formal object of mode @key{in out}, its type
is determined by the @nt<subtype_mark> in the declaration;
its nominal subtype is nonstatic, even if the
@nt<subtype_mark> denotes a static subtype.

@Defn2{Term=[stand-alone constant],
  Sec=(corresponding to a formal object of mode @key[in])}
In an instance,
a @nt{formal_object_declaration} of mode @key{in}
declares a new stand-alone constant
object whose initialization expression is the
actual,
whereas a @nt{formal_object_declaration} of mode @key{in out}
declares a view whose properties are identical to those of the actual.
@begin{Ramification}
These rules imply that generic formal objects of mode @key{in} are
passed by copy,
whereas generic formal objects of mode @key{in out} are passed by
reference.

Initialization and finalization happen for the constant declared by
a @nt{formal_object_declaration} of mode @key{in} as for any constant;
see @RefSec{Object Declarations}
and @RefSec{User-Defined Assignment and Finalization}.

@PDefn2{Term=[subtype], Sec=(of a generic formal object)}
In an instance,
the subtype of a generic formal object of mode
@key{in} is as for the equivalent constant.
In an instance,
the subtype of a generic formal object of mode @key{in out}
is the subtype of the corresponding generic actual.
@end{Ramification}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[evaluation], Sec=(generic_association for a formal
object of mode @key{in})}
@Defn2{Term=[assignment operation], Sec=(during evaluation of a
@nt{generic_association} for a formal object of mode @key{in})}
For the evaluation of a @nt{generic_association}
for a formal object of mode @key{in},
a constant object is created, the value of the actual parameter
is converted to the nominal subtype of the formal object,
and assigned to the object@Redundant[, including any value adjustment @em
see @RefSecNum{User-Defined Assignment and Finalization}].
@PDefn2{Term=[implicit subtype conversion],Sec=(generic formal object of mode @key[in])}
@begin{Ramification}
This includes evaluating the actual
and doing a subtype conversion,
which might raise an exception.
@end{Ramification}
@begin{Discussion}
The rule for evaluating a @nt<generic_association> for a formal
object of mode @key{in out} is covered by
the general Dynamic Semantics rule in @RefSecNum{Generic Instantiation}.
@end{Discussion}
@end{RunTime}

@begin{Notes}
The constraints that apply to a generic formal object of mode @key{in
out} are those of the corresponding generic actual parameter (not
those implied by the @nt{subtype_mark} that appears in the
@nt{formal_object_declaration}).
Therefore, to avoid confusion, it is recommended that the name of a
first subtype
be used for the declaration of such a formal object.
@begin{Ramification}
Constraint checks are done at instantiation time for formal objects of
mode @key{in},
but not for formal objects of mode @key{in out}.
@end{Ramification}
@end{Notes}

@begin{Extend83}
In Ada 83, it is forbidden to pass a (nongeneric) formal parameter
of mode @key{out}, or a subcomponent thereof, to a generic formal
object of mode @key{in out}.
This restriction is removed in Ada 9X.
@end{Extend83}

@begin{DiffWord83}
We make ``@nt{mode}'' explicit in the syntax.
RM83 refers to the mode without saying what it is.
This is also more uniform with the way (nongeneric) formal parameters
are defined.

We considered allowing mode @key{out} in Ada 9X,
for uniformity with (nongeneric) formal parameters.
The semantics would be identical for modes @key{in out} and
@key{out}.
(Note that generic formal objects of mode @key{in out} are passed by
reference.  Note that for (nongeneric) formal parameters that are
allowed to be passed by reference, the semantics of @key{in out} and
@key{out} is the same.  The difference might serve as documentation.
The same would be true for generic formal objects, if @key{out} were
allowed, so it would be consistent.)
We decided not to make this change, because it does not produce any
important benefit, and any change has some cost.
@end{DiffWord83}

@LabeledClause{Formal Types}

@begin{Intro}
@redundant[
A generic formal subtype can be used to pass to a generic unit
a subtype whose type is in a certain class of types.
]
@begin{Reason}
We considered having intermediate syntactic categories
@nt{formal_integer_type_definition},
@nt{formal_real_type_definition}, and
@nt{formal_fixed_point_definition},
to be more uniform with the syntax rules for non-generic-formal
types.
However, that would make the rules for formal types slightly more
complicated, and it would cause confusion,
since @nt{formal_discrete_type_definition} would not fit into the
scheme very well.
@end{Reason}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_type_declaration>,rhs="
    @key{type} @Syn2{defining_identifier}[@Syn2{discriminant_part}] @key{is} @Syn2{formal_type_definition};"}
@Hinge{}

@Syn{lhs=<formal_type_definition>,rhs="
      @Syn2{formal_private_type_definition}
    | @Syn2{formal_derived_type_definition}
    | @Syn2{formal_discrete_type_definition}
    | @Syn2{formal_signed_integer_type_definition}
    | @Syn2{formal_modular_type_definition}
    | @Syn2{formal_floating_point_definition}
    | @Syn2{formal_ordinary_fixed_point_definition}
    | @Syn2{formal_decimal_fixed_point_definition}
    | @Syn2{formal_array_type_definition}
    | @Syn2{formal_access_type_definition}"}
@end{Syntax}

@begin{Legality}
@Defn{generic actual subtype}
@Defn{actual subtype}
@Defn{generic actual type}
@Defn{actual type}
For a generic formal subtype, the actual shall be
a @nt{subtype_mark};
it denotes the @i{(generic) actual subtype}.
@begin{Ramification}
When we say simply ``formal'' or ``actual'' (for a generic
formal that denotes a subtype) we're talking about the subtype, not
the type, since a name that denotes a @nt{formal_type_declaration}
denotes a subtype, and the corresponding actual also denotes a
subtype.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@Defn{generic formal type}
@Defn{formal type}
@Defn{generic formal subtype}
@Defn{formal subtype}
A @nt{formal_type_declaration} declares a @i{(generic) formal type},
and its first subtype, the @i{(generic) formal subtype}.
@begin{Ramification}
A subtype (other than the first subtype)
of a generic formal type is not a generic formal subtype.
@end{Ramification}

@Defn{determined class for a formal type}
@Defn{class determined for a formal type}
The form of a @nt{formal_type_definition} @i{determines a
class} to which the formal type belongs.
For a @nt{formal_private_type_definition} the reserved words
@key{tagged} and @key{limited} indicate the class
(see @RefSecNum{Formal Private and Derived Types}).
For a @nt{formal_derived_type_definition} the class is
the derivation class rooted at the ancestor type.
For other formal types,
the name of the syntactic category indicates the
class;
a @nt{formal_discrete_type_definition} defines a discrete type,
and so on.
@begin{Reason}
This rule is clearer with the flat syntax rule for
@nt{formal_type_definition} given above.
Adding @nt{formal_integer_type_definition} and others would make this
rule harder to state clearly.
@end{Reason}
@end{StaticSem}

@begin{Legality}
The actual type shall be in the class determined for the formal.
@begin{Ramification}
For example, if the class determined for the formal is the class of
all discrete types, then the actual has to be discrete.

Note that this rule does not require the actual to belong to every
class to which the formal belongs.
For example, formal private types are in the class of composite types,
but the actual need not be composite.
Furthermore, one can imagine an infinite number of classes that are just
arbitrary sets of types that obey the closed-under-derivation rule,
and are therefore technically classes
(even though we don't give them names,
since they are uninteresting).
We don't want this rule to apply to @i{those} classes.

``Limited'' is not a ``interesting'' class, but ``nonlimited'' is;
it is legal to pass a nonlimited type to a limited formal type,
but not the other way around.
The reserved word @nt{limited} really represents a class containing
both limited and nonlimited types.
``Private'' is not a class; a generic formal private type accepts
both private and nonprivate actual types.

It is legal to pass a class-wide subtype as the actual
if it is in the right class,
so long as the formal has unknown discriminants.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@Redundant[The formal type also belongs to each class that contains
the determined class.]
The primitive subprograms of the type are as for any
type in the determined class.  For a formal type other than a formal
derived type, these are the predefined operators of the type;
they are implicitly declared immediately after the declaration
of the formal type.  In an instance, the copy of such an
implicit declaration declares a view of the predefined operator
of the actual type, even if this operator has been overridden for
the actual type.
@Redundant[The rules specific to formal derived types are given
in @RefSecNum{Formal Private and Derived Types}.]
@begin{Ramification}
All properties of the type are as for any type in the class.
Some examples:
The primitive operations available are as defined by the language for each
class.
The form of @nt{constraint} applicable to a formal type in a
@nt{subtype_indication} depends on the class of the type as for a
nonformal type.
The formal type is tagged if and only if it is declared as a tagged
private type, or as a type derived from a (visibly) tagged type.
(Note that the actual type might be tagged even if the formal type is
not.)
@end{Ramification}

@end{StaticSem}

@begin{Notes}
Generic formal types, like all types, are not named.
Instead, a @nt{name} can denote a generic formal subtype.
Within a generic unit, a generic formal type is considered as being
distinct from all other (formal or nonformal) types.
@begin{TheProof}
This follows from the fact that each @nt{formal_type_declaration}
declares a type.
@end{TheProof}

A @nt{discriminant_part} is allowed only for certain kinds of types,
and therefore only for certain kinds of generic formal types.
See @RefSecNum{Discriminants}.
@begin{Ramification}
The term ``formal floating point type'' refers to a type defined by a
@nt{formal_floating_point_definition}.
It does not include
a formal derived type whose ancestor is floating point.
Similar terminology applies to the other kinds of
@nt{formal_type_definition}.
@end{Ramification}
@end{Notes}

@begin{Examples}
@i{Examples of generic formal types:}
@begin{Example}
@key[type] Item @key[is] @key[private];
@key[type] Buffer(Length : Natural) @key[is] @key[limited] @key[private];

@key[type] Enum  @key[is] (<>);
@key[type] Int   @key[is] @key[range] <>;
@key[type] Angle @key[is] @key[delta] <>;
@key[type] Mass  @key[is] @key[digits] <>;

@key[type] Table @key[is] @key[array] (Enum) @key[of] Item;
@end{Example}

@i{Example of a generic formal part declaring a formal integer type:}
@begin{Example}
@key[generic]
   @key[type] Rank @key[is] @key[range] <>;
   First  : Rank := Rank'First;
   Second : Rank := First + 1;  --@i{  the operator "+" of the type Rank  }
@end{Example}
@end{Examples}

@begin{DiffWord83}
RM83 has separate sections ``Generic Formal Xs'' and ``Matching Rules for
Formal Xs'' (for various X's) with most of the text
redundant between the two.
We have combined the two in order to reduce the redundancy.
In RM83, there is no ``Matching Rules for Formal Types'' section; nor is
there a ``Generic Formal Y Types'' section (for Y = Private, Scalar, Array,
and Access).
This causes, for example, the duplication across all the ``Matching
Rules for Y Types'' sections of the rule that the actual passed to a
formal type shall be a subtype;
the new organization avoids that problem.

The matching rules are stated more concisely.

We no longer consider the multiplying
operators that deliver a result of type @i{universal_fixed} to be
predefined for the various types; there is only one of each in
package Standard.  Therefore, we need not mention them here as RM83
had to.
@end{DiffWord83}

@LabeledSubClause{Formal Private and Derived Types}

@begin{Intro}
@Redundant[The class determined for a formal private type
can be either
limited or nonlimited, and either tagged or untagged;
no more specific class is known for such a type.
The class determined for a formal derived type is the derivation class
rooted at the ancestor type.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_private_type_definition>,
  rhs="[[@key{abstract}] @key{tagged}] [@key{limited}] @key{private}"}
@Hinge{}
@Syn{lhs=<formal_derived_type_definition>,
  rhs="[@key{abstract}] @key{new} @Syn2{subtype_mark} [@key{with} @key{private}]"}
@end{Syntax}

@begin{Legality}
If a generic formal type declaration has a @nt{known_discriminant_part},
then it shall not include a
@nt{default_expression} for a discriminant.
@begin{Ramification}
Consequently,
a generic formal subtype
with a @nt{known_discriminant_part} is an indefinite subtype, so
the declaration of a stand-alone variable has to provide a constraint
on such a subtype, either explicitly, or by its initial value.
@end{Ramification}

@Defn2{Term=[ancestor subtype], Sec=(of a formal derived type)}
The @i(ancestor subtype) of a formal derived type is the
subtype denoted by the @nt<subtype_mark> of
the @nt<formal_derived_type_definition>.
For a formal derived type declaration,
the reserved words @key{with private} shall appear if and only
if the ancestor type is a tagged type; in this case
the formal derived type is a private extension of the
ancestor type and the ancestor shall not be a class-wide
type.
@Redundant[Similarly,
the optional reserved word @key{abstract} shall
appear only if the ancestor type is a tagged type].
@begin{Reason}
We use the term ``ancestor'' here instead of ``parent''
because the actual can be any descendant of the ancestor,
not necessarily a direct descendant.
@end{Reason}

If the formal subtype is definite, then the actual subtype shall
also be definite.
@begin{Ramification}
On the other hand, for an indefinite formal subtype,
the actual can be either definite or indefinite.
@end{Ramification}

For a generic formal derived type with no
@nt<discriminant_part>:
@begin(Itemize)
  If the ancestor subtype is constrained,
  the actual subtype shall be constrained,
  and shall be statically compatible with the ancestor;
@begin{Ramification}
  In other words, any constraint on the ancestor subtype is considered
  part of the ``contract.''
@end{Ramification}

  If the ancestor subtype is an unconstrained access
  or composite subtype,
  the actual subtype shall be unconstrained.
@begin{Reason}
  This rule ensures that if a composite constraint is allowed on the
  formal, one is also allowed on the actual.
  If the ancestor subtype is an unconstrained scalar subtype,
  the actual is allowed to be constrained, since a scalar constraint
  does not cause further constraints to be illegal.
@end{Reason}

  If the ancestor subtype is an unconstrained discriminated
  subtype, then the actual shall have the same number of
  discriminants,

  and each discriminant of the actual shall correspond to
  a discriminant of the ancestor,
  in the sense of @RefSecNum{Discriminants}.

@begin{Reason}
  This ensures that if a discriminant constraint is given on
  the formal subtype, the corresponding constraint in the instance
  will make sense, without additional run-time checks.
  This is not necessary for arrays, since the bounds cannot be
  overridden in a type extension.
  An @nt<unknown_discriminant_part> may be used
  to relax these matching requirements.
@end{Reason}
@end(Itemize)

The declaration of a formal derived type shall not have a
@nt{known_discriminant_part}.
For a generic formal private type with a
@nt{known_discriminant_part}:
@begin{Itemize}
The actual type shall be a type with the same number of discriminants.

The actual subtype shall be unconstrained.

The subtype of each discriminant of the
actual type shall statically match the subtype of the corresponding
discriminant of the formal type.
@PDefn2{Term=[statically matching],Sec=(required)}
@begin{Reason}
We considered defining the first and third rule to be called
``subtype conformance'' for @nt{discriminant_parts}.
We rejected that idea, because it would require implicit (inherited)
@nt{discriminant_parts}, which seemed like too much mechanism.
@end{Reason}
@end{Itemize}


@Redundant[For a generic formal type with an
@nt{unknown_discriminant_part},
the actual may, but need not, have discriminants,
and may be definite or indefinite.]

@end{Legality}

@begin{StaticSem}
The class determined for a formal private type is as follows:
@TabClear{}@Tabset(P32)
@begin{Display}
@i(Type Definition) @\@i(Determined Class)

@key{limited private} @\the class of all types
@key{private} @\the class of all nonlimited types
@key{tagged limited private} @\the class of all tagged types
@key{tagged private} @\the class of all nonlimited tagged types
@end{Display}

@Redundant[The presence of the reserved word @key{abstract} determines
whether the actual type may be abstract.]

A formal private or derived type is a private or derived type,
respectively.
A formal derived tagged type is a private extension.
@Redundant[A formal private or derived type is abstract if the reserved
word @key(abstract) appears in its declaration.]

If the ancestor type is a composite type that is not an
array type, the formal type inherits components from the ancestor
type (including
discriminants if a new @nt<discriminant_part> is not specified),
as for a derived type defined by a @nt<derived_type_definition>
(see @RefSecNum(Derived Types and Classes)).

For a formal derived type, the predefined
operators and inherited user-defined subprograms are determined
by the ancestor type, and are implicitly declared
at the earliest place, if any, within the immediate scope of the
formal type, where the corresponding primitive subprogram
of the ancestor is visible (see @RefSecNum{Private Operations}).
In an instance, the copy of such an implicit declaration declares a view
of the corresponding primitive subprogram of the ancestor,
even if this primitive has been overridden for the actual type.
@Redundant[In the case of a
formal private extension, however,
the tag of the formal type is that of the actual type,
so if the tag in a call is statically determined to be that of the
formal type,
the body executed will be that corresponding to the actual type.]
@begin{Ramification}
The above rule defining the properties of primitive subprograms in an
instance applies even if the subprogram has been overridden or
hidden for the actual type.
This rule is necessary for untagged types,
because their primitive subprograms might have been overridden by
operations that are not subtype-conformant with the operations
defined for the class.
For tagged types, the rule still applies, but the primitive
subprograms will dispatch to the appropriate implementation based on
the type and tag of the operands.
Even for tagged types, the formal parameter names and
@nt{default_expression}s are determined
by those of the primitive subprograms of the specified
ancestor type.
@end{Ramification}

For @PrefixType{a prefix S that denotes a formal indefinite subtype},
the following attribute is defined:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Definite>,
  Text=[S'Definite yields True if the actual subtype corresponding
    to S is definite; otherwise it yields False.  The value of this
    attribute is of the predefined type Boolean.]}
@begin{Discussion}
Whether an actual subtype is definite or indefinite may
have a major effect on the algorithm used in a generic.
For example, in a generic I/O package, whether to use fixed-length or
variable-length records could depend on whether the actual is
definite or indefinite.
This attribute is essentially a replacement for the Constrained attribute
which is now considered obsolete.
@end{Discussion}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{Notes}
In accordance with the general rule that the actual type shall
belong to the class determined for the formal
(see @RefSec(Formal Types)):
@begin(itemize)
  If the formal type is nonlimited, then so shall be the actual;

  For a formal derived type, the actual shall be in the class rooted
  at the ancestor subtype.
@end(itemize)

@Redundant[The actual type can be abstract only if the formal type is abstract
(see @RefSecNum{Abstract Types and Subprograms}).]
@begin{Reason}
This is necessary to avoid contract model problems,
since one or more of its primitive subprograms are abstract;
it is forbidden to create objects of the type,
or to declare functions returning the type.
@end{Reason}
@begin{Ramification}
On the other hand, it is OK to pass a non-abstract actual to an abstract
formal @em @key[abstract] on the formal indicates that the actual might
be abstract.
@end{Ramification}

If the formal has a @nt{discriminant_part},
the actual can be either definite or indefinite.
Otherwise, the actual has to be definite.
@end{Notes}

@begin{Incompatible83}
Ada 83 does not have
@nt{unknown_discriminant_part}s, so it allows indefinite
subtypes to be passed to definite formals,
and applies a legality rule to the instance body.
This is a contract model violation.
Ada 9X disallows such cases at the point of the instantiation.
The workaround is to add (<>)
as the @nt{discriminant_part} of
any formal subtype if it is intended
to be used with indefinite actuals.
If that's the intent, then there can't be anything
in the generic body that would require a definite subtype.

The check for discriminant subtype matching is changed from a
run-time check to a compile-time check.
@end{Incompatible83}

@LabeledSubClause{Formal Scalar Types}

@begin{Intro}
A @i{formal scalar type} is one defined by any of the
@nt{formal_type_definition}s in this subclause.
@Redundant[The class determined for a formal scalar type
is discrete, signed integer, modular, floating point,
ordinary fixed point, or decimal.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_discrete_type_definition>,rhs="(<>)"}
@Hinge{}

@Syn{lhs=<formal_signed_integer_type_definition>,rhs="@key{range} <>"}
@Syn{lhs=<formal_modular_type_definition>,rhs="@key{mod} <>"}
@Hinge{}

@Syn{lhs=<formal_floating_point_definition>,rhs="@key{digits} <>"}
@Hinge{}

@Syn{lhs=<formal_ordinary_fixed_point_definition>,rhs="@key{delta} <>"}
@Syn{lhs=<formal_decimal_fixed_point_definition>,rhs="@key{delta} <> @key{digits} <>"}
@end{Syntax}

@begin{Legality}
The actual type for a formal scalar type
shall not be a nonstandard numeric type.
@begin{Reason}
This restriction is necessary because nonstandard numeric
types have some number of restrictions on their use, which could cause
contract model problems in a generic body.  Note that nonstandard
numeric types can be passed to formal derived and formal private
subtypes, assuming they obey all the other rules, and assuming the
implementation allows it (being nonstandard means the implementation
might disallow anything).
@end{Reason}
@end{Legality}

@begin{Notes}
The actual type shall be in the class of types implied
by the syntactic category of the formal type definition
(see @RefSec(Formal Types)).  For example, the actual for a
@nt<formal_modular_type_definition> shall be a modular type.
@end{Notes}

@LabeledSubClause{Formal Array Types}

@begin{Intro}
@Redundant[The class determined for a formal array type
is the class of all array types.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_array_type_definition>,rhs="@Syn2{array_type_definition}"}
@end{Syntax}

@begin{Legality}
The only form of @nt{discrete_subtype_definition} that is allowed within the
declaration of a
generic formal (constrained) array subtype is a @nt{subtype_mark}.
@begin{Reason}
The reason is the same as for forbidding @nt{constraint}s in
@nt{subtype_indication}s (see @RefSecNum{Generic Declarations}).
@end{Reason}

For a formal array subtype, the actual subtype shall satisfy the
following conditions:
@begin{Itemize}
The formal array type and the actual array type shall have the same
dimensionality; the formal subtype and the actual subtype shall be
either both constrained or both unconstrained.

For each index position, the index types shall be the same,
and the index subtypes (if unconstrained),
or the index ranges (if constrained), shall statically match
(see @RefSecNum{Statically Matching Constraints and Subtypes}).
@PDefn2{Term=[statically matching],Sec=(required)}

The component subtypes of the formal and
actual array types shall statically match.
@PDefn2{Term=[statically matching],Sec=(required)}

If the formal type has aliased components,
then so shall the actual.
@begin{Ramification}
On the other hand, if the formal's components are not aliased,
then the actual's components can be either aliased or not.
@end{Ramification}
@end{Itemize}
@end{Legality}

@begin{Examples}
@i{Example of formal array types:}
@begin{Example}
--@i{  given the generic package }

@key[generic]
   @key[type] Item   @key[is] @key[private];
   @key[type] Index  @key[is] (<>);
   @key[type] Vector @key[is] @key[array] (Index @key[range] <>) @key[of] Item;
   @key[type] Table  @key[is] @key[array] (Index) @key[of] Item;
@key[package] P @key[is]
   ...
@key[end] P;

--@i{  and the types }

@key[type] Mix    @key[is] @key[array] (Color @key[range] <>) @key[of] Boolean;
@key[type] Option @key[is] @key[array] (Color) @key[of] Boolean;

--@i{  then Mix can match Vector and Option can match Table }

@key[package] R @key[is] @key[new] P(Item   => Boolean, Index => Color,
                   Vector => Mix,     Table => Option);

--@i{  Note that Mix cannot match Table and Option cannot match Vector}
@end{Example}
@end{Examples}

@begin{Incompatible83}
The check for matching of component subtypes and index subtypes or
index ranges is changed from a
run-time check to a compile-time check.
The Ada 83 rule that ``If the component type is not a scalar type,
then the component subtypes shall be either both constrained or both
unconstrained'' is removed, since it is subsumed by static matching.
Likewise, the rules requiring that component types be
the same is subsumed.
@end{Incompatible83}

@LabeledSubClause{Formal Access Types}

@begin{Intro}
@Redundant[The class determined for a formal access type
is the class of all access types.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_access_type_definition>,rhs="@Syn2{access_type_definition}"}
@end{Syntax}

@begin{Legality}
For a formal access-to-object type,
the designated subtypes of the formal and actual types shall
statically match.
@PDefn2{Term=[statically matching],Sec=(required)}

If and only if the @nt{general_access_modifier} @key{constant} applies
to the formal,
the actual shall be an access-to-constant type.
If the @nt{general_access_modifier} @key{all} applies to the
formal, then the actual shall be a general access-to-variable type
(see @RefSecNum{Access Types}).
@begin{Ramification}
If no @nt{_modifier} applies to the formal, then
the actual type may be either a pool-specific or a general
access-to-variable type.
@end{Ramification}

For a formal access-to-subprogram subtype,
the designated profiles of the formal and the actual
shall be mode-conformant,
and the calling convention of the actual shall be @i{protected}
if and only if that of the formal is @i{protected}.
@Defn2{Term=[mode conformance],Sec=(required)}
@begin{Reason}
We considered requiring subtype conformance here,
but mode conformance is more flexible,
given that there is no way in general to specify the convention of
the formal.
@end{Reason}
@end{Legality}

@begin{Examples}
@i{Example of formal access types:}
@begin{Example}
--@i{  the formal types of the generic package }

@key[generic]
   @key[type] Node @key[is] @key[private];
   @key[type] Link @key[is] @key[access] Node;
@key[package] P @key[is]
   ...
@key[end] P;

--@i{  can be matched by the actual types }

@key[type] Car;
@key[type] Car_Name @key[is] @key[access] Car;

@key[type] Car @key[is]
   @key[record]
      Pred, Succ : Car_Name;
      Number     : License_Number;
      Owner      : Person;
   @key[end] @key[record];

--@i{  in the following generic instantiation }

@key[package] R @key[is] @key[new] P(Node => Car, Link => Car_Name);
@end{Example}
@end{Examples}

@begin{Incompatible83}
The check for matching of designated subtypes is changed from a
run-time check to a compile-time check.
The Ada 83 rule that ``If the
designated type is other than a scalar type, then the designated
subtypes shall be either both constrained or both unconstrained'' is
removed, since it is subsumed by static matching.
@end{Incompatible83}

@begin{Extend83}
Formal access-to-subprogram subtypes and formal general access
types are new concepts.
@end{Extend83}

@LabeledClause{Formal Subprograms}

@begin{Intro}
@redundant[
@Defn{generic formal subprogram}
@Defn{formal subprogram, generic}
Formal subprograms can be used to pass callable entities to a generic
unit.
]
@end{Intro}

@begin{MetaRules}
Generic formal subprograms are like renames of the @nt{explicit_generic_actual_parameter}.
@end{MetaRules}

@begin{Syntax}
@Syn{lhs=<formal_subprogram_declaration>,rhs="@key{with} @Syn2{subprogram_specification} [@key{is} @Syn2{subprogram_default}];"}

@Syn{lhs=<subprogram_default>,rhs="@Syn2{default_name} | <>"}
@Syn{lhs=<default_name>,rhs="@Syn2{name}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected profile], Sec=(formal subprogram default_name)}
The expected profile for the @nt<default_name>, if any, is that of the
formal subprogram.
@begin{Ramification}
This rule,
  unlike others in this clause, is observed at compile
  time of the @nt{generic_declaration}.

The evaluation
  of the @nt{default_name} takes place during the
  elaboration of each instantiation that uses the default, as defined
  in @RefSec{Generic Instantiation}.
@end{Ramification}

@PDefn2{Term=[expected profile], Sec=(formal subprogram actual)}
For a generic formal subprogram,
the expected profile for the actual is that of the formal subprogram.
@end{Resolution}

@begin{Legality}
The profiles of the formal and any named default shall be
mode-conformant.
@Defn2{Term=[mode conformance],Sec=(required)}
@begin{Ramification}
This rule, unlike others in this clause, is checked at compile
time of the @nt{generic_declaration}.
@end{Ramification}

The profiles of the formal and actual shall be mode-conformant.
@Defn2{Term=[mode conformance],Sec=(required)}
@end{Legality}

@begin{StaticSem}
A @nt{formal_subprogram_declaration} declares a generic formal subprogram.
The types of the formal parameters and result, if any, of
the formal subprogram are
those determined by the @nt<subtype_mark>s given in
the @nt{formal_subprogram_declaration}; however, independent of
the particular subtypes that are denoted by the @nt<subtype_mark>s,
the nominal subtypes of the formal parameters and result, if any,
are defined to be nonstatic, and unconstrained if
of an array type @Redundant[(no applicable index constraint
is provided in a call on a formal subprogram)].
In an instance,
a @nt{formal_subprogram_declaration} declares a view
of the actual.
The profile of this view takes its subtypes
and calling convention
from the original profile of the actual entity,
while taking the formal parameter
@nt{name}s and @nt{default_expression}s from the profile given in the
@nt{formal_subprogram_declaration}.
The view is a function or procedure,
never an entry.
@begin{Discussion}
This rule is intended to be the same as the one for
renamings-as-declarations, where the @nt{formal_subprogram_declaration}
is analogous to a renaming-as-declaration,
and the actual is analogous to the renamed view.
@end{Discussion}

If a generic unit has a @nt<subprogram_default> specified by a box, and
the corresponding actual parameter is omitted, then it is equivalent to
an explicit actual parameter that is a usage name identical to the
defining name of the formal.
@end{StaticSem}

@begin{Notes}
The matching rules for formal subprograms state requirements that are
similar to those applying to @nt{subprogram_renaming_declaration}s
(see @RefSecNum{Subprogram Renaming Declarations}).
In particular, the name of a parameter of the formal subprogram need not
be the same as that of the corresponding parameter of the actual
subprogram;
similarly, for these parameters, @nt{default_expression}s need not
correspond.

The constraints that apply to a parameter of a formal subprogram are
those of the corresponding formal parameter of the matching actual
subprogram (not those implied by the corresponding @nt{subtype_mark} in
the @nt{_specification} of the formal subprogram).  A similar remark
applies to the result of a function.  Therefore, to avoid confusion, it
is recommended that the @nt{name} of a first subtype
be used in any declaration of a formal subprogram.

The subtype specified for a formal parameter of a generic formal
subprogram can be any visible subtype, including a generic formal
subtype of the same @nt{generic_formal_part}.

A formal subprogram is matched by an attribute of a type if the
attribute is a function with a matching specification.
An enumeration literal of a given type matches a parameterless formal
function whose result type is the given type.

A @nt{default_name} denotes an entity that is visible or directly
visible at the place of the @nt{generic_declaration};
a box used as a default is equivalent to a name that denotes an
entity that is directly visible at the place of the
@nt{_instantiation}.
@begin{TheProof}
Visibility and name resolution are applied to the equivalent explicit
actual parameter.
@end{TheProof}

The actual subprogram cannot be abstract
(see @RefSecNum{Abstract Types and Subprograms}).
@end{Notes}

@begin{Examples}
@i{Examples of generic formal subprograms:}
@begin{Example}
@key[with] @key[function] "+"(X, Y : Item) @key[return] Item @key[is] <>;
@key[with] @key[function] Image(X : Enum) @key[return] String @key[is] Enum'Image;
@key[with] @key[procedure] Update @key[is] Default_Update;

--@i{  given the generic procedure declaration }

@key[generic]
   @key[with] @key[procedure] Action (X : @key[in] Item);
@key[procedure] Iterate(Seq : @key[in] Item_Sequence);

--@i{  and the procedure }

@key[procedure] Put_Item(X : @key[in] Item);

--@i{  the following instantiation is possible }

@key[procedure] Put_List @key[is] @key[new] Iterate(Action => Put_Item);
@end{Example}
@end{Examples}

@LabeledClause{Formal Packages}

@begin{Intro}
@redundant[
@Defn{generic formal package}
@Defn{formal package, generic}
Formal packages can be used to pass packages to a generic unit.
The @nt{formal_package_declaration} declares that the formal package
is an instance of a given generic package.
Upon instantiation, the actual package has to be an instance
of that generic package.
]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_package_declaration>,rhs="
    @key{with} @key{package} @Syn2{defining_identifier} @key{is} @key{new} @SynI{generic_package_}@Syn2{name}  @Syn2{formal_package_actual_part};"}

@Syn{lhs=<formal_package_actual_part>,rhs="
    (<>) | [@Syn2{generic_actual_part}]"}
@end{Syntax}

@begin{Legality}
@Defn2{Term=template, Sec=(for a formal package)}
The @i(generic_package_)@nt<name> shall denote a generic package
(the @i(template) for the formal package);
the formal package is an instance of the template.

The actual shall be an instance of the template.
If the @nt<formal_package_actual_part> is (<>),
@Redundant[then the actual may be any instance of the template];
otherwise,
each actual parameter of the actual instance shall match the corresponding
actual parameter of the formal package @Redundant[(whether the
actual parameter is given explicitly or by default)], as follows:
@begin{Itemize}
For a formal object of mode @key[in] the actuals match if they are
static expressions with the same value, or if they statically denote
the same constant,
or if they are both the literal @key[null].
@begin{Reason}
  We can't simply require full conformance between the two
  actual parameter expressions, because the two
  expressions are being evaluated at different times.
@end{Reason}

For a formal subtype, the actuals match if they denote
statically matching subtypes.
@PDefn2{Term=[statically matching],Sec=(required)}

For other kinds of formals, the actuals match if they statically
denote the same entity.
@end{Itemize}
@end{Legality}

@begin{StaticSem}
A @nt{formal_package_declaration} declares a generic formal package.

@PDefn2{Term=[visible part], Sec=(of a formal package)}
The visible part of a formal package includes
the first list of @nt{basic_declarative_item}s of the
@nt{package_specification}.
In addition, if the @nt{formal_package_actual_part} is (<>),
it also includes the @nt{generic_formal_part} of the template
for the formal package.
@begin{Ramification}
If the @nt<formal_package_actual_part> is (<>),
then the declarations that occur immediately within the
@nt<generic_formal_part> of the template for the formal package
are visible outside the formal package,
and can be denoted by expanded names outside the formal package.
@end{Ramification}
@begin{Reason}
We always want either the actuals
or the formals of an instance to be namable from outside, but never both.
If both were namable, one would get some funny anomalies since
they denote the same entity, but, in the case of types at least,
they might have different and inconsistent sets of primitive operators
due to predefined operator ``reemergence.''  Formal derived types
exacerbate the difference.  We want the implicit declarations
of the @nt<generic_formal_part> as well as the explicit
declarations, so we get operations on the formal types.
@end{Reason}
@begin{Ramification}
A generic formal package is a package, and is an instance.
Hence, it is possible to pass a generic formal package
as an actual to another generic formal package.
@end{Ramification}
@end{StaticSem}

@begin{Extend83}
Formal packages are new to Ada 9X.
@end{Extend83}

@LabeledClause{Example of a Generic Package}

@begin{Intro}
The following example provides a possible formulation of stacks by means
of a generic package.
The size of each stack and the type of the stack elements are provided
as generic formal parameters.
@end{Intro}

@begin{Examples}
@begin{Example}
@key[generic]
   Size : Positive;
   @key[type] Item @key[is] @key[private];
@key[package] Stack @key[is]
   @key[procedure] Push(E : @key[in]  Item);
   @key[procedure] Pop (E : @key[out] Item);
   Overflow, Underflow : @key[exception];
@key[end] Stack;

@key[package] @key[body] Stack @key[is]

   @key[type] Table @key[is] @key[array] (Positive @key[range] <>) @key[of] Item;
   Space : Table(1 .. Size);
   Index : Natural := 0;

   @key[procedure] Push(E : @key[in] Item) @key[is]
   @key[begin]
      @key[if] Index >= Size @key[then]
         @key[raise] Overflow;
      @key[end] @key[if];
      Index := Index + 1;
      Space(Index) := E;
   @key[end] Push;

   @key[procedure] Pop(E : @key[out] Item) @key[is]
   @key[begin]
      @key[if] Index = 0 @key[then]
         @key[raise] Underflow;
      @key[end] @key[if];
      E := Space(Index);
      Index := Index - 1;
   @key[end] Pop;

@key[end] Stack;
@end{Example}

Instances of this generic package can be obtained as follows:
@begin{Example}
@key[package] Stack_Int  @key[is] @key[new] Stack(Size => 200, Item => Integer);
@key[package] Stack_Bool @key[is] @key[new] Stack(100, Boolean);
@end{Example}

Thereafter, the procedures of the instantiated packages can be called as
follows:
@begin{Example}
Stack_Int.Push(N);
Stack_Bool.Push(True);
@end{Example}

Alternatively, a generic formulation of the type Stack can be given as
follows (package body omitted):
@begin{Example}
@key[generic]
   @key[type] Item @key[is] @key[private];
@key[package] On_Stacks @key[is]
   @key[type] Stack(Size : Positive) @key[is] @key[limited] @key[private];
   @key[procedure] Push(S : @key[in] @key[out] Stack; E : @key[in]  Item);
   @key[procedure] Pop (S : @key[in] @key[out] Stack; E : @key[out] Item);
   Overflow, Underflow : @key[exception];
@key[private]
   @key[type] Table @key[is] @key[array] (Positive @key[range] <>) @key[of] Item;
   @key[type] Stack(Size : Positive) @key[is]
      @key[record]
         Space : Table(1 .. Size);
         Index : Natural := 0;
      @key[end] @key[record];
@key[end] On_Stacks;
@end{Example}

In order to use such a package, an instance has to be created and
thereafter stacks of the corresponding type can be declared:
@begin{Example}
@key[declare]
   @key[package] Stack_Real @key[is] @key[new] On_Stacks(Real); @key[use] Stack_Real;
   S : Stack(100);
@key[begin]
   ...
   Push(S, 2.54);
   ...
@key[end];
@end{Example}
@end{Examples}
