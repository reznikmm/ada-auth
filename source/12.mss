@Part(12, Root="ada.mss")

@Comment{$Date: 2006/10/18 00:25:25 $}
@LabeledSection{Generic Units}

@Comment{$Source: e:\\cvsroot/ARM/Source/12.mss,v $}
@Comment{$Revision: 1.65 $}

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
  packages. An instance of a generic unit is created by a
  @nt(generic_instantiation).
  The rules of the language are enforced when a generic unit is compiled,
  using a generic contract model; additional checks are
  performed upon instantiation to verify the contract is met.
  That is, the declaration of a generic unit represents a contract
  between the body of the generic and instances of the generic.
  Generic units can be used to perform the role that macros
  sometimes play in other languages.>}@ChgNote{Correction for AI-00024, no mechism to correct glossary entries.}

@redundant[A generic unit is declared by a @nt{generic_declaration}. This form
of declaration has a @nt{generic_@!formal_@!part} declaring any generic
formal parameters. An instance of a generic unit is obtained as the
result of a @nt{generic_instantiation} with appropriate
generic actual parameters for the generic formal parameters. An
instance of a generic subprogram is a subprogram. An instance of a
generic package is a package.

Generic units are templates. As templates they do not have the
properties that are specific to their nongeneric counterparts. For
example, a generic subprogram can be instantiated but it cannot be
called. In contrast, an instance of a generic subprogram is a
(nongeneric) subprogram; hence, this instance can be called but it
cannot be used to produce further instances.]
@end{Intro}

@LabeledClause{Generic Declarations}

@begin{Intro}
@redundant[A @nt{generic_declaration} declares a generic unit, which is either a
generic subprogram or a generic package. A @nt{generic_declaration}
includes a @nt{generic_formal_part} declaring any generic formal
parameters. A generic formal parameter can be an object;
alternatively (unlike a parameter of a subprogram), it can be a type,
a subprogram, or a package.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<generic_declaration>,rhs="@Syn2{generic_subprogram_declaration} | @Syn2{generic_package_declaration}"}


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
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@Defn{generic package}
@Defn{generic subprogram}
@Defn{generic procedure}
@Defn{generic function}
A @nt{generic_declaration} declares a generic unit @em a
generic package, generic procedure@Chg{Version=[2],New=[,],Old=[]}
or generic function, as appropriate.

@Defn{generic formal}
An entity is a @i{generic formal} entity if it is declared
by a @nt<generic_formal_parameter_declaration>. @lquotes@;Generic formal,@rquotes@;
or simply @lquotes@;formal,@rquotes@; is used as a prefix in referring
to objects, subtypes (and types), functions, procedures and packages,
that are generic formal entities, as well as to their respective
declarations.
@Redundant[Examples: @lquotes@;generic formal procedure@rquotes@;
or a @lquotes@;formal integer type declaration.@rquotes@;]
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
This is stated officially as part of the @lquotes@;current instance@rquotes@;
rule in @RefSec{The Context of Overload Resolution}.
See also @RefSec{Generic Instantiation}.
@end{TheProof}

Within a generic @nt{subprogram_body}, the name of this program unit
acts as the name of a subprogram. Hence this name can be overloaded,
and it can appear in a recursive call of the current instance. For the
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
@leading@keepnext@i{Examples of generic formal parts:}
@begin{Example}
@key[generic]     --@RI{  parameterless }

@key[generic]
   Size : Natural;  --@RI{  formal object }

@key[generic]
   Length : Integer := 200;          --@RI{ formal object with a default expression}

   Area   : Integer := Length*Length; --@RI{ formal object with a default expression}

@key[generic]
   @key[type] Item  @key[is] @key[private];                       --@RI{ formal type}
   @key[type] Index @key[is] (<>);                          --@RI{ formal type}
   @key[type] Row   @key[is] @key[array](Index @key[range] <>) @key[of] Item; --@RI{ formal type}
   @key[with] @key[function] "<"(X, Y : Item) @key[return] Boolean;    --@RI{ formal subprogram }
@end{Example}

@begin{Wide}
@leading@keepnext@i{Examples of generic declarations declaring generic subprograms
Exchange and Squaring:}
@end{Wide}
@begin{Example}
@key[generic]
   @key[type] Elem @key[is] @key[private];
@key[procedure] Exchange(U, V : @key[in] @key[out] Elem);

@key[generic]
   @key[type] Item @key[is] @key[private];
   @key[with] @key[function] "*"(U, V : Item) @key[return] Item @key[is] <>;
@key[function] Squaring(X : Item) @key[return] Item;
@end{Example}

@begin{Wide}
@leading@keepnext@i{Example of a generic declaration declaring a generic package:}
@end{Wide}
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
@Defn{extensions to Ada 83}
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
For example, we changed @ntf{generic_parameter_declaration} to
@nt{generic_formal_parameter_declaration}, because the thing it declares
is a generic formal, not a generic.
In the others, we abbreviate @lquotes@;generic_formal@rquotes@; to just @lquotes@;formal@rquotes@;.
We can't do that for @nt{generic_formal_parameter_declaration},
because of confusion with normal formal parameters of subprograms.
@end{DiffWord83}



@RmNewPage@Comment{Insert page break so printed RM's look better.}
@LabeledClause{Generic Bodies}

@begin{Intro}
@Defn{generic body}
The body of a generic unit (a @i{generic body})
@Redundant[is a template for the instance bodies.
The syntax of a generic body is identical to that of a nongeneric body].
@begin{Ramification}
We also use terms like @lquotes@;generic function body@rquotes@; and
@lquotes@;nongeneric package body.@rquotes@;
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
@leading@keepnext@i{Example of a generic procedure body:}
@begin{Example}
@key[procedure] Exchange(U, V : @key[in] @key[out] Elem) @key[is]  --@RI{ see @RefSecNum{Generic Declarations}}
   T : Elem;  --@RI{  the generic formal type}
@key[begin]
   T := U;
   U := V;
   V := T;
@key[end] Exchange;
@end{Example}

@begin{Wide}
@leading@keepnext@i{Example of a generic function body:}
@end{Wide}
@begin{Example}
@key[function] Squaring(X : Item) @key[return] Item @key[is]  --@RI{  see @RefSecNum{Generic Declarations}}
@key[begin]
   @key[return] X*X;  --@RI{  the formal operator "*"}
@key[end] Squaring;
@end{Example}

@begin{Wide}
@leading@keepnext@i{Example of a generic package body:}
@end{Wide}
@begin{Example}
@key[package] @key[body] On_Vectors @key[is]  --@RI{  see @RefSecNum{Generic Declarations}}

   @key[function] Sum(A, B : Vector) @key[return] Vector @key[is]
      Result : Vector(A'Range); --@RI{  the formal type Vector}
      Bias   : @key[constant] Integer := B'First - A'First;
   @key[begin]
      @key[if] A'Length /= B'Length @key[then]
         @key[raise] Length_Error;
      @key[end] @key[if];

      @key[for] N @key[in] A'Range @key[loop]
         Result(N) := Sum(A(N), B(N + Bias)); --@RI{ the formal function Sum}
      @key[end] @key[loop];
      @key[return] Result;
   @key[end] Sum;

   @key[function] Sigma(A : Vector) @key[return] Item @key[is]
      Total : Item := A(A'First); --@RI{  the formal type Item}
   @key[begin]
      @key[for] N @key[in] A'First + 1 .. A'Last @key[loop]
         Total := Sum(Total, A(N)); --@RI{  the formal function Sum}
      @key[end] @key[loop];
      @key[return] Total;
   @key[end] Sigma;
@key[end] On_Vectors;
@end{Example}
@end{Examples}


@LabeledClause{Generic Instantiation}

@begin{Intro}
@redundant[@Defn2{Term=[instance], Sec=(of a generic unit)}
An instance of a generic unit is declared by a
@nt{generic_instantiation}.]
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
@lquotes@;legality determinable via semantic dependences@rquotes@;
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
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00218-03]}
@Syn{lhs=<generic_instantiation>,rhs="
     @key{package} @Syn2{defining_program_unit_name} @key{is}
         @key{new} @SynI{generic_package_}@Syn2{name} [@Syn2{generic_actual_part}];
   | @Chg{Version=[2],New=<[@Syn2{overriding_indicator}]
     >,Old=<>}@key{procedure} @Syn2{defining_program_unit_name} @key{is}
         @key{new} @SynI{generic_procedure_}@Syn2{name} [@Syn2{generic_actual_part}];
   | @Chg{Version=[2],New=<[@Syn2{overriding_indicator}]
     >,Old=<>}@key{function} @Syn2{defining_designator} @key{is}
         @key{new} @SynI{generic_function_}@Syn2{name} [@Syn2{generic_actual_part}];"}


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
according to whether or not the @i{generic_@!formal_@!parameter_}@!@nt<selector_@!name>
is specified. Any positional associations shall precede any
named associations.
@end{SyntaxText}
@end{Syntax}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised]}
@Defn{generic actual parameter}
@Defn{generic actual}
@Defn{actual}
The @i{generic actual parameter} is either the
@nt{explicit_generic_actual_parameter} given in a
@Chg{Version=[2],New=[@nt{generic_@!association}],Old=[@ntf{generic_@!parameter_@!association}]}
for each formal,
or the corresponding @nt{default_@!expression} or @nt{default_@!name} if no
@Chg{Version=[2],New=[@nt{generic_@!association}],Old=[@ntf{generic_@!parameter_@!association}]}
is given for the formal.
When the meaning is clear from context,
the term @lquotes@;generic actual,@rquotes@; or simply @lquotes@;actual,@rquotes@; is used as a synonym for
@lquotes@;generic actual parameter@rquotes@;
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
most one @nt<generic_association> for each formal.
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
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Since rules are checked using the properties of the formals,
and since these properties do not always carry over to the actuals,
we need to check the rules again in the visible part of the instance.
For example, only if a tagged type is limited may
an extension of it have limited components in
the @Chg{Version=[2],New=[@nt{record_extension_part}],Old=[@ntf<extension_part>]}.
A formal tagged limited type
is limited, but the actual might be nonlimited. Hence
any rule that requires a tagged type to be limited
runs into this problem.
Such rules are rare; in most cases, the rules for matching of formals
and actuals guarantee that if the rule is obeyed in the generic unit,
then it has to be obeyed in the instance.
@end{Reason}
@begin{Ramification}
@leading@;The @lquotes@;properties@rquotes@; of the formals are determined
without knowing anything about the actuals:
@begin{Itemize}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0095],ARef=[AI95-00034-01]}
A formal derived subtype is constrained if and only if the ancestor
subtype is constrained.
A formal array type is constrained if and only if the declarations
@Chg{New=[say],Old=[says]}
so.@Chg{New=[ A formal private type is constrained if it does not have a
discriminant part.],Old=[]}
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
(This covers the case of AI83-00878.)

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
@leading@;The exceptions to the above rule about when legality rules are
enforced fall into these categories:
@begin{Itemize}
@leading@;Some rules are checked in the generic declaration, and then again
in both the visible and private parts of the instance:
@begin{InnerItemize}
The parent type of a record extension has to be specific
(see @RefSecNum{Type Extensions}).
This rule is not checked in the instance body.

The parent type of a private extension has to be specific
(see @RefSecNum{Private Types and Private Extensions}).
This rule is not checked in the instance body.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00402-01]}
A type with @Chg{Version=[2],New=[a @nt{default_expression} of ],Old=[]}an
access discriminant has to be a
descendant of @Chg{Version=[2],New=[an explicitly limited record type],Old=[a
type declared with @key[limited]]}, or be a task or protected
type. This rule is irrelevant in the instance body.]}

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

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00402-01]}
@ChgAdded{Version=[2],NoPrefix=[T],Text=[The corrections made by the
Corrigendum added a number of such rules, and the Amendment added many more.
There doesn't seem to be much value in repeating all of these rules here (as
of this writing, there are roughly 17 such rules).
As noted below, all such rules are indexed in the AARM.]}

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

The rules requiring @lquotes@;reasonable@rquotes@; values for static expressions are
ignored when the expected type for the expression is a descendant of a
generic formal type other than a generic formal derived type,
and do not apply in an instance.

The rule forbidding two explicit homographs in the same declarative
region does not apply in an instance of a generic unit,
except that it @i{does} apply in the declaration of a record extension
that appears in the visible part of an instance.

@leading@;Some rules do not apply at all in an instance,
not even in the visible part:
@begin{InnerItemize}
@nt{Body_stub}s are not normally allowed to be multiply nested,
but they can be in instances.
@end{InnerItemize}
@end{Itemize}

@RootDefn{generic contract issue}
Each rule that is an exception is marked with
@lquotes@;generic contract issue;@rquotes@; look that up in the index to find them all.
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
@lquotes@;In the declaration of a record extension,
if the parent type is nonlimited, then each of the
components of the @nt{record_extension_part} shall be nonlimited.@rquotes@;
@begin{Example}
@key[generic]
    @key[type] Parent @key[is] @key[tagged] @key[private];
    @key[type] Comp @key[is] @key[limited] @key[private];
@key[package] G1 @key[is]
    @key[type] Extension @key[is] @key[new] Parent @key[with]
        @key[record]
            C : Comp; --@RI{ Illegal!}
        @key[end] @key[record];
@key[end] G1;
@end{Example}

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00114}
The parent type is nonlimited, and the component type is limited,
which is illegal.
It doesn't matter that @Chg{New=[],Old=[an ]}one could imagine writing an
instantiation with the actual for Comp being nonlimited @em we never get to
the instance, because the generic itself is illegal.

@leading@;On the other hand:
@begin{Example}
@key[generic]
    @key[type] Parent @key[is] @key[tagged] @key[limited] @key[private]; --@RI{ Parent is limited.}
    @key[type] Comp @key[is] @key[limited] @key[private];
@key[package] G2 @key[is]
    @key[type] Extension @key[is] @key[new] Parent @key[with]
        @key[record]
            C : Comp; --@RI{ OK.}
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
                         Comp => Limited_Untagged); --@RI{ Illegal!}
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
it is equivalent to the instance declaration (a @nt{package_@!declaration}
or @nt{subprogram_@!declaration}) immediately followed by the instance body,
both at the place of the instantiation.
@begin{Ramification}
The declaration and the body of the instance are not @lquotes@;implicit@rquotes@;
in the technical sense, even though you can't see them in the program text.
Nor are declarations within an instance @lquotes@;implicit@rquotes@;
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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
Other properties of the copy (for example, staticness,
@Chg{Version=[2],New=[categories],Old=[classes]} to
which types belong) are recalculated for each instance;
this is implied by the fact that it's a copy.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00317-01]}
Although the @nt{generic_formal_part} is included in an instance,
the declarations in the @nt{generic_formal_part} are only visible
outside the instance in the case of a generic formal package whose
@nt{formal_package_actual_part} @Chg{Version=[2],New=[includes one or more
<> indicators],Old=[is (<>)]}
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
@lquotes@;interpretation@rquotes@; and @lquotes@;overloading rule.@rquotes@;

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

This rule is partly a ramification of the @lquotes@;current instance@rquotes@;
rule in @RefSec{The Context of Overload Resolution}.
Note that that rule doesn't cover the @nt{generic_formal_part}.

Although the overloading rules are not observed in the instance,
they are, of course, observed in the @ntf{_instantiation} in order to
determine the interpretation of the constituents of the
@ntf{_instantiation}.

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
  there are no @lquotes@;properties@rquotes@; of types and subtypes
  that come from the formal.
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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  A @nt{formal_type_declaration} can contain
  @nt{discriminant_specification}s,
  a @nt{formal_subprogram_declaration} can contain
  @Chg{Version=[2],New=[@nt{parameter_specification}s],Old=[@ntf{formal_parameter_specification}s]}, and
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
  (See AI83-00409.)

  Similarly, if a generic actual is a static expression and
  the corresponding formal parameter has a
  static @Redundant[(scalar or string)] subtype,
  then each use of the formal parameter in the specification of the
  instance is considered to be static.
  (See AI83-00505.)

  @leading@;If a primitive subprogram of a type derived from a generic formal
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
    @key[procedure] Foo(X : @key[in] Derived_From_Formal); --@RI{ Does not override anything.}
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
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0009],ARef=[AI95-00137-01]}
For formal types,
an implementation that shares the code among multiple instances of the
same generic unit needs to beware that things like parameter passing
mechanisms (by-copy vs. by-reference) and
@Chg{New=[@nt{aspect_clause}s],Old=[@nt{representation_clause}s]} are
determined by the actual.
@end{ImplNote}

@redundant[Implicit declarations are also copied,
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
the copied ones. The copied ones
can be called only from within the instance; the new
ones can be called only from outside the instance, although
for tagged types, the
body of a new one can be executed by a call to an old one.]
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

AI83-00398.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
Since an actual type is always in the
@Chg{Version=[2],New=[category],Old=[class]} determined for
the formal, the new subprograms hide all of the copied ones,
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
@leading@;For example:
@begin{Example}
@key[type] Ancestor @key[is] @key[tagged] @key[null] @key[record];

@key[generic]
    @key[type] Formal @key[is] @key[new] Ancestor @key[with] @key[private];
@key[package] G @key[is]
    @key[type] T @key[is] @key[new] Formal @key[with] @key[null] @key[record];
    @key[procedure] P(X : @key[in] T); --@RI{ (1)}
@key[private]
    @key[procedure] Q(X : @key[in] T); --@RI{ (2)}
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
@ntf{_instantiation} need not look at the private part of the generic in
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
kind of construct @em we don't actually @lquotes@;evaluate@rquotes@; @nt{subtype_mark}s.
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
these subprograms from outside the instance are ambiguous. For example:
@begin{Example}
@key[generic]
   @key[type] A @key[is] (<>);
   @key[type] B @key[is] @key[private];
@key[package] G @key[is]
   @key[function] Next(X : A) @key[return] A;
   @key[function] Next(X : B) @key[return] B;
@key[end] G;

@key[package] P @key[is] @key[new] G(A => Boolean, B => Boolean);
--@RI{ All calls of P.Next are ambiguous.}
@end{Example}
@end{Ramification}
@begin{Ramification}
@leading@;The following example illustrates some of the subtleties of the
substitution of formals and actuals:
@begin{Example}
@key[generic]
    @key[type] T1 @key[is] @key[private];
    --@RI{ A predefined "=" operator is implicitly declared here:}
    --@RI{ function "="(Left, Right : T1) return Boolean;}
    --@RI{ Call this "="@-{1}.}
@key[package] G @key[is]
    @key[subtype] S1 @key[is] T1; --@RI{ So we can get our hands on the type from}
                      --@RI{ outside an instance.}
    @key[type] T2 @key[is] @key[new] T1;
    --@RI{ An inherited "=" operator is implicitly declared here:}
    --@RI{ function "="(Left, Right : T2) return Boolean;}
    --@RI{ Call this "="@-{2}.}

    T1_Obj : T1 := ...;
    Bool_1 : Boolean := T1_Obj = T1_Obj;

    T2_Obj : T2 := ...;
    Bool_2 : Boolean := T2_Obj = T2_Obj;
@key[end] G;
...

@key[package] P @key[is]
    @key[type] My_Int @key[is] @key[new] Integer;
    --@RI{ A predefined "=" operator is implicitly declared here:}
    --@RI{ function "="(Left, Right : My_Int) return Boolean;}
    --@RI{ Call this "="@-{3}.}
    @key[function] "="(X, Y : My_Int) @key[return] Boolean;
    --@RI{ Call this "="@-{4}.}
    --@RI{ "="@-{3} is hidden from all visibility by "="@-{4}.}
    --@RI{ Nonetheless, "="@-{3} can @lquotes@;reemerge@rquotes@; in certain circumstances.}
@key[end] P;
@key[use] P;
...
@key[package] I @key[is] @key[new] G(T1 => My_Int); --@RI{ "="@-{5} is declared in I (see below).}
@key[use] I;

Another_T1_Obj : S1 := 13; --@RI{ Can't denote T1, but S1 will do.}
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
@leading@keepnext@i{Examples of generic instantiations (see
@RefSecNum{Generic Declarations}):}
@begin{Example}
@tabclear()@tabset(P49)
@key[procedure] Swap @key[is] @key[new] Exchange(Elem => Integer);
@key[procedure] Swap @key[is] @key[new] Exchange(Character);  @\--@RI{  Swap is overloaded }
@key[function] Square @key[is] @key[new] Squaring(Integer); @\--@RI{  "*" of Integer used by default}
@key[function] Square @key[is] @key[new] Squaring(Item => Matrix, "*" => Matrix_Product);
@key[function] Square @key[is] @key[new] Squaring(Matrix, Matrix_Product); --@RI{ same as previous    }

@key[package] Int_Vectors @key[is] @key[new] On_Vectors(Integer, Table, "+");
@end{Example}

@begin{Wide}
@leading@keepnext@i{Examples of uses of instantiated units:}
@end{Wide}
@begin{Example}
Swap(A, B);
A := Square(A);

T : Table(1 .. 5) := (10, 20, 30, 40, 50);
N : Integer := Int_Vectors.Sigma(T);  --@RI{  150 (see @RefSec{Generic Bodies} for the body of Sigma)}

@key[use] Int_Vectors;
M : Integer := Sigma(T);  --@RI{  150}
@end{Example}
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
In Ada 83, all explicit actuals are evaluated before all defaults,
and the defaults are evaluated in the order of the formal
declarations.
This ordering requirement is relaxed in Ada 95.
@end{Inconsistent83}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
We have attempted to remove every violation of the contract model.
Any remaining contract model violations should be considered bugs in
the RM95.
The unfortunate property of reverting to the predefined operators of
the actual types is retained for upward compatibility.
(Note that fixing this would require subtype conformance rules.)
However, tagged types do not revert in this sense.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt{explicit_generic_actual_parameter} is modified to allow a
@SynI{package_instance_}@nt{name}.
@end{Extend83}

@begin{DiffWord83}
The fact that named associations cannot be used for two formal
subprograms with the same defining name is moved to AARM-only material,
because it is a ramification of other rules, and because it is not of
interest to the average user.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
The rule that
@lquotes@;An explicit @nt{explicit_generic_actual_parameter} shall not be supplied more
than once for a given @Chg{Version=[2],New=[generic formal parameter],
Old=[@ntf{generic_formal_parameter}]}@rquotes@;
seems to be missing from RM83, although it was clearly the intent.

In the explanation that the instance is a copy of the template,
we have left out RM83-12.3(5)'s @lquotes@;apart from the generic formal
part@rquotes@;, because it seems that things in the formal part still need to
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
entities, and the instance is not @lquotes@;implicit.@rquotes@;

In RM83, there is a rule saying the formal and actual shall match, and
then there is much text defining what it means to match.
Here, we simply state all the latter text as rules.
For example, @lquotes@;A formal foo is matched by an actual greenish bar@rquotes@;
becomes @lquotes@;For a formal foo, the actual shall be a greenish bar.@rquotes@;
This is necessary to split the @ResolutionName@;s
from the @LegalityName@;s.
Besides, there's really no need to define the concept of matching for
generic parameters.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00218-03]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  An @nt{overriding_indicator} (see
  @RefSecNum{Overriding Indicators}) is allowed on a subprogram instantiation.]}
@end{Extend95}


@LabeledClause{Formal Objects}

@begin{Intro}
@redundant[@Defn{generic formal object}
@Defn{formal object, generic}
A generic formal object can be used to pass a value or variable
to a generic unit.]
@end{Intro}

@begin{MetaRules}
A generic formal object of mode @key{in} is like a constant
initialized to the value of the @nt{explicit_generic_actual_parameter}.

A generic formal object of mode @key{in out} is like a renaming
of the @nt{explicit_generic_actual_parameter}.
@end{MetaRules}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00423-01]}
@Syn{lhs=<formal_object_declaration>,rhs="
    @Syn2{defining_identifier_list} : @Syn2{mode} @Chg{Version=[2],New=<[@Syn2{null_exclusion}] >,Old=<>}@Syn2{subtype_mark} [:= @Syn2{default_expression}];@Chg{Version=[2],New=<
    @Syn2{defining_identifier_list} : @Syn2{mode} @Syn2{access_definition} [:= @Syn2{default_expression}];>,Old=<>}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(generic formal object default_expression)}
The expected type for the @nt{default_expression}, if any, of a formal
object is the type of the formal object.


@PDefn2{Term=[expected type], Sec=(generic formal in object actual)}
For a generic formal object of mode @key[in],
the expected type for the actual is the type of the formal.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00423-01]}
For a generic formal object of mode @key[in out],
the type of the actual shall resolve to the type @Chg{Version=[2],
New=[determined by the @nt{subtype_mark}, or for a
@nt{formal_object_declaration} with an @nt{access_definition}, to a specific
anonymous access type. If the anonymous access type is an access-to-object type,
the type of the actual shall
have the same designated type as that of the @nt{access_definition}.
If the anonymous access type is an access-to-subprogram type, the type
of the actual shall have a designated profile which
is type conformant with that of the @nt{access_definition}.
@Defn2{Term=[type conformance],Sec=(required)}],Old=[of the formal]}.
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


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00423-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}@ChgNote{Conditional leading}
@Chg{Version=[2],New=[In the case where the type of the formal is defined by an
@nt{access_definition}, the type of the actual and the type of the formal:],
Old=[The type of a generic formal object of mode
@key{in} shall be nonlimited.]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00423-01]}
  @ChgAdded{Version=[2],Text=[shall both be access-to-object types with
  statically matching designated subtypes and with both or neither being
  access-to-constant types; or
  @PDefn2{Term=[statically matching],Sec=(required)}]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00423-01]}
  @ChgAdded{Version=[2],Text=[shall both be access-to-subprogram types with
  subtype conformant designated profiles.
  @Defn2{Term=[subtype conformance],Sec=(required)}]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00423-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For a
@nt{formal_object_declaration} with a @nt{null_exclusion} or an
@nt{access_definition} that has a @nt{null_exclusion}:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[if the actual matching the
  @nt{formal_object_declaration} denotes the generic formal object
  of another generic unit @i{G}, and the instantiation containing the actual
  occurs within the body
  of @i{G} or within the body of a generic unit declared within the declarative
  region of @i{G}, then the declaration of the formal object of @i{G}
  shall have a @nt{null_exclusion};]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[otherwise, the subtype of the actual
  matching the @nt{formal_object_declaration} shall exclude null.
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00423-01]}
  @Chg{Version=[2],New=[This rule prevents @lquotes@;lying@rquotes.
  @b<Null> must never be the value of an object with an explicit
  @nt{null_exclusion}. The first bullet is an assume-the-worst rule
  which prevents trouble in generic bodies (including bodies of child
  units) when the subtype of the formal object excludes null implicitly.],
  Old=[Since a generic formal object is like a
  constant of mode @key{in} initialized to the value of the actual,
  a limited type would not make sense, since initializing a constant is
  not allowed for a limited type.
  That is, generic formal objects of mode @key{in} are passed by copy,
  and limited types are not supposed to be copied.]}
@end{Reason}
@end{Itemize}

@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00255-01],ARef=[AI95-00423-01]}
A @nt{formal_object_declaration} declares a generic formal object.
The default mode is @key{in}.
@PDefn2{Term=[nominal subtype], Sec=(of a generic formal object)}
For a formal object of mode @key{in},
the nominal subtype is the one denoted by the
@nt{subtype_mark} @Chg{Version=[2],New=[or @nt{access_definition} ],Old=[]}in
the declaration of the formal.
@PDefn2{Term=[static], Sec=(subtype)}
For a formal object of mode @key{in out}, its type
is determined by the @nt<subtype_mark> @Chg{Version=[2],
New=[or @nt{access_definition} ],Old=[]}in the declaration;
its nominal subtype is nonstatic, even if the
@nt<subtype_mark> denotes a static subtype@Chg{Version=[2],
New=[; for a composite type, its nominal subtype is unconstrained if the first
subtype of the type is unconstrained@Redundant[, even if the @nt{subtype_mark}
denotes a constrained subtype]],Old=[]}.
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00255-01]}
  @ChgAdded{Version=[2],Text=[We require that the subtype is
  unconstrained because a formal @key{in out} acts like a renaming, and
  thus the given subtype is ignored for purposes of matching; any value of
  the type can be passed. Thus we can assume only that the object is
  constrained if the first subtype is constrained (and thus there can be
  no unconstrained subtypes for the type). If we didn't do this, it
  would be possible to
  rename or take 'Access of components that could disappear due to an
  assignment to the whole object.]}
@end{Reason}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00423-01]}
  @ChgAdded{Version=[2],Text=[The two @lquotes@;even if@rquotes clauses are
  OK even though they don't mention @nt{access_definition}s; an access subtype
  can neither be a static subtype nor be a composite type.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00269-01]}
@Chg{Version=[2],New=[@Defn2{Term=[full constant declaration],
  Sec=(corresponding to a formal object of mode @key[in])}],Old=[]}
@Defn2{Term=[stand-alone constant],
  Sec=(corresponding to a formal object of mode @key[in])}
@PDefn{stand-alone object}
In an instance,
a @nt{formal_object_declaration} of mode @key{in}
@Chg{Version=[2],New=[is a @i<full constant declaration> and ],
Old=[]}declares a new stand-alone constant
object whose initialization expression is the actual,
whereas a @nt{formal_object_declaration} of mode @key{in out}
declares a view whose properties are identical to those of the actual.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
These rules imply that generic formal objects of mode @key{in} are
passed by copy@Chg{Version=[2],New=[ (or are built-in-place for
a limited type)],Old=[]},
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
@Defn{extensions to Ada 83}
In Ada 83, it is forbidden to pass a (nongeneric) formal parameter
of mode @key{out}, or a subcomponent thereof, to a generic formal
object of mode @key{in out}.
This restriction is removed in Ada 95.
@end{Extend83}

@begin{DiffWord83}
We make @lquotes@;@nt{mode}@rquotes@; explicit in the syntax.
RM83 refers to the mode without saying what it is.
This is also more uniform with the way (nongeneric) formal parameters
are defined.

We considered allowing mode @key{out} in Ada 95,
for uniformity with (nongeneric) formal parameters.
The semantics would be identical for modes @key{in out} and
@key{out}.
(Note that generic formal objects of mode @key{in out} are passed by
reference. Note that for (nongeneric) formal parameters that are
allowed to be passed by reference, the semantics of @key{in out} and
@key{out} is the same. The difference might serve as documentation.
The same would be true for generic formal objects, if @key{out} were
allowed, so it would be consistent.)
We decided not to make this change, because it does not produce any
important benefit, and any change has some cost.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  A generic formal @key{in} object can have
  a limited type. The actual for such an object must be built-in-place
  via a @nt{function_call} or @nt{aggregate}, see @RefSecNum{Limited Types}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00423-01]}
  @ChgAdded{Version=[2],Text=[A generic formal object can have
  a @nt{null_exclusion} or an anonymous access type.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00255-01]}
  @ChgAdded{Version=[2],Text=[Clarified that the nominal subtype of a
  composite formal @key{in out} object is unconstrained if the first subtype
  of the type is unconstrained.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00269-01]}
  @ChgAdded{Version=[2],Text=[Clarified that a formal @key{in} object can
  be static when referenced from outside of the instance (by declaring
  such an object to be a full constant declaration).]}
@end{DiffWord95}


@RMNewPage@Comment{For printed RM Ada 2005}
@LabeledClause{Formal Types}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
@redundant[A generic formal subtype can be used to pass to a generic unit
a subtype whose type is in a certain @Chg{Version=[2],New=[category],
Old=[class]} of types.]
@begin{Reason}
We considered having intermediate syntactic categories
@ntf{formal_integer_type_definition},
@ntf{formal_real_type_definition}, and
@ntf{formal_fixed_point_definition},
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


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
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
    | @Syn2{formal_access_type_definition}@Chg{Version=[2],New=[
    | @Syn2{formal_interface_type_definition}],Old=[]}"}
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
When we say simply @lquotes@;formal@rquotes@; or @lquotes@;actual@rquotes@; (for a generic
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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
@Chg{Version=[2],New=[@Defn{determined category for a formal type}
@Defn{category determined for a formal type}],
Old=[@Defn{determined class for a formal type}
@Defn{class determined for a formal type}]}
The form of a @nt{formal_type_definition} @i{determines a
@Chg{Version=[2],New=[category (of types)],Old=[class]}} to which the
formal type belongs.
For a @nt{formal_private_type_definition} the reserved words
@key{tagged} and @key{limited} indicate the @Chg{Version=[2],New=[category of types],Old=[class]}
(see @RefSecNum{Formal Private and Derived Types}).
For a @nt{formal_derived_type_definition} the
@Chg{Version=[2],New=[category of types],Old=[class]} is
the derivation class rooted at the ancestor type.
For other formal types,
the name of the syntactic category indicates the
@Chg{Version=[2],New=[category of types],Old=[class]};
a @nt{formal_discrete_type_definition} defines a discrete type,
and so on.
@begin{Reason}
This rule is clearer with the flat syntax rule for
@nt{formal_type_definition} given above.
Adding @ntf{formal_integer_type_definition} and others would make this
rule harder to state clearly.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
@ChgAdded{Version=[2],Text=[We use @lquotes@;category@rquote rather than
@lquotes@;class@rquotes above, because the requirement that classes are
closed under derivation is not important here. Moreover, there are
interesting categories that are not closed under derivation. For instance,
limited and interface are categories that do not form classes.]}
@end{Reason}
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
The actual type shall be in the @Chg{Version=[2],New=[category],Old=[class]}
determined for the formal.
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
For example, if the @Chg{Version=[2],New=[category],Old=[class]} determined
for the formal is the @Chg{Version=[2],New=[category],Old=[class]} of all
discrete types, then the actual has to be discrete.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
Note that this rule does not require the actual to belong to every
@Chg{Version=[2],New=[category],Old=[class]} to which the formal belongs.
For example, formal private types are in the
@Chg{Version=[2],New=[category],Old=[class]} of composite types,
but the actual need not be composite.
Furthermore, one can imagine an infinite number of @Chg{Version=[2],
New=[categories],Old=[classes]} that are just
arbitrary sets of types @Chg{Version=[2],New=[],Old=[that obey the
closed-under-derivation rule,
and are therefore technically classes]}
(even though we don't give them names,
since they are uninteresting).
We don't want this rule to apply to @i{those}
@Chg{Version=[2],New=[categories],Old=[classes]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01],ARef=[AI95-00442-01]}
@lquotes@;Limited@rquotes@; is not @Chg{Version=[2],New=[an],Old=[a]}
@lquotes@;interesting@rquotes@; @Chg{Version=[2],New=[category],Old=[class]},
but @lquotes@;nonlimited@rquotes@; is;
it is legal to pass a nonlimited type to a limited formal type,
but not the other way around.
The reserved word @Chg{Version=[2],New=[@key[limited]],Old=[@ntf{limited}]} really represents a
@Chg{Version=[2],New=[category],Old=[class]} containing
both limited and nonlimited types.
@lquotes@;Private@rquotes@; is not a @Chg{Version=[2],New=[category for this purpose],
Old=[class]}; a generic formal private type accepts
both private and nonprivate actual types.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
It is legal to pass a class-wide subtype as the actual
if it is in the right @Chg{Version=[2],New=[category],Old=[class]},
so long as the formal has unknown discriminants.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0037],ARef=[AI95-00043-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00233-01],ARef=[AI95-00442-01]}
@Redundant[The formal type also belongs to each
@Chg{Version=[2],New=[category],Old=[class]} that contains
the determined @Chg{Version=[2],New=[category],Old=[class]}.]
The primitive subprograms of the type are as for any
type in the determined @Chg{Version=[2],New=[category],Old=[class]}. For a
formal type other than a formal
derived type, these are the predefined operators of the type@Chg{New=[.
For an elementary formal type, the predefined operators are implicitly declared
immediately after the declaration of the formal type. For a composite formal
type, the predefined operators are implicitly declared either immediately after
the declaration of the formal type, or later
@Chg{Version=[2],New=[immediately within the declarative region in which the
type is declared],Old=[in its immediate scope]} according
to the rules of @RefSecNum(Private Operations).],
Old=[; they are implicitly declared immediately after the declaration
of the formal type.]} In an instance, the copy of such an
implicit declaration declares a view of the predefined operator
of the actual type, even if this operator has been overridden for
the actual type.
@Redundant[The rules specific to formal derived types are given
in @RefSecNum{Formal Private and Derived Types}.]
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
All properties of the type are as for any type in the @Chg{Version=[2],New=[category],Old=[class]}.
Some examples:
The primitive operations available are as defined by the language for each
@Chg{Version=[2],New=[category],Old=[class]}.
The form of @nt{constraint} applicable to a formal type in a
@nt{subtype_indication} depends on the @Chg{Version=[2],New=[category],Old=[class]} of the type as for a
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
The term @lquotes@;formal floating point type@rquotes@; refers to a type defined by a
@nt{formal_floating_point_definition}.
It does not include
a formal derived type whose ancestor is floating point.
Similar terminology applies to the other kinds of
@nt{formal_type_definition}.
@end{Ramification}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of generic formal types:}
@begin{Example}
@key[type] Item @key[is] @key[private];
@key[type] Buffer(Length : Natural) @key[is] @key[limited] @key[private];

@key[type] Enum  @key[is] (<>);
@key[type] Int   @key[is] @key[range] <>;
@key[type] Angle @key[is] @key[delta] <>;
@key[type] Mass  @key[is] @key[digits] <>;

@key[type] Table @key[is] @key[array] (Enum) @key[of] Item;
@end{Example}

@begin{Wide}
@leading@keepnext@i{Example of a generic formal part declaring a
formal integer type:}
@end{Wide}
@begin{Example}
@key[generic]
   @key[type] Rank @key[is] @key[range] <>;
   First  : Rank := Rank'First;
   Second : Rank := First + 1;  --@RI{  the operator "+" of the type Rank  }
@end{Example}
@end{Examples}

@begin{DiffWord83}
RM83 has separate sections @lquotes@;Generic Formal Xs@rquotes@; and @lquotes@;Matching Rules for
Formal Xs@rquotes@; (for various X's) with most of the text
redundant between the two.
We have combined the two in order to reduce the redundancy.
In RM83, there is no @lquotes@;Matching Rules for Formal Types@rquotes@; section; nor is
there a @lquotes@;Generic Formal Y Types@rquotes@; section (for Y = Private, Scalar, Array,
and Access).
This causes, for example, the duplication across all the @lquotes@;Matching
Rules for Y Types@rquotes@; sections of the rule that the actual passed to a
formal type shall be a subtype;
the new organization avoids that problem.

The matching rules are stated more concisely.

We no longer consider the multiplying
operators that deliver a result of type @i{universal_fixed} to be
predefined for the various types; there is only one of each in
package Standard. Therefore, we need not mention them here as RM83
had to.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0037],ARef=[AI95-00043-01],ARef=[AI95-00233-01]}
  @ChgAdded{Version=[2],Text=[Corrigendum 1 corrected the wording to properly
  define the location where operators are defined for formal array types.
  The wording here was inconsistent with that in @RefSec{Private Operations}.
  For the Amendment, this wording was corrected again, because it didn't
  reflect the Corrigendum 1 revisions in @RefSecNum{Private Operations}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[Formal interface types are defined; see
  @RefSec{Formal Interface Types}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[We use @lquotes@;determines a category@rquotes
  rather than class, since not all interesting properties form a class.]}
@end{DiffWord95}


@LabeledSubClause{Formal Private and Derived Types}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
@Redundant[@Chg{Version=[2],New=[In its most general form, the category],
Old=[The class]}
determined for a formal private type @Chg{Version=[2],New=[is all types,
but it can be restricted to only nonlimited types or to only tagged types],
Old=[can be either limited or nonlimited, and either tagged or untagged;
no more specific class is known for such a type]}.
The @Chg{Version=[2],New=[category],Old=[class]} determined for a formal
derived type is the derivation class rooted at the ancestor type.]
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[The first rule is given normatively below,
  and the second rule is given normatively in
  @RefSecNum{Formal Types}; they are repeated here to give a capsule
  summary of what this subclause is about.]}
@end{TheProof}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_private_type_definition>,
  rhs="[[@key{abstract}] @key{tagged}] [@key{limited}] @key{private}"}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00419-01],ARef=[AI95-00443-01]}
@Syn{lhs=<formal_derived_type_definition>,
  rhs="@Chg{Version=[2],New=[
     ],Old=[]}[@key{abstract}] @Chg{Version=[2],New=<[@key{limited} | @key{synchronized}] >,Old=[]}@key{new} @Syn2{subtype_mark} [@Chg{Version=[2],New=<[@key{and} @Syn2{interface_list}]>,Old=<>}@key{with} @key{private}]"}
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

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01],ARef=[AI95-00419-01],ARef=[AI95-00443-01]}
@Defn2{Term=[ancestor subtype], Sec=(of a formal derived type)}
@PDefn{private extension}
The @i(ancestor subtype) of a formal derived type is the
subtype denoted by the @nt<subtype_mark> of
the @nt<formal_derived_type_definition>.
For a formal derived type declaration,
the reserved words @key{with private} shall appear if and only
if the ancestor type is a tagged type; in this case
the formal derived type is a private extension of the
ancestor type and the ancestor shall not be a class-wide
type.
@Redundant[Similarly, @Chg{Version=[2],New=[an @nt{interface_list} or ],Old=[]}
the optional reserved @Chg{Version=[2],New=[words],Old=[word]} @key{abstract}
@Chg{Version=[2],New=[or @key{synchronized} ],Old=[]}shall
appear only if the ancestor type is a tagged type].@Chg{Version=[2],
New=[ The reserved word @key{limited} or @key{synchronized} shall appear
only if the ancestor type @Redundant[and any progenitor types] are limited types.
The reserved word @key{synchronized} shall appear (rather than @key{limited}) if
the ancestor type or any of the progenitor types are
synchronized interfaces.],Old=[]}

@begin{Reason}
We use the term @lquotes@;ancestor@rquotes@; here instead of @lquotes@;parent@rquotes@;
because the actual can be any descendant of the ancestor,
not necessarily a direct descendant.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
@ChgAdded{Version=[2],Text=[We require the ancestor type to be limited when
@key{limited} appears so that we avoid oddies like limited integer types.
Normally, @key{limited} means @lquotes@;match anything@rquotes for a generic
formal, but it was felt that allowing limited elementary types to be declared
was just too weird. Integer still matches a formal limited private type;
it is only a problem when the type is known to be elementary.
Note that the progenitors are required to be limited by rules in
@RefSecNum{Interface Types}, thus that part of the rule is redundant.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00443-01]}
@ChgAdded{Version=[2],Text=[We require that @key{synchronized} appear if the
ancestor or any
of the progenitors are synchronized, so that property is explicitly given
in the program text @en it is not automatically inherited from the ancestors.
However, it can be given even if neither the ancestor nor the progenitors
are synchronized.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01],ARef=[AI95-00401-01],ARef=[AI95-00443-01]}
@ChgAdded{Version=[2],Text=[The actual type for a formal derived type
shall be a descendant of @Redundant[the ancestor type and] every progenitor of
the formal type. If the reserved word @key[synchronized] appears
in the declaration of the formal derived type, the actual
type shall be a synchronized tagged type.]}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The actual type has to be a descendant of the
  ancestor type, in order that it be in the correct class. Thus, that part
  of the rule is redundant.]}
@end{TheProof}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[For a non-formal private extension, we
  require the partial view to be synchronized if the full view is synchronized
  tagged. This does not apply to a formal private extension @em it is OK if
  the formal is not synchronized. Any attempt to extend the formal
  type will be rechecked in the instance, where the rule disallowing
  extending a sychronized non-interface type will be enforced. This is
  consistent with the @lquotes@;no hidden interfaces@rquotes rule also
  applying only to non-formal private extensions, as well as the rule that
  a limited non-formal private extension implies a limited full type.
  Formal private extensions are exempted from all these rules to
  enable the construction of generics that can be used with the widest
  possible range of types. In particular, an indefinite tagged
  limited formal private type can match any @lquotes@;concrete@rquotes
  actual tagged type.]}
@end{Discussion}

If the formal subtype is definite, then the actual subtype shall
also be definite.
@begin{Ramification}
On the other hand, for an indefinite formal subtype,
the actual can be either definite or indefinite.
@end{Ramification}

@leading@;For a generic formal derived type with no @nt<discriminant_part>:
@begin(Itemize)
  If the ancestor subtype is constrained,
  the actual subtype shall be constrained,
  and shall be statically compatible with the ancestor;
@begin{Ramification}
  In other words, any constraint on the ancestor subtype is considered
  part of the @lquotes@;contract.@rquotes@;
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

  If the ancestor subtype is an unconstrained discriminated subtype, then
  the actual shall have the same number of discriminants, and each
  discriminant of the actual shall correspond to a discriminant of the
  ancestor, in the sense of @RefSecNum{Discriminants}.

@begin{Reason}
  This ensures that if a discriminant constraint is given on
  the formal subtype, the corresponding constraint in the instance
  will make sense, without additional run-time checks.
  This is not necessary for arrays, since the bounds cannot be overridden
  in a type extension. An @nt<unknown_discriminant_part> may be used
  to relax these matching requirements.
@end{Reason}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[If the ancestor subtype is an access subtype, the
  actual subtype shall exclude null if and only if the ancestor subtype
  excludes null.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We require that the
  @lquotes@;excludes null@rquotes property match, because it would be difficult
  to write a correct generic for a formal access type without knowing this
  property. Many typical algorithms and techniques will not work for a
  subtype that excludes null (setting an unused component to @key{null},
  default-initialized objects, and so on). We want this sort of requirement to
  be reflected in the contract of the generic.]}

@end{Reason}

@end(Itemize)

@Leading@;The declaration of a formal derived type shall not have a
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
@lquotes@;subtype conformance@rquotes@; for @nt{discriminant_part}s.
We rejected that idea, because it would require implicit (inherited)
@nt{discriminant_part}s, which seemed like too much mechanism.
@end{Reason}
@end{Itemize}

@Redundant[For a generic formal type with an
@nt{unknown_discriminant_part},
the actual may, but need not, have discriminants,
and may be definite or indefinite.]

@end{Legality}

@begin{StaticSem}
@leading@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
The @Chg{Version=[2],New=[category],Old=[class]} determined for a formal
private type is as follows:
@ChgRef{Version=[2],Kind=[Revised]}
@TabClear{}@Tabset(P32)
@begin{Display}
@i(Type Definition) @\@i(Determined @Chg{Version=[2],New=[Category],Old=[Class]})@*
@key{limited private} @\the @Chg{Version=[2],New=[category],Old=[class]} of all types
@key{private} @\the @Chg{Version=[2],New=[category],Old=[class]} of all nonlimited types
@key{tagged limited private} @\the @Chg{Version=[2],New=[category],Old=[class]} of all tagged types
@key{tagged private} @\the @Chg{Version=[2],New=[category],Old=[class]} of all nonlimited tagged types
@end{Display}

@Redundant[The presence of the reserved word @key{abstract} determines
whether the actual type may be abstract.]

A formal private or derived type is a private or derived type,
respectively.
A formal derived tagged type is a private extension.
@Redundant[A formal private or derived type is abstract if the reserved
word @key(abstract) appears in its declaration.]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00233-01]}
If the ancestor type is a composite type that is not an
array type, the formal type inherits components from the ancestor
type (including
discriminants if a new @nt<discriminant_part> is not specified),
as for a derived type defined by a @nt<derived_type_definition>
(see @RefSecNum(Derived Types and Classes)@Chg{Version=[2],New=[ and
@RefSecNum{Private Operations}],Old=[]}).

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0038],ARef=[AI95-00202]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00233-01],ARef=[AI95-00401-01]}
For a formal derived type,
the predefined operators and inherited user-defined subprograms are determined
by the ancestor type@Chg{Version=[2],New=[ and any progenitor types],Old=[]}, and
are implicitly declared at the earliest place, if any,
@Chg{Version=[2],New=[immediately within the declarative region in which],
Old=[within the immediate scope of]} the formal
type@Chg{Version=[2],New=[ is declared],Old=[]}, where the corresponding
primitive subprogram of the ancestor @Chg{Version=[2],New=[or progenitor
],Old=[]}is visible (see @RefSecNum{Private Operations}). In an instance, the
copy of such an implicit declaration declares a view of the corresponding
primitive subprogram of the ancestor@Chg{New=[@Chg{Version=[2], New=[ or
progenitor],Old=[]} of the formal derived type],Old=[]}, even if this primitive
has been overridden for the actual type. @Chg{New=[When the
ancestor@Chg{Version=[2], New=[ or progenitor],Old=[]} of the formal derived
type is itself a formal type, the copy of the implicit declaration declares a
view of the corresponding copied operation of the ancestor@Chg{Version=[2],
New=[ or progenitor],Old=[]}.],Old=[]} @Redundant[In the case of a formal
private extension, however, the tag of the formal type is that of the actual
type, so if the tag in a call is statically determined to be that of the formal
type, the body executed will be that corresponding to the actual type.]
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
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
ancestor type@Chg{Version=[2],New=[ (or progenitor type, for subprograms
inherited from an interface type)],Old=[]}.
@end{Ramification}

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Leading@;For @ChgPrefixType{Version=[1],Kind=[Revised],Text=[a
@Chg{New=[@nt{prefix}],Old=[prefix]} S that denotes a formal indefinite subtype]},
the following attribute is defined:
@begin{Description}
@Attribute{Prefix=<S>, AttrName=<Definite>,
  Text=[S'Definite yields True if the actual subtype corresponding
    to S is definite; otherwise it yields False. The value of this
    attribute is of the predefined type Boolean.]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
Whether an actual subtype is definite or indefinite may
have a major effect on the algorithm used in a generic.
For example, in a generic I/O package, whether to use fixed-length or
variable-length records could depend on whether the actual is
definite or indefinite.
This attribute is essentially a replacement for the Constrained
attribute@Chg{Version=[2],New=[,],Old=[]}
which is now considered obsolete.
@end{Discussion}
@end{Description}
@EndPrefixType{}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00158-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[In the case where a formal type is
tagged with unknown discriminants, and the actual type is a class-wide type
@i<T>'Class:]}

@begin{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00158-01]}
@ChgAdded{Version=[2],Text=[For the purposes of defining the primitive
operations of the formal type, each of the primitive operations of the actual
type is considered to be a subprogram (with an intrinsic calling convention @em
see @RefSecNum{Conformance Rules}) whose body consists of a dispatching call
upon the corresponding operation of @i<T>, with its formal parameters as the
actual parameters. If it is a function, the result of the dispatching call is
returned.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00158-01]}
@ChgAdded{Version=[2],Text=[If the corresponding operation of @i<T>
has no controlling formal
parameters, then the controlling tag value is determined by the
context of the call, according to the rules for tag-indeterminate
calls (see @RefSecNum{Dispatching Operations of Tagged Types} and
@RefSecNum{Assignment Statements}). In the case where the tag would be
statically determined to be that of the formal type, the call raises
Program_Error. If such a function is renamed, any call on the
renaming raises Program_Error.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}]}

@end{Itemize}


@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[As it states in @RefSecNum{Conformance Rules},
the convention of an inherited subprogram
of a generic formal tagged type with unknown discriminants is intrinsic.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[In the case of a corresponding
primitive of T with no controlling
formal parameters, the context of the call provides the controlling
tag value for the dispatch. If no tag is provided by context,
Program_Error is raised rather than resorting to a nondispatching
call. For example:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} NT(<>) @key{is new} T @key{with private};
    -- @RI[Assume T has operation "]@key{function} Empty @key{return} T;@RI["]
@key{package} G @key{is}
   @key{procedure} Test(X : @key{in out} NT);
@key{end} G;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package body} G @key{is}
   @key{procedure} Test(X : @key{in out} NT) @key{is}
   @key{begin}
      X := Empty;  -- @RI[Dispatching based on X'Tag takes]
                   -- @RI[place if actual is class-wide.]
      @key{declare}
          Y : NT := Empty;
                   -- @RI[If actual is class-wide, this raises Program_Error]
                   -- @RI[as there is no tag provided by context.]
      @key{begin}
          X := Y;  -- @RI[We never get this far.]
      @key{end};
   @key{end} Test;
@key{end} G;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} T1 @key{is new} T @key{with null record};
@key{package} I @key{is new} G(T1'Class);]}
@end{Example}
@end{Discussion}
@end{RunTime}

@begin{Notes}
@Leading@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
In accordance with the general rule that the actual type shall
belong to the @Chg{Version=[2],New=[category],Old=[class]} determined for the formal
(see @RefSec(Formal Types)):
@begin(itemize)
  If the formal type is nonlimited, then so shall be the actual;

  For a formal derived type, the actual shall be in the class rooted
  at the ancestor subtype.
@end(itemize)

The actual type can be abstract only if the formal type is abstract
(see @RefSecNum{Abstract Types and Subprograms}).
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
@Defn{incompatibilities with Ada 83}
Ada 83 does not have
@nt{unknown_discriminant_part}s, so it allows indefinite
subtypes to be passed to definite formals,
and applies a legality rule to the instance body.
This is a contract model violation.
Ada 95 disallows such cases at the point of the instantiation.
The workaround is to add (<>)
as the @nt{discriminant_part} of
any formal subtype if it is intended
to be used with indefinite actuals.
If that's the intent, then there can't be anything
in the generic body that would require a definite subtype.

The check for discriminant subtype matching is changed from a
run-time check to a compile-time check.
@end{Incompatible83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00401-01],ARef=[AI95-00419-01],ARef=[AI95-00443-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  A generic formal derived type can include progenitors (interfaces) as well
  as a primary ancestor. It also may include @key{limited} to indicate that
  it is a limited type, and @key{synchronized} to indicate that it is a
  synchronized type.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0038],ARef=[AI95-00202-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected wording to define the
  operations that are inherited when the ancestor of a formal type is itself
  a formal type to avoid anomalies.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00158-01]}
  @ChgAdded{Version=[2],Text=[Added a semantic description of the meaning
  of operations of an actual class-wide type, as such a type does not have
  primitive operations of its own.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[Added a matching rule for access subtypes that
  exclude null.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00233-01]}
  @ChgAdded{Version=[2],Text=[The wording for the declaration of implicit
  operations is corrected to be consistent with @RefSecNum{Private Operations}
  as modified by Corrigendum 1.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[We change to
  @lquotes@;determines a category@rquotes as that is the new terminology
  (it avoids confusion, since not all interesting properties form a class).]}
@end{DiffWord95}


@LabeledSubClause{Formal Scalar Types}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
A @i{formal scalar type} is one defined by any of the
@nt{formal_type_definition}s in this subclause.
@Redundant[The @Chg{Version=[2],New=[category],Old=[class]} determined for a
formal scalar type is @Chg{Version=[2],New=[the category of all ],Old=[]}discrete,
signed integer, modular, floating point, ordinary fixed point, or
decimal@Chg{Version=[2],New=[ types],Old=[]}.]
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[The second rule follows from the rule in
  @RefSecNum{Formal Types} that says that the category is determined by the
  one given in the name of the syntax production. The effect of the rule
  is repeated here to give a capsule
  summary of what this subclause is about.]}
@end{TheProof}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[The @lquotes@;category of a type@rquotes
  includes any classes that the type belongs to.]}
@end{Ramification}
@end{Intro}


@begin{Syntax}
@Syn{lhs=<formal_discrete_type_definition>,rhs="(<>)"}


@Syn{lhs=<formal_signed_integer_type_definition>,rhs="@key{range} <>"}
@Syn{lhs=<formal_modular_type_definition>,rhs="@key{mod} <>"}


@Syn{lhs=<formal_floating_point_definition>,rhs="@key{digits} <>"}


@Syn{lhs=<formal_ordinary_fixed_point_definition>,rhs="@key{delta} <>"}
@Syn{lhs=<formal_decimal_fixed_point_definition>,rhs="@key{delta} <> @key{digits} <>"}
@end{Syntax}

@begin{Legality}
The actual type for a formal scalar type
shall not be a nonstandard numeric type.
@begin{Reason}
This restriction is necessary because nonstandard numeric
types have some number of restrictions on their use, which could cause
contract model problems in a generic body. Note that nonstandard
numeric types can be passed to formal derived and formal private
subtypes, assuming they obey all the other rules, and assuming the
implementation allows it (being nonstandard means the implementation
might disallow anything).
@end{Reason}
@end{Legality}

@begin{Notes}
The actual type shall be in the class of types implied
by the syntactic category of the formal type definition
(see @RefSec(Formal Types)). For example, the actual for a
@nt<formal_modular_type_definition> shall be a modular type.
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[We change to
  @lquotes@;determines a category@rquotes as that is the new terminology
  (it avoids confusion, since not all interesting properties form a class).]}
@end{DiffWord95}


@LabeledSubClause{Formal Array Types}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
@Redundant[The @Chg{Version=[2],New=[category],Old=[class]} determined for a
formal array type is the @Chg{Version=[2],New=[category],Old=[class]} of all array types.]
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[This rule follows from the rule in
  @RefSecNum{Formal Types} that says that the category is determined by the
  one given in the name of the syntax production. The effect of the rule
  is repeated here to give a capsule
  summary of what this subclause is about.]}
@end{TheProof}
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

@Leading@;For a formal array subtype, the actual subtype shall satisfy the
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
@Leading@Keepnext@i{Example of formal array types:}
@begin{Example}
--@RI{  given the generic package }

@key[generic]
   @key[type] Item   @key[is] @key[private];
   @key[type] Index  @key[is] (<>);
   @key[type] Vector @key[is] @key[array] (Index @key[range] <>) @key[of] Item;
   @key[type] Table  @key[is] @key[array] (Index) @key[of] Item;
@key[package] P @key[is]
   ...
@key[end] P;

--@RI{  and the types }

@key[type] Mix    @key[is] @key[array] (Color @key[range] <>) @key[of] Boolean;
@key[type] Option @key[is] @key[array] (Color) @key[of] Boolean;

--@RI{  then Mix can match Vector and Option can match Table }

@key[package] R @key[is] @key[new] P(Item   => Boolean, Index => Color,
                   Vector => Mix,     Table => Option);

--@RI{  Note that Mix cannot match Table and Option cannot match Vector}
@end{Example}
@end{Examples}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
The check for matching of component subtypes and index subtypes or
index ranges is changed from a
run-time check to a compile-time check.
The Ada 83 rule that @lquotes@;If the component type is not a scalar type,
then the component subtypes shall be either both constrained or both
unconstrained@rquotes@; is removed, since it is subsumed by static matching.
Likewise, the rules requiring that component types be
the same is subsumed.
@end{Incompatible83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[We change to
  @lquotes@;determines a category@rquotes as that is the new terminology
  (it avoids confusion, since not all interesting properties form a class).]}
@end{DiffWord95}


@RMNewPage@Comment{For printed RM Ada 2005}
@LabeledSubClause{Formal Access Types}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
@Redundant[The @Chg{Version=[2],New=[category],Old=[class]} determined for a
formal access type is the @Chg{Version=[2],New=[category],Old=[class]} of all access types.]
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[This rule follows from the rule in
  @RefSecNum{Formal Types} that says that the category is determined by the
  one given in the name of the syntax production. The effect of the rule
  is repeated here to give a capsule
  summary of what this subclause is about.]}
@end{TheProof}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_access_type_definition>,rhs="@Syn2{access_type_definition}"}
@end{Syntax}

@begin{Legality}
For a formal access-to-object type,
the designated subtypes of the formal and actual types shall
statically match.
@PDefn2{Term=[statically matching],Sec=(required)}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
If and only if the @nt{general_access_modifier} @key{constant} applies
to the formal,
the actual shall be an access-to-constant type.
If the @nt{general_access_modifier} @key{all} applies to the
formal, then the actual shall be a general access-to-variable type
(see @RefSecNum{Access Types}).@Chg{Version=[2],New=[ If and only
if the formal subtype excludes null, the actual subtype shall exclude null.],Old=[]}
@begin{Ramification}
If no @ntf{_modifier} applies to the formal, then
the actual type may be either a pool-specific or a general
access-to-variable type.
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0109],ARef=[AI95-00025-01]}
@ChgAdded{Version=[1],Text=[Matching an access-to-variable to a formal
access-to-constant type cannot be allowed. If it were allowed, it would
be possible to create an access-to-variable value designating a constant.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
@ChgAdded{Version=[2],Text=[We require that the @lquotes@;excludes null@rquotes
property match, because it would be difficult to write a correct generic for a
formal access type without knowing this property. Many typical algorithms and
techniques will not work for a subtype that excludes null (setting an unused
component to @key{null}, default-initialized objects, and so on). Even
Ada.Unchecked_Deallocation would fail for a subtype that excludes null. Most
generics would end up with comments saying that they are not intended to work
for subtypes that exclude null. We would rather that this sort of requirement
be reflected in the contract of the generic.]}
@end{Reason}

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
@Leading@keepnext@i{Example of formal access types:}
@begin{Example}
--@RI{  the formal types of the generic package }

@key[generic]
   @key[type] Node @key[is] @key[private];
   @key[type] Link @key[is] @key[access] Node;
@key[package] P @key[is]
   ...
@key[end] P;

--@RI{  can be matched by the actual types }

@key[type] Car;
@key[type] Car_Name @key[is] @key[access] Car;

@key[type] Car @key[is]
   @key[record]
      Pred, Succ : Car_Name;
      Number     : License_Number;
      Owner      : Person;
   @key[end] @key[record];

--@RI{  in the following generic instantiation }

@key[package] R @key[is] @key[new] P(Node => Car, Link => Car_Name);
@end{Example}
@end{Examples}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
The check for matching of designated subtypes is changed from a
run-time check to a compile-time check.
The Ada 83 rule that @lquotes@;If the
designated type is other than a scalar type, then the designated
subtypes shall be either both constrained or both unconstrained@rquotes@; is
removed, since it is subsumed by static matching.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Formal access-to-subprogram subtypes and formal general access
types are new concepts.
@end{Extend83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[Added a matching rule for subtypes that exclude
  null.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[We change to
  @lquotes@;determines a category@rquotes as that is the new terminology
  (it avoids confusion, since not all interesting properties form a class).]}
@end{DiffWord95}


@LabeledAddedSubClause{Version=[2],Name=[Formal Interface Types]}

@begin{Intro}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00442-01]}
@ChgAdded{Version=[2],Text=[@Redundant[The category determined for a formal
interface type is the category of all interface types.]]}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[This rule follows from the rule in
  @RefSecNum{Formal Types} that says that the category is determined by the
  one given in the name of the syntax production. The effect of the rule
  is repeated here to give a capsule
  summary of what this subclause is about.]}
@end{TheProof}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Here we're taking advantage
  of our switch in terminology from @lquotes@;determined class@rquotes to
  @lquotes@;determined category@rquotes; by saying @lquotes@;category@rquotes
  rather than @lquotes@;class@rquotes, we
  require that any actual type be an interface type, not just some type
  derived from an interface type.]}
@end{Ramification}


@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<formal_interface_type_definition>,Old=<>}>,
rhs="@Chg{Version=[2],New=<@Syn2{interface_type_definition}>,Old=<>}"}
@end{Syntax}

@begin{Legality}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251],ARef=[AI95-00401]}
@ChgAdded{Version=[2],Text=[The actual type shall be a descendant of every
progenitor of the formal type.]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00345]}
@ChgAdded{Version=[2],Text=[The actual type shall be a limited, task,
protected, or synchronized interface
if and only if the formal type is also, respectively, a limited, task,
protected, or synchronized interface.]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We require the kind of interface type to match
  exactly because without that it is almost impossible to properly implement
  the interface.]}
@end{Discussion}
@end{Legality}

@begin{Examples}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[@key{type} Root_Work_Item @key{is tagged private};]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} Managed_Task @key{is task interface};
   @key{type} Work_Item(<>) @key{is new} Root_Work_Item @key{with private};
@key{package} Server_Manager @key{is}
   @key{task type} Server @key{is new} Managed_Task @key{with}
      @key{entry} Start(Data : @key{in out} Work_Item);
   @key{end} Server;
@key{end} Server_Manager;]}
@end{Example}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Text=[This generic allows an application to establish a
standard interface that all tasks need to implement so they can be managed
appropriately by an application-specific scheduler.]}

@end{Examples}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00345-01],ARef=[AI95-00401-01],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The formal interface type is new.]}
@end{Extend95}


@LabeledClause{Formal Subprograms}

@begin{Intro}
@redundant[@Defn{generic formal subprogram}
@Defn{formal subprogram, generic}
Formal subprograms can be used to pass callable entities to a generic
unit.]
@end{Intro}

@begin{MetaRules}
Generic formal subprograms are like renames of the @nt{explicit_generic_actual_parameter}.
@end{MetaRules}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02]}
@Syn{lhs=<formal_subprogram_declaration>,rhs="@Chg{Version=[2],
New=<@Syn2{formal_concrete_subprogram_declaration}
    | @Syn2{formal_abstract_subprogram_declaration}>,
Old=<@key{with} @Syn2{subprogram_specification} [@key{is} @Syn2{subprogram_default}];>}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<formal_concrete_subprogram_declaration>,Old=<>}>,
rhs="@Chg{Version=[2],New=<
     @key{with} @Syn2{subprogram_specification} [@key{is} @Syn2{subprogram_default}];>,Old=<>}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<formal_abstract_subprogram_declaration>,Old=<>}>,
rhs="@Chg{Version=[2],New=<
     @key{with} @Syn2{subprogram_specification} @key{is abstract} [@Syn2{subprogram_default}];>,Old=<>}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00348-01]}
@Syn{lhs=<subprogram_default>,rhs="@Syn2{default_name} | <>@Chg{Version=[2],New=< | @key{null}>,Old=<>}"}

@Syn{lhs=<default_name>,rhs="@Syn2{name}"}

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02],ARef=[AI95-00348-01]}
@ChgAdded{Version=[2],Text=[A @nt{subprogram_default} of @key{null} shall not
be specified for a formal function or for a
@nt{formal_abstract_subprogram_declaration}.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[There are no null functions because the return
value has to be constructed somehow. We don't allow null for abstract formal
procedures, as the operation is dispatching. It doesn't seem appropriate (or
useful) to say that the implementation of something is null in the formal
type and all possible descendants of that type. This also would define a
dispatching operation that doesn't correspond to a slot in the tag of the
controlling type, which would be a new concept. Finally, additional rules
would be needed to define the meaning of a dispatching null procedure (for
instance, the convention of such a subprogram should be intrinsic, but that's
not what the language says). It doesn't seem worth the effort.]}
@end{Reason}
@end{SyntaxText}

@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected profile], Sec=(formal subprogram default_name)}
The expected profile for the @nt<default_name>, if any, is that of the
formal subprogram.
@begin{Ramification}
  This rule,
  unlike others in this clause, is observed at compile
  time of the @nt{generic_declaration}.

  The evaluation of the @nt{default_name} takes place during the
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

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00423-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[For a parameter or result subtype of
a @nt{formal_subprogram_declaration} that has an explicit @nt{null_exclusion}:]}

@begin{Itemize}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[if the actual matching the
  @nt{formal_subprogram_declaration} denotes a generic formal object of
  another generic unit @i{G}, and the instantiation containing the actual
  that occurs within the body of a generic unit @i{G} or within the body of a
  generic unit declared within the declarative region of
  the generic unit @i{G}, then the corresponding parameter or result type of
  the formal subprogram of @i{G} shall have a @nt{null_exclusion};]}

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[otherwise, the subtype of the corresponding
  parameter or result type of the actual matching the
  @nt{formal_subprogram_declaration} shall exclude null.
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.]}
@end{Itemize}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This rule prevents
  @lquotes@;lying@rquotes.
  @b<Null> must never be the value of a parameter or result with an explicit
  @nt{null_exclusion}. The first bullet is an assume-the-worst rule
  which prevents trouble in generic bodies (including bodies of child generics)
  when the formal subtype excludes null implicitly.]}
@end{Reason}


@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[If a formal parameter of a
@nt{formal_@!abstract_@!subprogram_@!declaration} is of a
specific tagged type @i<T> or of an anonymous access type designating a
specific tagged type @i<T>, @i<T> is called a @i<controlling type> of the
@nt{formal_@!abstract_@!subprogram_@!declaration}. Similarly, if the result
of a @nt{formal_@!abstract_@!subprogram_@!declaration} for a function is of
a specific tagged type @i<T> or of an anonymous access type designating a
specific tagged type @i<T>, @i<T> is called a controlling type of
the @nt{formal_@!abstract_@!subprogram_@!declaration}. A
@nt{formal_@!abstract_@!subprogram_@!declaration} shall have exactly
one controlling type.
@Defn2{Term=[controlling type],Sec=[of a @nt{formal_abstract_subprogram_declaration}]}]}

@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The specific tagged type could be any of
  a formal tagged private type,
  a formal derived type, a formal interface type,
  or a normal tagged type. While the last case doesn't
  seem to be very useful, there isn't any good reason for disallowing it.
  This rule ensures that the operation is a dispatching operation of some
  type, and that we unambiguously know what that type is.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We informally call a subprogram declared by
  a @nt{formal_@!abstract_@!subprogram_@!declaration} an
  @i{abstract formal subprogram},
  but we do not use this term in normative wording.
  @Defn{abstract formal subprogram}
  (We do use it often in these notes.)]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[The actual subprogram for a
@nt{formal_@!abstract_@!subprogram_@!declaration} shall be a
dispatching operation of the controlling type or of the actual type
corresponding to the controlling type.]}

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We mean the controlling type of the
  @nt{formal_@!abstract_@!subprogram_@!declaration}, of course.
  Saying that gets unwieldy and redundant (so says at least one reviewer,
  anyway).]}
@end{Honest}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[This means that the actual is
  either a primitive operation of the
  controlling type, or an abstract formal subprogram. Also note that this
  prevents the controlling type from being class-wide (with one exception
  explained below), as only specific types have primitive operations (and a
  formal subprogram eventually has to have an actual that is a primitive of
  some type). This could happen in a case like:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} T(<>) @key{is tagged private};
   @key{with procedure} Foo (Obj : @key{in} T) @key{is abstract};
@key{package} P ...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} New_P @key{is new} P (Something'Class, Some_Proc);]}
@end{Example}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The instantiation here is always illegal,
  because Some_Proc could never be a primitive operation of Something'Class
  (there are no such operations). That's good, because we want calls to Foo
  always to be dispatching calls.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[Since it is possible for a formal
  tagged type to be instantiated with a class-wide type, it is possible for the
  (real) controlling type to be class-wide in one unusual case:]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} NT(<>) @key{is new} T @key{with private};
   -- @RI[Presume that T has the following primitive operation:]
   -- @key{with procedure} Bar (Obj : @key{in} T);
@key{package} Gr ...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package body} Gr @key{is}
   @key{package} New_P2 @key{is new} P (NT, Foo => Bar);
@key{end} Gr;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} New_Gr @key{is new} Gr (Something'Class);]}
@end{Example}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The instantiation of New_P2 is legal, since
  Bar is a dispatching operation of the actual type of the controlling type
  of the abstract formal subprogram Foo. This is not a problem, since the
  rules given in @RefSecNum{Formal Private and Derived Types} explain how
  this routine dispatches even though its parameter is class-wide.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that this legality rule never needs to be
  rechecked in an instance (that contains a nested instantiation). The rule
  only talks about the actual type of the instantiation; it does not require
  looking further; if the actual type is in fact a formal type, we do not
  intend looking at the actual for that formal.]}
@end{Ramification}
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
@nt{name}s and @nt{default_@!expression}s from the profile given in the
@nt{formal_@!subprogram_@!declaration}. The
view is a function or procedure, never an entry.
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

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00348-01]}
@ChgAdded{Version=[2],Text=[If a generic unit has a @nt{subprogram_default}
specified by the reserved word @key{null}, and the corresponding actual
parameter is omitted, then it is equivalent to an explicit actual parameter
that is a null procedure having the profile given in the
@nt{formal_@!subprogram_@!declaration}.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[The subprogram declared by a
@nt{formal_@!abstract_@!subprogram_@!declaration} with a controlling type @i<T>
is a dispatching operation of type @i<T>.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[This is necessary to trigger all of the
dispatching operation
rules. It otherwise would not be considered a dispatching operation, as
formal subprograms are never primitive operations.]}
@end{Reason}

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
the @ntf{_specification} of the formal subprogram). A similar remark
applies to the result of a function. Therefore, to avoid confusion, it
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
@ntf{_instantiation}.
@begin{TheProof}
Visibility and name resolution are applied to the equivalent explicit
actual parameter.
@end{TheProof}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00260-02]}
The actual subprogram cannot be abstract@Chg{Version=[2],New=[ unless
the formal subprogram is a @nt{formal_@!abstract_@!subprogram_@!declaration}],
Old=[]} (see @RefSecNum{Abstract Types and Subprograms}).

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00260-02]}
@ChgAdded{Version=[2],Text=[The subprogram declared by a
@nt{formal_@!abstract_@!subprogram_@!declaration} is an abstract subprogram.
All calls on a subprogram declared by a
@nt{formal_@!abstract_@!subprogram_@!declaration} must be dispatching calls.
See @RefSecNum{Abstract Types and Subprograms}.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00348-01]}
@ChgAdded{Version=[2],Text=[A null procedure as a subprogram default has
convention Intrinsic (see @RefSecNum{Conformance Rules}).]}
@begin{TheProof}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[This is an implicitly declared subprogram,
so it has convention Intrinsic as defined in @RefSecNum{Conformance Rules}.]}
@end{TheProof}

@end{Notes}

@begin{Examples}
@Leading@Keepnext@i{Examples of generic formal subprograms:}
@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key[with] @key[function] "+"(X, Y : Item) @key[return] Item @key[is] <>;
@key[with] @key[function] Image(X : Enum) @key[return] String @key[is] Enum'Image;
@key[with] @key[procedure] Update @key[is] Default_Update;@Chg{Version=[2],New=[
@key[with] @key[procedure] Pre_Action(X : @key[in] Item) @key[is null];  --@RI[ defaults to no action]
@key[with] @key[procedure] Write(S    : @key[not null access] Root_Stream_Type'Class;
                     Desc : Descriptor)
                     @b<is abstract> Descriptor'Write;  --@RI[ see @RefSecNum{Stream-Oriented Attributes}]
--@RI[ Dispatching operation on Descriptor with default]],Old=[]}

--@RI{  given the generic procedure declaration }

@key[generic]
   @key[with] @key[procedure] Action (X : @key[in] Item);
@key[procedure] Iterate(Seq : @key[in] Item_Sequence);

--@RI{  and the procedure }

@key[procedure] Put_Item(X : @key[in] Item);

--@RI{  the following instantiation is possible }

@key[procedure] Put_List @key[is] @key[new] Iterate(Action => Put_Item);
@end{Example}
@end{Examples}


@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00260-02]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The @nt{formal_abstract_subprogram_declaration} is new. It allows
  the passing of dispatching operations to generic units.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00348-01]}
  @ChgAdded{Version=[2],Text=[
  The formal subprogram default of @key{null} is new. It allows the default
  of a generic procedure to do nothing, such as for passing a debugging
  routine.]}
@end{Extend95}

@begin{Diffword95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00423-01]}
  @ChgAdded{Version=[2],Text=[Added matching rules for @nt{null_exclusion}s.]}
@end{Diffword95}


@LabeledClause{Formal Packages}

@begin{Intro}
@redundant[@Defn{generic formal package}
@Defn{formal package, generic}
Formal packages can be used to pass packages to a generic unit.
The @nt{formal_package_declaration} declares that the formal package
is an instance of a given generic package.
Upon instantiation, the actual package has to be an instance
of that generic package.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<formal_package_declaration>,rhs="
    @key{with} @key{package} @Syn2{defining_identifier} @key{is} @key{new} @SynI{generic_package_}@Syn2{name}  @Syn2{formal_package_actual_part};"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00317-01]}
@Syn{lhs=<formal_package_actual_part>,rhs="
    @Chg{Version=[2],New=`([@key{others} =>] <>)
  | [@Syn2{generic_actual_part}]
  | (@Syn2{formal_package_association} {, @Syn2{formal_package_association}} [, @key{others} => <>])',
Old=[(<>) | [@Syn2{generic_actual_part}]]}"}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00317-01]}
@AddedSyn{Version=[2],lhs=<@Chg{Version=[2],New=<formal_package_association>,Old=<>}>,
rhs="@Chg{Version=[2],New={
    @Syn2{generic_association}
  | @SynI{generic_formal_parameter_}@Syn2{selector_name} => <>},Old={}}"}

@begin{SyntaxText}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00317-01]}
@ChgAdded{Version=[2],Text=[Any positional @nt{formal_package_association}s
shall precede any named @nt{formal_package_association}s.]}
@end{SyntaxText}
@end{Syntax}

@begin{Legality}
@Defn2{Term=[template], Sec=(for a formal package)}
The @i(generic_package_)@nt<name> shall denote a generic package
(the @i(template) for the formal package);
the formal package is an instance of the template.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00398-01]}
@ChgAdded{Version=[2],Text=[A @nt<formal_package_actual_part> shall contain
at most one @nt<formal_package_association> for each formal parameter. If the
@nt<formal_package_actual_part> does not include
@lquotes@key[others] => <>@rquotes, each
formal parameter without an association shall have a @nt<default_expression>
or @nt<subprogram_default>.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00317-01]}
@Leading@;The actual shall be an instance of the template.
If the @nt<formal_package_actual_part> is (<>)@Chg{Version=[2],
New=[ or (@key{others} => <>)],Old=[]},
@Redundant[then the actual may be any instance of the template]; otherwise,
@Chg{Version=[2],New=[certain of the actual parameters],
Old=[each actual parameter]}
of the actual instance shall match the corresponding
actual @Chg{Version=[2],New=[parameters],Old=[parameter]}
of the formal package@Chg{Version=[2],New=[, determined],
Old=[ @Redundant[(whether the
actual parameter is given explicitly or by default)],]} as follows:

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00317-01]}
@ChgAdded{Version=[2],Text=[If the @nt{formal_@!package_@!actual_@!part}
includes @nt{generic_association}s as well as associations with <>,
then only the actual parameters specified explicitly with
@nt{generic_association}s are required to match;]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00317-01]}
@ChgAdded{Version=[2],Text=[Otherwise, all actual parameters shall
match@Redundant[, whether any actual parameter is given explicitly
or by default].]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00317-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[The rules for matching of
actual parameters between the actual instance and the formal package
are as follows:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00317-01]}
For a formal object of mode @key[in]@Chg{Version=[2],New=[,],Old=[]} the
actuals match if they are
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
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0039],ARef=[AI95-00213-01]}
@ChgAdded{Version=[1],Text=[For the purposes of matching, any actual parameter
that is the name
of a formal object of mode @key{in} is replaced by the formal object's actual
expression (recursively).]}
@end{Legality}

@begin{StaticSem}
A @nt{formal_package_declaration} declares a generic formal package.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00317-01]}
@PDefn2{Term=[visible part], Sec=(of a formal package)}
The visible part of a formal package includes
the first list of @nt{basic_declarative_item}s of the
@nt{package_@!specification}.
In addition, @Chg{Version=[2],New=[for each actual parameter that is
not required to match, a copy of the
declaration of the corresponding formal parameter of the template is
included in the visible part of the formal package. If the copied
declaration is for a formal type, copies of the implicit declarations
of the primitive subprograms of the formal type are also included in
the visible part of],
Old=[if the @nt{formal_@!package_@!actual_@!part} is (<>),
it also includes the @nt{generic_@!formal_@!part} of the template
for]} the formal package.

@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00317-01]}
If the @nt<formal_package_actual_part> is (<>),
then the declarations that occur immediately within the
@nt<generic_formal_part> of the template for the formal package
are visible outside the formal package,
and can be denoted by expanded names outside the formal
package.@Chg{Version=[2],New=[If only some of the actual parameters are
given by <>, then the declaration corresponding to those parameters (but
not the others) are made visible.],Old=[]}
@end{Ramification}
@begin{Reason}
We always want either the actuals
or the formals of an instance to be namable from outside, but never both.
If both were namable, one would get some funny anomalies since
they denote the same entity, but, in the case of types at least,
they might have different and inconsistent sets of primitive operators
due to predefined operator @lquotes@;reemergence.@rquotes@; Formal derived types
exacerbate the difference. We want the implicit declarations
of the @nt<generic_formal_part> as well as the explicit
declarations, so we get operations on the formal types.
@end{Reason}
@begin{Ramification}
A generic formal package is a package, and is an instance.
Hence, it is possible to pass a generic formal package
as an actual to another generic formal package.
@end{Ramification}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00317-01]}
@ChgAdded{Version=[2],Text=[
For the purposes of matching, if the actual instance @i<A> is itself a
formal package, then the actual parameters of @i<A> are those specified
explicitly or implicitly in the @nt{formal_package_actual_part} for @i<A>, plus,
for those not specified, the copies of the formal parameters of the
template included in the visible part of @i<A>.]}

@end{StaticSem}

@begin{Examples}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[@i{Example of a generic
package with formal package parameters:}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[with] Ada.Containers.Ordered_Maps;  --@RI[ see @RefSecNum{The Package Containers.Ordered_Maps}]
@key[generic]
   @key[with package] Mapping_1 @key[is new] Ada.Containers.Ordered_Maps(<>);
   @key[with package] Mapping_2 @key[is new] Ada.Containers.Ordered_Maps
                                    (Key_Type => Mapping_1.Element_Type,
                                     @key[others] => <>);
@key[package] Ordered_Join @key[is]
   --@RI[ Provide a "join" between two mappings]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[subtype] Key_Type @key[is] Mapping_1.Key_Type;
   @key[subtype] Element_Type @key[is] Mapping_2.Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[function] Lookup(Key : Key_Type) @key[return] Element_Type;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   ...
@key[end] Ordered_Join;]}
@end{Example}

@RMNewPage@Comment{For printed Ada 2005 RM only}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
@ChgAdded{Version=[2],Type=[Leading],Keepnext=[T],Text=[@i{Example of
an instantiation of a package with formal packages:}]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[with] Ada.Containers.Ordered_Maps;
@key[package] Symbol_Package @key[is]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[type] String_Id @key[is] ...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[type] Symbol_Info @key[is] ...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[package] String_Table @key[is new] Ada.Containers.Ordered_Maps
           (Key_Type => String,
            Element_Type => String_Id);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[package] Symbol_Table @key[is new] Ada.Containers.Ordered_Maps
           (Key_Type => String_Id,
            Element_Type => Symbol_Info);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[package] String_Info @key[is new] Ordered_Join(Mapping_1 => String_Table,
                                           Mapping_2 => Symbol_Table);]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   Apple_Info : @key[constant] Symbol_Info := String_Info.Lookup("Apple");]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[end] Symbol_Package;]}

@end{Example}
@end{Examples}



@begin{Extend83}
  @Defn{extensions to Ada 83}
  Formal packages are new to Ada 95.
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00317-01],ARef=[AI95-00398-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  It's now allowed to mix actuals of a formal package that are specified
  with those that are not specified.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0039],ARef=[AI95-00213-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the description of
  formal package matching to say that formal parameters are always replaced by
  their actual parameters (recursively). This matches the actual practice of
  compilers, as the ACATS has always required this behavior.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00317-01]}
  @ChgAdded{Version=[2],Text=[The description of which operations are
  visible in a formal package has been clarified. We also specify how matching
  is done when the actual is a formal package.]}
@end{DiffWord95}


@LabeledClause{Example of a Generic Package}

@begin{Intro}
The following example provides a possible formulation of stacks by means
of a generic package.
The size of each stack and the type of the stack elements are provided
as generic formal parameters.
@end{Intro}

@begin{Examples}
@Leading
@ChgRef{Version=[1], Kind=[Deleted]}
@ChgDeleted[Version=[1],Text=<@ @;@comment{Empty paragraph to hang junk paragraph number from original RM}>]
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

@begin{Wide}
@Leading@Keepnext@;Instances of this generic package can be obtained as follows:
@end{Wide}
@begin{Example}
@key[package] Stack_Int  @key[is] @key[new] Stack(Size => 200, Item => Integer);
@key[package] Stack_Bool @key[is] @key[new] Stack(100, Boolean);
@end{Example}

@begin{Wide}
@Leading@Keepnext@;Thereafter, the procedures of the instantiated packages can be called as
follows:
@end{Wide}
@begin{Example}
Stack_Int.Push(N);
Stack_Bool.Push(True);
@end{Example}

@begin{Wide}
@Leading@Keepnext@;Alternatively, a generic formulation of the type Stack can be given as
follows (package body omitted):
@end{Wide}
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

@begin{Wide}
@Leading@Keepnext@;In order to use such a package, an instance has to be created and
thereafter stacks of the corresponding type can be declared:
@end{Wide}
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
