@Part(03, Root="ada.mss")

@Comment{$Date: 2012/04/03 20:37:01 $}
@LabeledSection{Declarations and Types}

@Comment{$Source: e:\\cvsroot/ARM/Source/03a.mss,v $}
@Comment{$Revision: 1.121 $}

@begin{Intro}
This section describes the types in the language and the rules
for declaring constants, variables, and named numbers.
@end{Intro}

@LabeledClause{Declarations}

@begin{Intro}
@PDefn{entity}
The language defines
several kinds of named @i(entities) that are declared
by declarations.
@PDefn{name}
The entity's @i(name)
is defined by the declaration, usually by a
@nt<defining_@!identifier>,
but sometimes by a @nt{defining_@!character_@!literal}
or @nt{defining_@!operator_@!symbol}.

There are several forms of declaration. A @nt<basic_declaration>
is a form of declaration defined as follows.
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00348-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0177-1]}
@Syn{tabs=[P27], lhs=<basic_declaration>,rhs="
     @Syn2{type_declaration}@\| @Syn2{subtype_declaration}
   | @Syn2{object_declaration}@\| @Syn2{number_declaration}
   | @Syn2{subprogram_declaration}@\| @Syn2{abstract_subprogram_declaration}
   | @Chg{Version=[2],New=[@Syn2{null_procedure_declaration}@\| @Chg{Version=<3>,New=<@Syn2{expression_function_declaration}
   | >,Old=<>}],Old=[]}@Syn2{package_declaration}@Chg{Version=[2],New=[@Chg{Version=[3],New=[@\|],Old=[
   |]}],Old=[@\|]} @Syn2{renaming_declaration}@Chg{Version=[2],New=[@Chg{Version=[3],New=[
   | ],Old=[@\|]}],Old=[
   | ]}@Syn2{exception_declaration}@Chg{Version=[2],New=[@Chg{Version=[3],New=[@\|],Old=[
   |]}],Old=[@\|]} @Syn2{generic_declaration}@Chg{Version=[2],New=[@Chg{Version=[3],New=[
   | ],Old=[@\|]}],Old=[
   | ]}@Syn2{generic_instantiation}"}

@Syn{lhs=<defining_identifier>,rhs="@Syn2{identifier}"}
@end{Syntax}

@begin{StaticSem}
@ToGlossaryAlso{Term=<Declaration>,
  Text=<A @i(declaration) is a language construct that associates a name
with (a view of) an entity.
@Defn(explicit declaration)
@Defn(implicit declaration)
A declaration may appear explicitly in the program
text (an @i(explicit) declaration), or may be supposed to
occur at a given place in the text as a
consequence of the semantics of another construct (an @i(implicit)
declaration).>}
@begin{Discussion}
  An implicit declaration generally declares
  a predefined or inherited operation associated with the definition
  of a type. This term is
  used primarily when allowing explicit declarations to override
  implicit declarations, as part of a type declaration.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0255-1],ARef=[AI05-0277-1]}
@Defn{declaration}
Each of the following is defined to be a declaration:
any @nt{basic_@!declaration};
an @nt{enumeration_@!literal_@!specification};
a @nt{discriminant_@!specification};
a @nt{component_@!declaration};
a @nt{loop_@!parameter_@!specification};@Chg{Version=[3],New=[
an @nt{iterator_@!specification};],Old=[]}
a @nt{parameter_@!specification};
a @nt{subprogram_@!body};@Chg{Version=[3],New=[
an @nt{extended_@!return_@!object_@!declaration};],Old=[]}
an @nt{entry_@!declaration};
an @nt{entry_@!index_@!specification};
a @nt{choice_@!parameter_@!specification};
a @nt{generic_@!formal_@!parameter_@!declaration}.@Chg{Version=[3],New=[],Old=[@Chg{Version=[2],New=[
In addition, an @nt{extended_return_statement} is a declaration of its
@nt{defining_identifier}.],Old=[]}]}
@begin(Discussion)
  This list (when @nt<basic_declaration> is expanded out)
  contains all syntactic categories that end in "_declaration"
  or "_specification", except for program unit _specifications.
  Moreover, it contains @nt{subprogram_body}.
  A @nt{subprogram_body} is a declaration,
  whether or not it completes a previous declaration.
  This is a bit strange, @nt{subprogram_body} is not part of the syntax
  of @nt{basic_declaration} or @nt{library_unit_declaration}.
  A renaming-as-body is considered a declaration.
  An @nt{accept_statement} is not considered a declaration.
  Completions are sometimes declarations, and sometimes not.
@end(Discussion)

@Defn(view)@Defn(definition)
All declarations contain a @i(definition) for a @i(view) of an entity.
A view consists of an identification of the entity
(the entity @i(of) the view),
plus view-specific characteristics that affect the use
of the entity through that view (such as mode of access to an object,
formal parameter names and defaults for a subprogram, or visibility to
components of a type).
In most cases, a declaration also contains the definition for the
entity itself (a @nt(renaming_declaration) is an example of a declaration
that does not define a new entity,
but instead defines a view of an existing entity
(see @RefSecNum(Renaming Declarations))).

@ChgToGlossary{Version=[2],Kind=[Revised],Term=<View>,
Text=<@Chg{Version=[2],New=[A view of an entity reveals some or all of the
properties of the entity. A single entity may have multiple views.],
Old=[(See @b[Definition].)]}>}
@begin{Discussion}
  Most declarations define a view (of some entity) whose
  view-specific characteristics are unchanging for the
  life of the view. However, subtypes are somewhat unusual
  in that they inherit characteristics from whatever view
  of their type is currently visible. Hence, a subtype is not a
  @i(view) of a type; it is more of an indirect reference.
  By contrast, a private type provides a single, unchanging (partial)
  view of its full type.
@end{Discussion}

@ChgToGlossary{Version=[2],Kind=[Deleted],Term=<Definition>,
  Text=<@ChgDeleted{Version=[2],Text=[@Defn(view)
All declarations contain a @i(definition) for a @i(view) of an entity.
A view consists of an identification of the entity
(the entity @i(of) the view),
plus view-specific characteristics that affect the use
of the entity through that view (such as mode of access to an object,
formal parameter names and defaults for a subprogram, or visibility to
components of a type).
In most cases, a declaration also contains the definition for the
entity itself (a @nt(renaming_declaration) is an example of a declaration
that does not define a new entity,
but instead defines a view of an existing entity
(see @RefSecNum(Renaming Declarations))).]}>}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0080-1]}
@ChgAdded{Version=[3],Text=[When it is clear from context, the term @i<object>
is used in place of @i<view of an object>. Similarly, the terms @i<type> and
@i<subtype> are used in place of @i<view of a type> and @i<view of a subtype>,
respectively.@Defn2{Term=[view],Sec=[of an object (implied)]}@Defn2{Term=[view],
Sec=[of a type (implied)]}@Defn2{Term=[view],Sec=[of a subtype (implied)]}]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Rules interpreted at compile time generally
  refer to views of entities, rather than the entities themselves. This is
  necessary to preserve privacy; characteristics that are not visible should
  not be used in compile-time rules. Thus, @StaticSemTitle and
  @LegalityTitle generally implicitly have @ldquote@;view of@rdquote.
  @LegalityTitle that need to look into the private part
  are the exception to this interpretation.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[On the other hand, run-time rules can work
  either way, so @ldquote@;view of@rdquote should not be assumed
  in @RunTimeTitle rules.]}
@end{Discussion}

@PDefn2{Term=[scope], Sec=(informal definition)}
For each declaration, the language rules define a certain
region of text called the @i(scope) of the declaration
(see @RefSecNum(Scope of Declarations)). Most declarations
associate an @nt<identifier>
with a declared entity. Within its scope,
and only there, there are places where it is possible to use the
@nt<identifier> to refer to the declaration, the view it defines,
and the associated entity; these places are defined by
the visibility rules (see @RefSecNum(Visibility)).
@Defn2{Term=[name], Sec={of (a view of) an entity}}
At such places
the @nt<identifier> is said to be a @i(name) of the entity (the
@nt<direct_name> or @nt<selector_name>);
@PDefn2{Term=[denote], Sec={informal definition}}
the name is said to @i(denote) the declaration,
the view, and the associated entity
(see @RefSecNum{The Context of Overload Resolution}).
@Defn{declare}
The declaration is said
to @i(declare) the name, the view, and in most cases, the
entity itself.

As an alternative to an @nt<identifier>,
an enumeration literal can be declared with a @nt<character_literal>
as its name (see @RefSecNum(Enumeration Types)),
and a function can be declared with an @nt<operator_symbol>
as its name (see @RefSecNum(Subprogram Declarations)).

@Defn{defining name}
The syntax rules use the terms @nt<defining_identifier>,
@nt<defining_@!character_@!literal>, and @nt<defining_@!operator_@!symbol>
for the defining occurrence of a name; these are collectively
called @i(defining names).
@Defn{usage name}
The terms @nt<direct_name> and
@nt<selector_name> are used for usage occurrences of @nt<identifier>s,
@nt<character_literal>s, and @nt<operator_symbol>s. These
are collectively called @i(usage names).
@begin(Honest)
The terms @nt<identifier>,
@nt<character_literal>, and @nt<operator_symbol> are used directly
in contexts where the normal visibility rules do not
apply (such as the @nt<identifier> that appears after the @key(end) of
a @nt<task_body>). Analogous conventions apply to the use of @nt<designator>,
which is the collective term for @nt<identifier> and @nt<operator_symbol>.
@end(Honest)

@end{StaticSem}

@begin{RunTime}
@RootDefn{execution}
The process by which a construct achieves its run-time effect is
called @i(execution).
@RootDefn{elaboration}
@RootDefn{evaluation}
This process is also called @i(elaboration) for declarations
and @i(evaluation) for expressions.
One of the terms execution, elaboration, or evaluation is defined
by this International Standard for each construct that has a run-time effect.
@ToGlossary{Term=<Execution>,
  Text=<The process by which a construct achieves its run-time effect is
  called @i(execution).
  @Defn(elaboration)
  @Defn(evaluation)
  Execution of a declaration is also called @i(elaboration).
  Execution of an expression is also called @i(evaluation).>}
@begin{Honest}
The term elaboration is also used for the execution of certain
constructs that are not declarations,
and the term evaluation is used for the execution of certain
constructs that are not expressions.
For example, @nt{subtype_indication}s are elaborated,
and @nt{range}s are evaluated.

For bodies, execution and elaboration are both explicitly defined.
When we refer specifically to the execution of a body,
we mean the explicit definition of execution for that kind of body,
not its elaboration.
@end{Honest}
@begin(Discussion)
Technically, "the execution of a declaration" and "the
elaboration of a declaration" are synonymous.
We use the term "elaboration" of a construct when we know the
construct is elaborable.
When we are talking about more arbitrary constructs,
we use the term "execution".
For example, we use the term "erroneous execution",
to refer to any erroneous execution, including
erroneous elaboration or evaluation.

When we explicitly define evaluation or elaboration for a construct,
we are implicitly defining execution of that construct.

We also use the term "execution" for things like @nt{statement}s,
which are executable, but neither elaborable nor evaluable.
We considered using the term "execution" only for nonelaborable,
nonevaluable constructs, and defining the term "action" to mean what
we have defined "execution" to mean.
We rejected this idea because we thought three terms that mean the
same thing was enough @em four would be overkill.
Thus, the term "action" is used only informally in the standard
(except where it is defined as part of a larger term,
such as "protected action").
@end(Discussion)
@ChgToGlossary{Version=[2],Kind=[Added],Term=<Elaboration>,
Text=<@ChgAdded{Version=[2],Text=[The process by which a declaration achieves
its run-time effect is called elaboration. Elaboration is one of the forms of
execution.]}>}
@ChgToGlossary{Version=[2],Kind=[Added],Term=<Evaluation>,
Text=<@ChgAdded{Version=[2],Text=[The process by which an expression achieves
its run-time effect is called evaluation. Evaluation is one of the forms of
execution.]}>}
@begin{Honest}
@Defn{elaborable}
A construct is @i(elaborable) if elaboration is defined for it.
@Defn{evaluable}
A construct is @i(evaluable) if evaluation is defined for it.
@Defn{executable}
A construct is @i(executable) if execution is defined for it.
@end{Honest}
@begin(Discussion)
  Don't confuse @lquotes@;elaborable@rquotes@; with @lquotes@;preelaborable@rquotes@; (defined
  in @RefSecNum(Elaboration Control)).

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  Evaluation of an evaluable construct produces a result that is
  either a value, a denotation, or a range.
  The following are evaluable:
  expression; @nt{name}
  @nt{prefix}; @nt{range};
  @Chg{Version=[2],New=[@nt{entry_index_specification}],Old=[@ntf{entry_list_iterator}]};
  and possibly @nt{discrete_range}.
  The last one is curious @em RM83 uses the term @lquotes@;evaluation of a
  @nt{discrete_range},@rquotes@; but never defines it.
  One might presume that the evaluation of a @nt{discrete_range}
  consists of the evaluation of the @nt{range} or the
  @nt{subtype_indication}, depending on what it is.
  But @nt{subtype_indication}s are not evaluated; they are elaborated.

  Intuitively, an @i(executable) construct is one that has
  a defined run-time effect (which may be null). Since execution
  includes elaboration and evaluation as special cases, all
  elaborable and all evaluable constructs
  are also executable. Hence, most constructs in Ada are executable.
  An important exception is that the constructs inside a generic unit are
  not executable directly, but rather are used as a template for
  (generally) executable constructs in instances of the generic.
@end(Discussion)
@end{RunTime}

@begin{Notes}
@Defn{declare}
At compile time, the declaration of an entity @i(declares) the entity.
@Defn{create}
At run time, the elaboration of the declaration @i(creates) the entity.
@begin{Ramification}
Syntactic categories for declarations are named either
@i(entity_)@ntf<declaration> (if they include a trailing semicolon)
or @i(entity_)@ntf<specification> (if not).

@Defn{entity}
The various kinds of named entities that can be declared are as
follows: an object (including components and parameters), a named number,
a type (the name always refers to its first subtype), a subtype,
a subprogram (including enumeration literals and operators), a single entry,
an entry family, a package, a protected or task unit (which corresponds
to either a type or a single object), an exception,
a generic unit, a label,
and the name of a statement.

Identifiers are also associated with names of pragmas, arguments to
pragmas, and with attributes, but these are not user-definable.
@end{Ramification}
@end{Notes}

@begin{DiffWord83}
The syntax rule for @nt{defining_identifier} is new.
It is used for the defining occurrence of an @nt{identifier}.
Usage occurrences use the @nt{direct_name} or @nt{selector_name}
syntactic categories.
@Leading@;Each occurrence of an @nt{identifier} (or @ntf{simple_name}), @nt{character_literal}, or
@nt{operator_symbol} in the Ada 83 syntax rules is handled as follows in Ada
95:
@begin{itemize}
It becomes a @nt{defining_identifier}, @nt{defining_character_literal}, or
@nt{defining_operator_symbol} (or some syntactic category composed of these),
to indicate a defining occurrence;

It becomes a @nt{direct_name}, in usage occurrences where
the usage is required (in Section 8) to be directly visible;

It becomes a @nt{selector_name}, in usage occurrences
where the usage is required (in
Section 8) to be visible but not
necessarily directly visible;

It remains an @nt{identifier}, @nt{character_literal}, or @nt{operator_symbol},
in cases where the visibility rules do not apply
(such as the @nt{designator} that appears after the
@key{end} of a @nt{subprogram_body}).
@end{itemize}

For declarations that come in @lquotes@;two parts@rquotes@;
(program unit declaration plus body, private or
incomplete type plus full type, deferred constant plus full constant),
we consider both to be defining occurrences.
Thus, for example, the syntax for @nt{package_body} uses
@nt{defining_identifier} after the reserved word @key{body},
as opposed to @nt{direct_name}.

The defining occurrence of a statement name is in
its implicit declaration, not where it appears in the program text.
Considering the statement name itself to be the defining occurrence would
complicate the visibility rules.

The phrase @lquotes@;visible by selection@rquotes@;
is not used in Ada 95. It is subsumed by simply @lquotes@;visible@rquotes@; and
the Name Resolution Rules for @nt<selector_name>s.

(Note that in Ada 95, a declaration is visible at all
places where one could have used a @nt{selector_name},
not just at places where a @nt{selector_name} was actually used.
Thus, the places where a declaration is directly visible are a
subset of the places where it is visible.
See Section 8 for details.)

We use the term @lquotes@;declaration@rquotes@; to cover @ntf<_specification>s that declare
(views of) objects, such as @nt<parameter_specification>s. In Ada 83,
these are referred to as a @lquotes@;form of declaration,@rquotes@; but it is not
entirely clear that they are considered simply @lquotes@;declarations.@rquotes@;

RM83 contains an incomplete definition of "elaborated" in this clause:
it defines "elaborated" for declarations,
@nt{declarative_part}s, @nt{declarative_item}s
and @nt{compilation_unit}s,
but "elaboration" is defined elsewhere for various other constructs.
To make matters worse, Ada 95 has a different set of elaborable
constructs.
Instead of correcting the list, it is more maintainable to
refer to the term "elaborable," which is defined in a distributed
manner.

RM83 uses the term @lquotes@;has no other effect@rquotes@; to describe an elaboration that
doesn't do anything except change the state from not-yet-elaborated to
elaborated. This was a confusing wording, because the answer to @lquotes@;other than
what?@rquotes@; was to be found many pages away.
In Ada 95, we change this wording to @lquotes@;has no effect@rquotes@; (for things that truly
do nothing at run time), and @lquotes@;has no effect other than to establish that
so-and-so can happen without failing the
Elaboration_Check@rquotes@; (for things where it matters).

We make it clearer that the term "execution" covers elaboration and
evaluation as special cases.
This was implied in RM83.
For example, "erroneous execution" can include any execution,
and RM83-9.4(3) has,
"The task designated by any other task object depends on the master
whose execution creates the task object;"
the elaboration of the master's @nt{declarative_part} is doing
the task creation.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
  @ChgAdded{Version=[2],Text=[Added @nt{extended_return_statement} to
  the list of declarations.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00348-01]}
  @ChgAdded{Version=[2],Text=[Added null procedures
  (see @RefSecNum{Null Procedures}) to the syntax.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0177-1]}
  @ChgAdded{Version=[3],Text=[Added expression functions
  (see @RefSecNum{Expression Functions}) to the syntax.]}
@end{DiffWord2005}



@LabeledClause{Types and Subtypes}

@begin{StaticSem}
@Defn{type}
@PDefn{primitive operation}
A @i(type) is characterized by a set of values,
and a set of @i(primitive operations)
which implement the fundamental aspects of its semantics.
@PDefn{object}
An @i(object) of a given type is a run-time entity that contains (has)
a value of the type.
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Type>,
  Text=<Each object has a type.
  A @i(type) has an associated set of values, and a set of @i(primitive
  operations) which implement the fundamental aspects of its semantics.
  Types are grouped into @Chg{Version=[2],New=[@i(categories)],
  Old=[@i(classes)]}.
  @Chg{Version=[2],New=[Most language-defined
  categories of types are also @i<classes> of types],Old=[The types of a
  given class share a set of primitive operations.
  @Defn(closed under derivation) Classes are closed under derivation;
  that is, if a type is in a class, then all of its derivatives
  are in that class]}.>}
@ChgToGlossary{Version=[3],Kind=[Revised],Term=<Subtype>,
  Text=<A subtype is a type together with @Chg{Version=[3],New=[optional
  constraints, null exclusions, and predicates],
  Old=[a constraint@Chg{Version=[2],New=[ or null exclusion],Old=[]}]},
  which @Chg{Version=[3],New=[constrain],Old=[constrains]}
  the values of the subtype to satisfy @Chg{Version=[3],New=[],Old=[a ]}certain
  @Chg{Version=[3],New=[conditions],Old=[condition]}. The values of a subtype
  are a subset of the values of its type.>}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
@Chg{Version=[2],New=[@Defn2{Term=[category], Sec=(of types)}],
Old=[]}@Defn2{Term=[class], Sec=(of types)}
Types are grouped into @Chg{Version=[2],New=[@i(categories)],
Old=[@i(classes)]} of types@Chg{Version=[2],New=[],Old=[, reflecting the
similarity of their values and primitive operations]}.
@Defn2{Term=[language-defined class], Sec=(of types)}
There exist several @i(language-defined @Chg{Version=[2],New=[categories],
Old=[classes]}) of types (see NOTES below)@Chg{Version=[2],New=[, reflecting
the similarity of their values and primitive operations],Old=[]}.@Chg{Version=[2],
New=[@Defn2{Term=[language-defined category], Sec=(of types)} @Redundant[Most
categories of types form @i(classes) of types.]],Old=[]}
@Defn{elementary type}
@i(Elementary) types are those whose values are logically indivisible;
@Defn{composite type}
@Defn{component}
@i(composite) types are those whose values are composed
of @i(component) values.
@IndexSeeAlso{Term={aggregate},See=(composite type)}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[The formal definition of @i<category> and
  @i<class> is found in @RefSecNum{Derived Types and Classes}.]}
@end{TheProof}
@ChgNote<The following ought to have the Term be:
Class@Chg{Version=[2],New=[ (of types)],Old=[]}
but that doesn't index properly and it is too much work to fix. See if
anyone complains.>
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Class (of types)>,
  Text=<@Defn(closed under derivation)
  A class is a set of types that is
  closed under derivation,
  which means that if a given type is in the class,
  then all types derived from that type are also in the class.
  The set of types of a class share common properties,
  such as their primitive operations.>}
@ChgToGlossary{Version=[2],Kind=[Added],Term=<Category (of types)>,
  Text=<@ChgAdded{Version=[2],Text=[A category of types is a set of types with
  one or more common properties, such as primitive operations. A
  category of types that is closed under derivation is also known as
  a @i<class>.]}>}
@ToGlossary{Term=<Elementary type>,
  Text=<An elementary type does not have components.>}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Composite type>,
  Text=<A composite type @Chg{Version=[2],New=[may have],Old=[has]} components.>}
@ToGlossary{Term=<Scalar type>,
  Text=<A scalar type is either a discrete type or a real type.>}
@ToGlossary{Term=<Access type>,
  Text=<An access type has values that designate aliased
  objects.
  Access types correspond to @lquotes@;pointer types@rquotes@;
  or @lquotes@;reference types@rquotes@; in some other languages.>}
@ToGlossary{Term=<Discrete type>,
  Text=<A discrete type is either an integer type or an enumeration
  type.
  Discrete types may be used, for example, in @nt(case_statement)s
  and as array indices.>}
@ToGlossary{Term=<Real type>,
  Text=<A real type has values that are approximations
  of the real numbers.
  Floating point and fixed point types are real types.>}
@ToGlossary{Term=<Integer type>,
  Text=<Integer types comprise the signed integer types
  and the modular types.
  A signed integer type has a base range that includes both
  positive and negative numbers,
  and has operations that may raise an exception when the result
  is outside the base range.
  A modular type has a base range whose lower bound is zero,
  and has operations with @lquotes@;wraparound@rquotes@; semantics.
  Modular types subsume what are called @lquotes@;unsigned types@rquotes@;
  in some other languages.>}
@ToGlossary{Term=<Enumeration type>,
  Text=<An enumeration type is defined by an enumeration of its values,
  which may be named by identifiers or character
  literals.>}
@ToGlossary{Term=<Character type>,
  Text=<A character type is an enumeration type whose values
  include characters.>}
@ToGlossary{Term=<Record type>,
  Text=<A record type is a composite type consisting of zero or more
  named components, possibly of different types.>}
@ToGlossary{Term=<Record extension>,
  Text=<A record extension is a type that extends another type by adding
  additional components.>}
@ToGlossary{Term=<Array type>,
  Text=<An array type is a composite type whose components are all of
  the same type. Components are selected by indexing.>}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Task type>,
  Text=<A task type is a composite type @Chg{Version=[2],New=[used to
  represent], Old=[whose values are tasks, which are]} active entities
  @Chg{Version=[2],New=[which],Old=[that may]} execute concurrently
  @Chg{Version=[2],New=[and which can communicate via queued task entries],
  Old=[with other tasks]}.
  The top-level task of a partition is called the environment task.>}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Protected type>,
  Text=<A protected type is a composite type whose components are
  @Chg{Version=[2],New=[accessible only
  through one of its protected operations which synchronize],
  Old=[protected from]} concurrent access by multiple tasks.>}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Private type>,
  Text=<A private type @Chg{Version=[2],New=[gives a],Old=[is a partial]} view
    of a type @Chg{Version=[2],New=[that reveals only some of its properties.
    The remaining properties are provided by the],Old=[whose]} full view
    @Chg{Version=[2],New=[given elsewhere. Private types can be used for
    defining abstractions that hide unnecessary details],Old=[is hidden]} from
    @Chg{Version=[2],New=[their],Old=[its]} clients.>}@ChgNote{This was changed
    to parallel "incomplete type"}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Private extension>,
  Text=<A private extension is @Chg{Version=[2],New=[a type that extends
  another type, with the additional properties],Old=[like a record extension,
  except that the components of the extension part
  are]} hidden from its clients.>}

@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<Incomplete type>,
  Text=<@ChgAdded{Version=[2],Text=[An incomplete type gives a view of a type
  that reveals only some of its properties. The remaining properties are
  provided by the full view given elsewhere. Incomplete types can be used
  for defining recursive data structures.]}>}

@Defn{scalar type}
The elementary types are the @i(scalar) types (@i(discrete) and @i(real))
and the @i(access) types (whose values provide access to objects or
subprograms).
@Defn{discrete type}
@Defn{enumeration type}
Discrete types are either @i(integer) types or are defined by enumeration
of their values (@i(enumeration) types).
@Defn{real type}
Real types are either @i(floating point) types
or @i(fixed point) types.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00326-01]}
The composite types are the @i(record) types, @i(record extensions),
@i(array) types, @Chg{Version=[2],New=[@i(interface) types, ],Old=[]}@i(task)
types, and @i(protected) types.@Chg{Version=[2], New=[],
Old=[@Defn{private type}
@Defn{private extension}
A @i(private) type or @i(private extension) represents a partial view
(see @RefSecNum{Private Types and Private Extensions})
of a type, providing support for data abstraction.
A partial view is a composite type.]}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00442-01]}
  @ChgDeleted{Version=[2],Text=[The set of all record types do not form a
  class (because tagged record types can have private extensions), though
  the set of untagged record types do.
  In any case, what record types had in common in Ada 83 (component selection)
  is now a property of the composite class, since all composite types
  (other than array types) can have discriminants.
  Similarly, the set of all private types do not form a class (because
  tagged private types can have record extensions), though
  the set of untagged private types do.
  Nevertheless, the set of untagged private types is not particularly
  @lquotes@;interesting@rquotes@; @em more interesting is the set of
  all nonlimited
  types, since that is what a generic formal (nonlimited) private
  type matches.]}
@end{Honest}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00326-01]}
@ChgAdded{Version=[2],Text=[@Defn{incomplete type}
@Defn{private type}
@Defn{private extension}
There can be multiple views of a type with varying
sets of operations. @Redundant[An @i(incomplete) type represents an incomplete
view (see @RefSecNum{Incomplete Type Declarations}) of a type with a
very restricted usage, providing support for recursive data structures. A
@i(private) type or @i(private extension) represents a partial view
(see @RefSecNum{Private Types and Private Extensions}) of a
type, providing support for data abstraction. The full view
(see @RefSecNum{Type Declarations}) of a
type represents its complete definition.] An incomplete or partial view is
considered a composite type@Redundant[, even if the full view is not].]}
@begin{TheProof}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The real definitions of the views are in the
referenced clauses.]}
@end{TheProof}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00326-01]}
@Defn{discriminant}
Certain composite types (and@Chg{Version=[2],New=[],Old=[ partial]} views
thereof) have special
components called @i(discriminants) whose values affect the
presence, constraints, or initialization of other components.
Discriminants can be thought of as parameters of the type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00366-01]}
@Defn{subcomponent}
The term @i(subcomponent) is used
in this International Standard in place of the term component
to indicate either a component, or a component of another
subcomponent. Where other subcomponents
are excluded, the term component is used instead.
@Defn2{Term=[part], Sec=(of an object or value)}
Similarly, a @i(part) of an object or value is used to mean
the whole object or value, or any set of its subcomponents.@Chg{Version=[2],
New=[ The terms component, subcomponent, and part are also applied to a type
meaning the component, subcomponent, or part of objects and values of the
type.@Defn2{Term=[part], Sec=(of a type)}@Defn2{Term=[component], Sec=(of a type)}],Old=[]}
@begin{Discussion}
  The definition of @lquotes@;part@rquotes@; here is designed to simplify rules
  elsewhere. By design, the intuitive meaning of
  @lquotes@;part@rquotes@; will convey the correct result to the casual reader,
  while this formalistic definition will answer the concern of
  the compiler-writer.

  We use the term @lquotes@;part@rquotes@; when talking about the parent part,
  ancestor part, or extension part of a type extension.
  In contexts such as these, the part might represent an empty
  set of subcomponents (e.g. in a null record extension, or a
  nonnull extension of a null record).
  We also use @lquotes@;part@rquotes@; when specifying rules such as
  those that apply to an object
  with a @lquotes@;controlled part@rquotes@; meaning that it applies if the
  object as a whole is controlled, or any subcomponent is.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
@PDefn{constraint}
The set of possible values for an object of a given type can be
subjected to a condition that is called a @i(constraint)
@Defn{null constraint}
(the case
of a @i(null constraint) that specifies no restriction is also
included)@Redundant[;
the rules for which values satisfy a given kind of constraint
are given in @RefSecNum(Scalar Types) for @nt<range_constraint>s,
@RefSecNum(Index Constraints and Discrete Ranges)
for @nt<index_constraint>s, and
@RefSecNum(Discriminant Constraints) for
@nt<discriminant_constraint>s].@Chg{Version=[2],New=[ The set of possible values
for an object of an access type can also be subjected to a condition that
excludes the null value (see @RefSecNum{Access Types}).],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01],ARef=[AI95-00415-01]}
@Defn{subtype}
A @i(subtype) of a given type is a combination of the type,
a constraint on values of the type, and certain
attributes specific to the subtype.
The given type is called the @Chg{Version=[2],New=[@i(type of the subtype)],
Old=[type @i(of) the subtype]}.@Chg{Version=[2],New=[@Defn2{Term=[type], Sec=(of a subtype)}
@Defn2{Term=[subtype], Sec=(type of)}],Old=[]}
Similarly, the associated constraint is
called the @Chg{Version=[2],New=[@i(constraint of the subtype)],
Old=[constraint @i(of) the subtype]}.@Chg{Version=[2],New=[@Defn2{Term=[constraint], Sec=(of a subtype)}
@Defn2{Term=[subtype], Sec=(constraint of)}],Old=[]} The set of
values of a subtype consists of the values of its type
that satisfy its constraint@Chg{Version=[2],New=[ and any exclusion of the
null value],
Old=[]}.
@Defn2{Term=[belong], Sec=(to a subtype)}
Such values @i(belong) to the subtype.@Chg{Version=[2],New=[@Defn2{Term=[values], Sec=(belonging to a subtype)}
@Defn2{Term=[subtype], Sec=(values belonging to)}],Old=[]}
@begin{Discussion}
  We make a strong distinction between a type and its
  subtypes.
  In particular, a type is @i(not) a subtype of itself.
  There is no constraint associated with a type (not even a null one),
  and type-related attributes are distinct from subtype-specific attributes.
@end{Discussion}
@begin{Discussion}
  We no longer use the term "base type."
  All types were "base types" anyway in Ada 83, so the term was redundant,
  and occasionally confusing. In the RM95 we
  say simply "the type @i(of) the subtype" instead of "the base type
  of the subtype."
@end{Discussion}
@begin{Ramification}
  The value subset for a subtype might be empty, and need
  not be a proper subset.
@end{Ramification}
@begin{Honest}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
Any name of a @Chg{Version=[2],New=[category],Old=[class]} of types (such as
@lquotes@;discrete@rquotes@;@Chg{Version=[2],New=[, ],Old=[ or]}
@lquotes@;real@rquotes@;@Chg{Version=[2],New=[, or],Old=[), or other category of
types (such as]} @lquotes@;limited@rquotes@;@Chg{Version=[2],New=[],
Old=[ or @lquotes@;incomplete@rquotes@;]})
is also used to qualify its subtypes, as well as its objects, values,
declarations, and definitions, such as an @lquotes@;integer type
declaration@rquotes@; or an @lquotes@;integer value.@rquotes@; In addition, if
a term such as @lquotes@;parent subtype@rquotes@; or @lquotes@;index
subtype@rquotes@; is defined, then the corresponding term for the type of the
subtype is @lquotes@;parent type@rquotes@; or @lquotes@;index type.@rquotes@;
@end{Honest}
@begin{Discussion}
  We use these corresponding terms without explicitly defining them,
  when the meaning is obvious.
@end{Discussion}

@Defn{constrained}
@Defn{unconstrained}
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
A subtype is called an @i(unconstrained) subtype if its
type has unknown discriminants,
or if its type allows range, index, or discriminant constraints,
but the subtype does not impose such a constraint;
otherwise, the subtype is called a @i(constrained) subtype
(since it has no unconstrained characteristics).
@begin{Discussion}
  In an earlier version of Ada 9X,
  "constrained" meant "has a nonnull
  constraint." However, we changed to this definition
  since we kept having to special
  case composite non-array/nondiscriminated types. It also corresponds
  better to the (now obsolescent) attribute 'Constrained.

  For scalar types, @lquotes@;constrained@rquotes@; means @lquotes@;has a nonnull
  constraint@rquotes@;.
  For composite types, in implementation terms, @lquotes@;constrained@rquotes@; means
  that the size of all objects of the subtype is the same, assuming a
  typical implementation model.

  Class-wide subtypes are always unconstrained.
@end{Discussion}
@end{StaticSem}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
@Chg{Version=[2],New=[Any set of types can be called a
@lquotes@;category@rquotes@; of types, and any],Old=[Any]}
set of types that is closed under derivation
(see @RefSecNum(Derived Types and Classes)) can be called
a @lquotes@;class@rquotes@; of types. However, only certain
@Chg{Version=[2],New=[categories and ],Old=[]}classes are used in the
description of the rules of the language @em generally those
that have their own particular set of primitive operations
(see @RefSecNum(Classification of Operations)), or that
correspond to a set of types that are matched by a given
kind of generic formal type (see @RefSecNum(Formal Types)).
@PDefn{language-defined class}
The following are examples of @lquotes@;interesting@rquotes@; @i(language-defined classes):
elementary, scalar, discrete, enumeration, character, boolean,
integer, signed integer, modular, real, floating point,
fixed point, ordinary fixed point, decimal fixed point,
numeric, access, access-to-object, access-to-subprogram,
composite, array, string, (untagged) record, tagged, task, protected,
nonlimited. Special syntax is provided to define types in
each of these classes.@Chg{Version=[2],New=[ In addition to these
classes, the following are examples of @lquotes@;interesting@rquotes@;
@i(language-defined categories): @PDefn{language-defined categories}
abstract, incomplete, interface, limited, private, record.],Old=[]}
@begin{Discussion}
@Defn{value}
A @i(value) is a run-time entity with a given type which can be
assigned to an object of an appropriate subtype of the type.
@Defn{operation}
An @i(operation) is a program entity that operates on zero or more
operands to produce an effect, or yield a result, or both.
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
  Note that a type's @Chg{Version=[2],New=[category (and ],Old=[]}class@Chg{Version=[2],New=[)],Old=[]}
  depends on the place of the reference @em a
  private type is composite outside and possibly elementary inside.
  It's really the @i{view} that is elementary or composite.
  Note that although private types are composite,
  there are some properties that
  depend on the corresponding full view @em for example,
  parameter passing modes, and
  the constraint checks that apply in various places.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01],ARef=[AI95-00442-01]}
  @Chg{Version=[2],New=[Every property of types forms a category, but not],
  Old=[Not]} every property of types represents a class.
  For example, the set of all abstract types does not form a class,
  because this set is not closed under derivation.@Chg{Version=[2],New=[
  Similarly, the set of all interface types does not form a class.],Old=[]}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
  The set of limited types @Chg{Version=[2],New=[does not form a class
  (since nonlimited types can inherit from limited interfaces), but the
  set of nonlimited types does. The set of tagged record types and the set
  of tagged private types do not form a class (because each of them can
  be extended to create a type of the other category); that implies that
  the set of record types and the set of private types also do not form a
  class (even though untagged record types and untagged private types do
  form a class). In all of these cases, we can talk about the category
  of the type; for instance, we can talk about the @lquotes@;category
  of limited types@rquotes.],
  Old=[forms a class in the sense that it is closed
  under derivation, but the more interesting
  class, from the point of generic formal type matching, is the
  set of all types, limited and nonlimited, since that is what
  matches a generic formal @lquotes@;limited@rquotes@; private type.
  Note also that a limited type can @lquotes@;become nonlimited@rquotes@; under
  certain circumstances, which
  makes @lquotes@;limited@rquotes@; somewhat problematic as a class of types]}.

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[Normatively, the @i<language-defined classes>
  are those that are defined to be inherited on derivation by
  @RefSecNum{Derived Types and Classes}; other properties either aren't
  interesting or form categories, not classes.]}
@end{Ramification}

@noprefix@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00442-01]}
These language-defined @Chg{Version=[2],New=[categories],Old=[classes]} are
organized like this:
@begin{Display}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01]}
@TabClear{}
@TabSet{3, 6, 9, 12, 15, 18, 21}
all types
@\elementary
@\@\scalar
@\@\@\discrete
@\@\@\@\enumeration
@\@\@\@\@\character
@\@\@\@\@\boolean
@\@\@\@\@\other enumeration
@\@\@\@\integer
@\@\@\@\@\signed integer
@\@\@\@\@\modular integer
@\@\@\real
@\@\@\@\floating point
@\@\@\@\fixed point
@\@\@\@\@\ordinary fixed point
@\@\@\@\@\decimal fixed point
@\@\access
@\@\@\access-to-object
@\@\@\access-to-subprogram
@\composite@Chg{Version=[2],New=[
@\@\untagged],Old=[]}
@Chg{Version=[2],New=[@\],Old=[]}@\@\array
@Chg{Version=[2],New=[@\],Old=[]}@\@\@\string
@Chg{Version=[2],New=[@\],Old=[]}@\@\@\other array
@\@\@Chg{Version=[2],New=[@\],Old=[untagged ]}record@Chg{Version=[2],New=[],Old=[
@\@\tagged]}
@Chg{Version=[2],New=[@\],Old=[]}@\@\task
@Chg{Version=[2],New=[@\],Old=[]}@\@\protected@Chg{Version=[2],New=[
@\@\tagged (including interfaces)
@\@\@\nonlimited tagged record
@\@\@\limited tagged
@\@\@\@\limited tagged record
@\@\@\@\synchronized tagged
@\@\@\@\@\tagged task
@\@\@\@\@\tagged protected],Old=[]}
@end{Display}

@noprefix@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00345-01],ARef=[AI95-00442-01]}
@Chg{Version=[2],New=[There are other categories, such as],Old=[The classes]}
@lquotes@;numeric@rquotes@; and
@lquotes@;@Chg{Version=[2],New=[discriminated],Old=[nonlimited]}@rquotes@;@Chg{Version=[2],
New=[, which],Old=[]}
represent other @Chg{Version=[2],New=[categorization],Old=[classification]}
dimensions@Chg{Version=[2],New=[, but],
Old=[ and]} do not fit into the above strictly hierarchical picture.
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[Note that this is also true for some categories
  mentioned in the chart. The category @lquotes@;task@rquotes@; includes both
  untagged tasks and tagged tasks. Similarly for @lquotes@;protected@rquotes@;,
  @lquotes@;limited@rquotes@;, and @lquotes@;nonlimited@rquotes@; (note that
  limited and nonlimited are not shown for untagged composite types).]}
@end{Discussion}
@end{Notes}

@begin{DiffWord83}
This clause and its subclauses now precede the clause and
subclauses on objects and named numbers, to cut down on the number of
forward references.

We have dropped the term "base type" in favor of simply "type" (all
types in Ada 83 were "base types" so it wasn't clear when it was
appropriate/necessary to say "base type"). Given a subtype S of
a type T, we call T the "type of the subtype S."
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[Added a mention of null exclusions when we're
  talking about constraints (these are not constraints, but they are similar).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[Defined an interface type to be a composite
  type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[Revised the wording so that it is clear
  that an incomplete view is similar to a partial view in terms of the
  language.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[Added a definition of component of a type,
  subcomponent of a type, and part of a type. These are commonly used in
  the standard, but they were not previously defined.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[Reworded most of this clause to use
  category rather than class, since so many interesting properties are
  not, strictly speaking, classes. Moreover, there was no normative
  description of exactly which properties formed classes, and which did
  not. The real definition of class, along with a list of properties, is now
  in @RefSecNum{Derived Types and Classes}.]}
@end{DiffWord95}


@LabeledSubClause{Type Declarations}

@begin{Intro}
A @nt<type_declaration> declares a type and its first subtype.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<type_declaration>,rhs=" @Syn2{full_type_declaration}
   | @Syn2{incomplete_type_declaration}
   | @Syn2{private_type_declaration}
   | @Syn2{private_extension_declaration}"}


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<full_type_declaration>,rhs="
     @key{type} @Syn2{defining_identifier} [@Syn2{known_discriminant_part}] @key{is} @Syn2{type_definition}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};
   | @Syn2{task_type_declaration}
   | @Syn2{protected_type_declaration}"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
@Syn{tabs=[P29], lhs=<type_definition>,rhs="
     @Syn2{enumeration_type_definition}@\| @Syn2{integer_type_definition}
   | @Syn2{real_type_definition}@\| @Syn2{array_type_definition}
   | @Syn2{record_type_definition}@\| @Syn2{access_type_definition}
   | @Syn2{derived_type_definition}@Chg{Version=[2],New=[@\| @Syn2{interface_type_definition}],Old=[]}"}
@end{Syntax}

@begin{Legality}
A given type shall not have a subcomponent whose type is the given
type itself.
@end{Legality}

@begin{StaticSem}
@Defn{first subtype}
The @nt{defining_@!identifier} of a @nt{type_@!declaration} denotes
the @i(first subtype) of the type.
The @nt<known_@!discriminant_@!part>, if any,
defines the discriminants of the type (see @RefSec(Discriminants)).
The remainder of the @nt<type_@!declaration> defines the
remaining characteristics of (the view of) the type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@Defn{named type}
A type defined by a @nt<type_@!declaration> is a @i(named) type;
such a type has one or more nameable subtypes.
@Defn{anonymous type}
Certain other forms of declaration also include type
definitions as part of the declaration for an object@Chg{Version=[2],New=[],
Old=[ (including a parameter or a discriminant)]}. The type defined by such
a declaration is @i(anonymous) @em it has no nameable subtypes.
@Defn2{Term=[italics],Sec=(pseudo-names of anonymous types)}
For explanatory purposes, this International Standard sometimes refers to
an anonymous type by a pseudo-name, written in italics, and
uses such pseudo-names at places where the syntax normally requires
an @nt<identifier>. For a named type whose first subtype is T,
this International Standard sometimes refers to the type of T
as simply @lquotes@;the type T@rquotes@;.
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
  The only user-defined types
  that can be anonymous in the above sense are array,
  access, task, and protected types.
  An anonymous array, task, or protected type
  can be defined as part of an @nt{object_declaration}.
  An anonymous access type can be defined as part of
  @Chg{Version=[2],New=[numerous other constructs],Old=[a parameter or
  discriminant specification]}.
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00326-01]}
@Defn{full type}
A named type that is declared by a @nt<full_type_@!declaration>,
or an anonymous type that is defined @Chg{Version=[2],New=[by an
@nt{access_definition} or ],Old=[]}as part of declaring an object of the type,
is called a
@i(full type).@Defn{full type definition} @Chg{Version=[2],New=[The
declaration of a full type also declares the @i<full view> of the
type.@Defn2{Term=[full view],Sec=(of a type)} ],Old=[]}The
@nt<type_@!definition>, @nt<task_@!definition>, @nt<protected_@!definition>,
or @nt<access_@!definition> that defines a full type is called
a @i(full type definition).
@redundant[Types declared by other forms of @nt<type_@!declaration> are
not separate types; they are partial or incomplete views
of some full type.]
@begin{Honest}
  Class-wide, universal, and root numeric types are full types.
@end{Honest}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[We need to mention @nt{access_definition}
  separately, as it may occur in renames, which do not declare objects.]}
@end{Reason}

@PDefn{predefined operator}
The definition of a type implicitly declares
certain @i(predefined operators) that operate on the type,
according to what classes the type belongs,
as specified in @RefSec(Operators and Expression Evaluation).
@begin{Discussion}
  We no longer talk about the implicit declaration of basic operations.
  These are treated like an @nt{if_statement} @em they don't need
  to be declared, but are still applicable to only certain classes of types.

@end{Discussion}

@Defn{predefined type}
The @i{predefined types}
@Redundant[(for example the types
Boolean, Wide_Character, Integer,
@i{root_integer}, and @i{universal_integer})]
are the types that are defined in
@Redundant[a predefined library package called] Standard@Redundant[;
this package also includes the @Redundant{(implicit)}
declarations of their predefined operators].
@Redundant[The package Standard is described in
@RefSecNum{The Package Standard}.]
@begin{Ramification}
We use the term @lquotes@;predefined@rquotes@; to refer to entities declared in the
visible part of Standard,
to implicitly declared operators of a type whose semantics are defined
by the language, to Standard itself,
and to the @lquotes@;predefined environment@rquotes@;.
We do not use this term to refer to library packages other than Standard.
For example Text_IO is a language-defined package,
not a predefined package,
and Text_IO.Put_Line is not a predefined operation.
@end{Ramification}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(full_type_declaration)}
The elaboration of a @nt{full_type_declaration} consists of the
elaboration of the full type definition.
@PDefn2{Term=[elaboration], Sec=(full type definition)}
Each elaboration of a full type definition
creates a distinct type and its first subtype.
@begin{Reason}
  The creation is associated with the type @i(definition), rather than
  the type @i(declaration), because there are types
  that are created by full type definitions that are
  not immediately contained within a type declaration (e.g.
  an array object declaration, a singleton task declaration, etc.).
@end{Reason}
@begin{Ramification}
  Any implicit declarations that occur immediately following the full
  type definition are elaborated where they (implicitly) occur.
@end{Ramification}
@end{RunTime}

@begin{Examples}
@Leading@keepnext@i(Examples of type definitions:)
@begin(Example)
(White, Red, Yellow, Green, Blue, Brown, Black)
@key(range) 1 .. 72
@key(array)(1 .. 10) @key(of) Integer
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of type declarations:)
@end{WideAbove}
@begin(Example)
@key(type) Color  @key(is) (White, Red, Yellow, Green, Blue, Brown, Black);
@key(type) Column @key(is) @key(range) 1 .. 72;
@key(type) Table  @key(is) @key(array)(1 .. 10) @key(of) Integer;
@end(Example)
@end{Examples}

@begin{Notes}
Each of the above examples declares a named type. The identifier
given denotes the first subtype of the type. Other named subtypes of the
type can be declared with @nt<subtype_declaration>s
(see @RefSecNum{Subtype Declarations}). Although names do not directly
denote types, a phrase like @lquotes@;the type Column@rquotes@; is sometimes used
in this International Standard to refer to the type of Column, where Column denotes
the first subtype of the type. For an example of the definition
of an anonymous type, see the declaration
of the array Color_Table in @RefSecNum{Object Declarations}; its type
is anonymous @em it has no nameable subtypes.
@end{Notes}

@begin{DiffWord83}
The syntactic category @nt{full_type_declaration} now includes task and
protected type declarations.

We have generalized the concept of first-named subtype (now
called simply @lquotes@;first subtype@rquotes@;) to cover all kinds of types, for uniformity
of description elsewhere.
RM83 defined first-named subtype in Section 13.
We define first subtype here, because it is now a more fundamental concept.
We renamed the term, because in Ada 95 some first subtypes have no
name.

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
We no longer elaborate @nt{discriminant_part}s, because
there is nothing to do, and it was complex to say that you only wanted
to elaborate it once for a private or incomplete type. This is also
consistent with the fact that subprogram specifications are not
elaborated (neither in Ada 83 nor in Ada 95). Note, however, that an
@nt<access_definition> appearing in a @nt<discriminant_part> is
elaborated@Chg{Version=[2],New=[ at the @nt{full_type_declaration} (for
a nonlimited type) or],Old=[]} when an object with such a discriminant is
created@Chg{Version=[2],New=[ (for a limited type)],Old=[]}.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[Added wording so that anonymous access types
  are always full types, even if they appear in renames.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[Added interface types
  (see @RefSecNum{Interface Types}) to the syntax.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[Added a definition of full view, so that
  all types have a well-defined full view.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a @nt{full_type_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}

@LabeledSubClause{Subtype Declarations}

@begin{Intro}
A @nt<subtype_declaration> declares a subtype of some previously
declared type, as defined by a @nt<subtype_indication>.
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<subtype_declaration>,rhs="
   @key{subtype} @Syn2{defining_identifier} @key{is} @Syn2{subtype_indication}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
@Syn{lhs=<subtype_indication>,rhs=" @Chg{Version=[2],New=<[@Syn2{null_exclusion}] >,Old=<>}@Syn2{subtype_mark} [@Syn2{constraint}]"}

@Syn{lhs=<subtype_mark>,rhs="@SynI{subtype_}@Syn2{name}"}
@begin{Ramification}
Note that @nt{name} includes @nt{attribute_reference};
thus, S'Base can be used as a @nt{subtype_mark}.
@end{Ramification}
@begin{Reason}
We considered changing @nt{subtype_mark} to @ntf{subtype_name}.
However, existing users are used to the word "mark,"
so we're keeping it.
@end{Reason}

@Syn{lhs=<constraint>,rhs="@Syn2<scalar_constraint> | @Syn2<composite_constraint>"}

@Syn{lhs=<scalar_constraint>,rhs="
     @Syn2{range_constraint} | @Syn2{digits_constraint} | @Syn2{delta_constraint}"}
@Syn{lhs=<composite_constraint>,rhs="
     @Syn2{index_constraint} | @Syn2{discriminant_constraint}"}
@end{Syntax}

@begin{Resolution}
A @nt{subtype_mark} shall resolve to denote a subtype.
@Defn2{Term=[determines], Sec=(a type by a @nt{subtype_mark})}
The type @i(determined by) a @nt<subtype_mark> is the
type of the subtype denoted by the @nt{subtype_mark}.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  Types are never directly named; all @nt{subtype_mark}s denote
  subtypes @em possibly an unconstrained (base) subtype,
  but never the type.
  When we use the term @i(anonymous type) we really
  mean a type with no @Chg{Version=[3],New=[nameable],Old=[namable]}
  subtypes.
@end{Ramification}
@end{Resolution}

@begin{RunTime}
@Leading@PDefn2{Term=[elaboration], Sec=(subtype_declaration)}
The elaboration of a @nt{subtype_declaration} consists of the elaboration
of the @nt{subtype_indication}.
@PDefn2{Term=[elaboration], Sec=(subtype_indication)}
The elaboration of a @nt{subtype_indication} creates a new subtype.
If the @nt{subtype_indication} does not include a
@nt<constraint>, the new subtype has the same (possibly null)
constraint as that denoted by the @nt{subtype_mark}.
The elaboration of a @nt{subtype_indication} that includes a
@nt<constraint> proceeds as follows:
@begin{itemize}
The @nt<constraint> is first elaborated.

@IndexCheck{Range_Check}
A check is then made that
the @nt<constraint> is @i(compatible) with the
subtype denoted by the @nt{subtype_mark}.
@begin{Ramification}
  The checks associated with
  constraint compatibility are all Range_Checks.
  Discriminant_Checks and Index_Checks are associated only with
  checks that a value satisfies a constraint.
@end{Ramification}
@end{itemize}

The condition imposed by a @nt<constraint> is the condition obtained
after elaboration of the @nt<constraint>.
@RootDefn2{Term=[compatibility], Sec=(constraint with a subtype)}
The rules defining compatibility are given for each form of @nt<constraint>
in the appropriate subclause. These rules are such that if a
@nt<constraint> is @i(compatible) with a subtype,
then the condition imposed by the @nt<constraint> cannot
contradict any condition already imposed by the subtype on its values.
@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
The exception Constraint_Error is raised if any check of
compatibility fails.
@begin{Honest}
The condition imposed by a @nt<constraint> is named after it @em
a @nt<range_constraint> imposes a range constraint, etc.
@end{Honest}
@begin{Ramification}
A @nt<range_constraint> causes freezing of its type.
Other @nt<constraint>s do not.
@end{Ramification}
@end{RunTime}

@begin{Notes}
A @nt<scalar_constraint> may be applied to a subtype of an appropriate
scalar type (see @RefSecNum{Scalar Types}, @RefSecNum{Fixed Point Types},
and @RefSecNum{Reduced Accuracy Subtypes}), even if the subtype
is already constrained. On the other hand, a @nt<composite_constraint>
may be applied to a composite subtype (or an access-to-composite subtype)
only if the composite subtype is unconstrained
(see @RefSecNum{Index Constraints and Discrete Ranges} and
@RefSecNum{Discriminant Constraints}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of subtype declarations:)
@begin(Example)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
@key(subtype) Rainbow   @key(is) Color @key(range) Red .. Blue;        --@RI[  see @RefSecNum(Type Declarations)]
@key(subtype) Red_Blue  @key(is) Rainbow;
@key(subtype) Int       @key(is) Integer;
@key(subtype) Small_Int @key(is) Integer @key(range) -10 .. 10;
@key(subtype) Up_To_K   @key(is) Column @key(range) 1 .. K;            --@RI[  see @RefSecNum(Type Declarations)]
@key(subtype) Square    @key(is) Matrix(1 .. 10, 1 .. 10);       --@RI[  see @RefSecNum(Array Types)]
@key(subtype) Male      @key(is) Person(Sex => M);               --@RI[  see @RefSecNum(Incomplete Type Declarations)]@Chg{Version=[2],New=[
@key(subtype) Binop_Ref @key(is not null) Binop_Ptr;             --@RI[  see @RefSecNum(Access Types)]],Old=[]}
@end(Example)

@end{Examples}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
In Ada 95, all @nt<range_constraint>s cause freezing of their type.
Hence, a type-related representation item for a scalar type has to
precede any @nt<range_constraint>s whose type is the scalar type.
@end{Incompatible83}

@begin{DiffWord83}
@nt{Subtype_mark}s allow only subtype names now, since
types are never directly named.
There is no need for RM83-3.3.2(3), which says a
@nt{subtype_mark} can denote both the type and the subtype;
in Ada 95, you denote an unconstrained (base) subtype if you want,
but never the type.

The syntactic category @ntf{type_mark} is now called @nt{subtype_mark},
since it always denotes a subtype.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  An optional @nt{null_exclusion} can be used in a @nt{subtype_indication}.
  This is described in @RefSecNum{Access Types}.]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a @nt{subtype_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}




@LabeledSubClause{Classification of Operations}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
@Defn{operates on a type}
An operation @i(operates
on a type) @i(T) if it yields a value of type @i(T), if it has an operand
whose expected type
(see @RefSecNum{The Context of Overload Resolution})
is @i(T), or if it has an access parameter@Chg{Version=[2],New=[ or access result type],Old=[]}
(see @RefSecNum(Subprogram Declarations))
designating @i(T).
@Defn2{Term=[predefined operation], Sec=(of a type)}
A predefined operator, or
other language-defined operation
such as assignment or a membership test, that operates
on a type, is called
a @i(predefined operation) of the type.
@Defn2{Term=[primitive operations], Sec=(of a type)}
The @i(primitive operations) of a type are
the predefined operations of the type, plus any user-defined
primitive subprograms.
@ToGlossary{Term=<Primitive operations>,
  Text=<The primitive operations of a type are the operations
  (such as subprograms) declared together with
  the type declaration.
  They are inherited by other types in the same class of types.
  For a tagged type,
  the primitive subprograms are dispatching subprograms,
  providing run-time polymorphism.
  A dispatching subprogram may be called with statically
  tagged operands, in which case the subprogram body invoked
  is determined at compile time.
  Alternatively, a dispatching subprogram may be called
  using a dispatching call,
  in which case the subprogram body invoked is determined
  at run time.>}
@begin{Honest}
  Protected subprograms are not considered to be
  @lquotes@;primitive subprograms,@rquotes@; even though they are
  subprograms, and they are inherited by derived types.
@end{Honest}
@begin{Discussion}
  We use the term
  @lquotes@;primitive subprogram@rquotes@; in most of the rest of the manual.
  The term @lquotes@;primitive operation@rquotes@; is used mostly in conceptual
  discussions.
@end{Discussion}

@Leading@Defn2{Term=[primitive subprograms], Sec=(of a type)}
The @i(primitive subprograms) of a
specific type are defined as follows:
@begin{Itemize}
  The predefined operators of the
  type (see @RefSecNum{Operators and Expression Evaluation});

  For a derived type, the inherited (see @RefSecNum{Derived Types and Classes})
  user-defined subprograms;

  For an enumeration type, the enumeration literals (which are
  considered parameterless
  functions @em see @RefSecNum{Enumeration Types});

  For a specific type declared immediately within
  a @nt<package_specification>,
  any subprograms (in addition to the enumeration literals)
  that are
  explicitly declared immediately within the same
  @nt<package_specification> and that operate on the type;

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0128-1]}
  @ChgAdded{Version=[3],Text=[For a specific type with an
  explicitly declared primitive "=" operator whose result type
  is Boolean, the corresponding "/=" operator (see
  @RefSecNum{Overloading of Operators});]}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00200-01]}
  @Defn2{Term=[override], Sec=(a primitive subprogram)}
  @Chg{Version=[2],New=[For a nonformal type, any],Old=[Any]}
  subprograms not covered above
  @Redundant[that are explicitly declared immediately within the same
  declarative region as the type] and that override (see @RefSecNum{Visibility})
  other implicitly declared primitive subprograms of the type.
@end{Itemize}
@begin{Discussion}
  In Ada 83, only subprograms
  declared in the visible part were @lquotes@;primitive@rquotes@; (i.e.
  derivable). In Ada 95, mostly because of child library units,
  we include all operations declared in the private part as well,
  and all operations that override implicit declarations.
@end{Discussion}
@begin{Ramification}
  It is possible for a subprogram
  to be primitive for more than one type, though it is illegal
  for a subprogram to be primitive for more than one tagged type.
  See @RefSecNum(Tagged Types and Type Extensions).
@end{Ramification}
@begin(Discussion)
  The order of the implicit declarations when there are both
  predefined operators and inherited subprograms is described in
  @RefSec(Derived Types and Classes).
@end(Discussion)
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00200-01]}
  @ChgAdded{Version=[2],Text=[Subprograms declared in a generic package
  specification are never primitive for a formal type, even if they happen
  to override an operation of the formal type. This includes formal
  subprograms, which are never primitive operations (that's true even
  for an abstract formal subprogram).]}
@end{Ramification}

@Defn2{Term=[primitive operator], Sec=(of a type)}
A primitive subprogram whose designator is an @nt<operator_symbol>
is called a @i(primitive operator).

@end{StaticSem}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
The attribute S'Base is no longer defined for
nonscalar subtypes. Since this was only permitted
as the prefix of another attribute, and there are no
interesting nonscalar attributes defined for an unconstrained
composite or access subtype, this should not affect any
existing programs.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The primitive subprograms (derivable subprograms) include
subprograms declared in the private part of a package
specification as well, and those that override implicitly declared
subprograms, even if declared in a body.
@end{Extend83}

@begin{DiffWord83}
We have dropped the confusing
term @i<operation of a type> in favor of the more useful
@i<primitive operation of a type> and the phrase
@i<operates on a type>.

The description of S'Base has been moved to
@RefSec{Scalar Types} because it is now defined only for scalar types.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00200-01]}
  @ChgAdded{Version=[2],Text=[Clarified that a formal subprogram that happens
  to override a primitive operation of a formal type is not a primitive
  operation (and thus not a dispatching operation) of the formal type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Added wording to include access result types
  in the kinds of operations that operate on a type T.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0128-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> The implicitly declared "/="
  for a primitive "=" operator is also primitive; this makes it eligible
  to be made visible by a @key[use type] clause.]}
@end{DiffWord2005}


@LabeledAddedSubClause{Version=[3],Name=[Subtype Predicates]}

@begin{Intro}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[The language-defined @i{predicate aspects}
Static_Predicate and Dynamic_Predicate may be used to define properties of
subtypes. A @i{predicate specification} is an @nt{aspect_specification}
for one of the two predicate
aspects.@Defn{predicate aspect}@Defn{predicate specification}@PDefn2{Term=[aspect],Sec=(predicate)}@AspectDefn{Static_Predicate}@AspectDefn{Dynamic_Predicate}
General rules for aspects and @nt{aspect_specification}s are found in
Section @RefSecNum{Representation Issues} (@RefSecNum{Operational and Representation Aspects}
and @RefSecNum{Aspect Specifications} respectively).]}
@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Static_Predicate],
  Text=[@ChgAdded{Version=[3],Text=[Condition that must hold true for objects of
    a given subtype; the subtype may be static.]}]}

@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Dynamic_Predicate],
  Text=[@ChgAdded{Version=[3],Text=[Condition that must hold true for objects of
    a given subtype; the subtype is not static.]}]}
@end{Intro}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Text=[The expected type for a predicate aspect
@nt{expression} is any boolean
type.@PDefn2{Term=[expected type],Sec=[@nt{expression} of a predicate aspect]}]}

@end{Resolution}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Type=[Leading],Text=[A predicate specification may be
given on a @nt{type_declaration} or a @nt{subtype_declaration}, and applies
to the declared subtype. In addition, predicate specifications apply to
certain other subtypes:]}
@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[For a (first) subtype defined by a derived
  type declaration, the predicates of the parent subtype and the progenitor
  subtypes apply.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[For a subtype created by a @nt{subtype_indication},
  the predicate of the subtype denoted by the @nt{subtype_mark} applies.]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Text=[The @i<predicate> of a subtype consists of all
predicate specifications that apply, and-ed together; if no predicate
specifications apply, the predicate is True @Redundant[(in particular, the
predicate of a base subtype is True)].@Defn2{Term=[Predicate],Sec=(of a subtype)}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0290-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Predicate checks are defined to be
@i<enabled> or @i<disabled> for a given subtype as
follows:@Defn2{Term=[enabled],Sec=[predicate checks]}@Defn2{Term=[disabled],Sec=[predicate checks]}@Defn2{Term=[predicate check],Sec=[enabled]}]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[If a subtype is declared by
  a @nt{type_declaration} or @nt{subtype_declaration}
  that includes a predicate specification, then:]}
  @begin{Itemize}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[if performing checks is required by the
      Static_Predicate assertion policy (see @RefSecNum{Pragmas Assert and Assertion_Policy})
      and the declaration includes a Static_Predicate specification, then
      predicate checks are enabled for the subtype;]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[if performing checks is required by the
      Dynamic_Predicate assertion policy (see @RefSecNum{Pragmas Assert and Assertion_Policy})
      and the declaration includes a Dynamic_Predicate specification, then
      predicate checks are enabled for the subtype;]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[otherwise, predicate checks are disabled for the
      subtype@Redundant[, regardless of whether predicate checking is enabled
      for any other subtypes mentioned in the declaration];]}
  @end{Itemize}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If a subtype is defined by a derived type
  declaration that does not include a predicate specification, then predicate
  checks are enabled for the subtype if and only if predicate checks are enabled
  for at least one of the parent subtype and the progenitor subtypes;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If a subtype is created by a
  @nt{subtype_indication} other than in one of the previous cases, then
  predicate checks are enabled for the subtype if and only if predicate checks
  are enabled for the subtype denoted by the @nt{subtype_mark};]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Otherwise, predicate checks are disabled for the
  given subtype.]}

@begin{Discussion}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[In this case, no predicate specifications can
    apply to the subtype and so it doesn't typically matter whether predicate
    checks are enabled. This rule does make a difference, however, when
    determining whether predicate checks are enabled for another type when this
    type is one of multiple progenitors. See the @ldquote@;derived type declaration@rdquote
    wording above.]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[Even when predicate checks are disabled,
    a predicate cam affect various @LegalityTitle, the results of membership
    tests, the items in a @key[for] loop, and the result of the Valid
    attribute.]}
@end{Discussion}

@end{Itemize}

@end{StaticSem}


@begin{Legality}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[The @nt{expression} of a
Static_Predicate specification shall be @i<predicate-static>; that is, one of
the following:@Defn{predicate-static}@Defn2{Term=[expression],Sec=[predicate-static]}]}
@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[a static expression;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[a membership test whose @nt{simple_expression}
  is the current instance, and whose @nt{membership_choice_list} meets the
  requirements for a static membership test
  (see @RefSecNum{Static Expressions and Static Subtypes});]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[a @nt{case_expression} whose
  @SynI{selecting_}@nt{expression} is the current instance,
  and whose @SynI{dependent_}@nt{expression}s are static expressions;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[a call to a predefined equality or ordering
  operator, where one operand is the current instance, and the other is a
  static expression;]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[a call to a predefined boolean logical operator,
  where each operand is predicate-static;]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Text=[a short-circuit control form where both operands
  are predicate-static; or]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[a parenthesized predicate-static @nt{expression}.]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[A predicate shall not be specified for an incomplete
subtype.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The expression of such a predicate could not
  depend on the properties of the value of the type (since it doesn't have
  any), so it is useless and we don't want to require the added complexity
  needed to support it.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0287-1]}
@ChgAdded{Version=[3],Text=[If a predicate applies to a subtype, then that
predicate shall not mention any other subtype to which the same predicate
applies.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[This is intended to prevent
  recursive predicates, which cause definitional problems for static predicates.
  Inside of the predicate, the subtype name refers to the current instance of
  the subtype, which is an object, so a direct use of the subtype name cannot be
  recursive. But other subtypes naming the same type might:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[   @key[type] Really_Ugly @key[is private];
@key[private]
   @key[subtype] Ugly @key[is] Really_Ugly;
   @key[type] Really_Ugly @key[is new] Integer
      @key[with] Static_Predicate => Really_Ugly @key[not in] Ugly; --@ExamCom{ Illegal!}]}
@end{Example}
@end{Reason}


@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Text=[An index subtype, @nt{discrete_range} of an
@nt{index_constraint} or @nt{slice}, or a
@nt{discrete_subtype_definition} of a @nt{constrained_array_definition},
@nt{entry_declaration}, or @nt{entry_index_specification} shall not denote a
subtype to which predicate specifications apply.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Text=[The @nt{prefix} of an @nt{attribute_reference}
whose @nt{attribute_designator} is First, Last, or Range shall not denote a
scalar subtype to which predicate specifications apply.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0297-1]}
  @ChgAdded{Version=[3],Text=[This is to prevent confusion about whether the
  First value is the lowest value of the subtype (which does not depend on the
  predicate) or the lowest value of the subtype which meets the predicate.
  (For a dynamic predicate, determining this latter value is expensive as it
  would usually require a loop.) For a static subtype that has a static
  predicate, the First_Valid and Last_Valid attributes (see
  @RefSecNum{Operations of Discrete Types}) can be used instead.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0262-1],ARef=[AI05-0287-1]}
@ChgAdded{Version=[3],Text=[The @nt{discrete_subtype_definition} of a
@nt{loop_parameter_specification} shall not denote a
nonstatic subtype to which predicate specifications apply or any
subtype to which Dynamic_Predicate specifications apply.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[The @nt{discrete_choice} of a
@nt{named_array_aggregate} shall not denote a nonstatic subtype to which
predicate specifications apply.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[This rule prevents noncontiguous
  dynamically bounded array aggregates, which could be expensive to check for.
  (Array aggregates have rules to prevent problems with static subtypes.) We
  define this rule here so that the runtime generic body check applies.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[In addition to the places
where @LegalityTitle normally apply (see @RefSecNum{Generic Instantiation}),
these rules apply also in the private part of an instance of a generic
unit.@PDefn{generic contract issue}]}
@end{Legality}

@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0290-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[If predicate checks are enabled for a
given subtype, then:]}
@begin{DescribeCode}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[@Redundant[On every subtype conversion, the
  predicate of the target subtype is evaluated, and a check is performed that the
  predicate is True. This includes all parameter passing, except for certain
  parameters passed by reference, which are covered by the following rule: ] After
  normal completion and leaving of a subprogram, for each @key[in out] or
  @key[out] parameter
  that is passed by reference, the predicate of the subtype of the actual is
  evaluated, and a check is performed that the predicate is True. For an object created
  by an @nt{object_declaration} with no explicit initialization @nt{expression},
  or by an uninitialized @nt{allocator}, if any subcomponents have
  @nt{default_expression}s, the predicate of the
  nominal subtype of the created object is evaluated, and a check is performed
  that the predicate is True. Assertions.Assertion_Error is raised if any
  of these checks fail.@Defn2{Term=[predicate check],
  Sec=[@key[in out] parameters]}@Defn2{Term=[predicate check],
  Sec=[@nt{object_declaration}]}@Defn2{Term=[predicate check],
  Sec=[@nt{allocator}]}@Defn2{Term=[check, language-defined],
  Sec=[controlled by assertion policy]}@Defn2{Term=(Assertion_Error),
  Sec=(raised by failure of run-time check)}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=<Predicates are not evaluated at the point of the
  (sub)type declaration.>}
@end{Ramification}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Static_Predicate checks can be removed even in the
  presence of potentially invalid values, just as constraint checks can be
  removed.]}
@end{ImplNote}
@end{DescribeCode}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[A value @i<satisfies> a predicate if the
predicate is True for that value.@PDefn2{Term=[satisfies], Sec=(a subtype predicate)}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0276-1]}
@ChgAdded{Version=[3],Text=[If any of the above @LegalityTitle is violated in an
instance of a generic unit, Program_Error is raised at the point of the
violation.]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is the usual way around the contract model;
  this applies even in instance bodies. Note that errors in instance
  specifications will be detected at compile-time by the "re-check" of the
  specification, only errors in the body should raise Program_Error.]}
@end{Discussion}
@end{Runtime}

@begin{Notes}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Text=[A predicate specification does not cause a subtype
to be considered constrained.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Text=[A Static_Predicate, like a constraint, always
remains True for all objects of the subtype, except in the case of uninitialized
variables and other invalid values. A Dynamic_Predicate, on the other hand, is
checked as specified above, but can become False at other times. For example,
the predicate of a record subtype is not checked when a subcomponent is
modified.]}
@end{Notes}


@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0262-1],ARef=[AI05-0276-1],ARef=[AI05-0290-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Predicate aspects are new in Ada 2012.]}
@end{Extend2005}



@LabeledClause{Objects and Named Numbers}

@begin{Intro}
@redundant[Objects are created at run time
and contain a value of a given type.
@Defn2{Term=[creation], Sec=(of an object)}
An object can be created and
initialized as part of elaborating a
declaration, evaluating an @nt<allocator>, @nt<aggregate>,
or @nt<function_call>, or passing a parameter by copy.
Prior to reclaiming the storage for an object, it is finalized if
necessary (see @RefSecNum(Completion and Finalization)).]
@end{Intro}

@begin{StaticSem}

@Leading@keepnext@Defn{object}
All of the following are objects:
@ToGlossary{Term=<Object>,
  Text=<An object is either a constant or a variable.
  An object contains a value.
  An object is created by an @nt(object_declaration)
  or by an @nt(allocator).
  A formal parameter is (a view of) an object.
  A subcomponent of an object is an object.>}
@begin(itemize)
  the entity declared by
  an @nt<object_declaration>;

  a formal parameter of a subprogram, entry, or generic subprogram;

  a generic formal object;

  a loop parameter;

  a choice parameter of an @nt<exception_handler>;

  an entry index of an @nt<entry_body>;

  the result of dereferencing an
  access-to-object value (see @RefSecNum{Names});

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0015-1]}
  the @Chg{Version=[2],New=[return object ],Old=[]}@Chg{Version=[3],
  New=[of a function],Old=[@Chg{Version=[2],New=[created as the ],Old=[]}result
  of evaluating a @nt<function_call> (or the equivalent
  operator invocation @em see @RefSecNum{Overloading of Operators})]};

  the result of evaluating an @nt<aggregate>;

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[a @nt{qualified_expression} whose operand denotes an object;]}

  a component, slice, or view conversion of another object.
@end(itemize)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0054-2]}
@Defn{constant}
@Defn{variable}
@Defn{constant object}
@Defn{variable object}
@Defn{constant view}
@Defn{variable view}
An object is either a @i(constant) object or a @i(variable)
object.@Chg{Version=[3],New=[],Old=[ The value of a constant object
cannot be changed between its initialization
and its finalization, whereas the value of a variable object can be
changed.]}
Similarly, a view of an object is either a @i(constant) or
a @i(variable). All views of a constant
@Chg{Version=[3],New=[elementary ],Old=[]}object are constant.
@Chg{Version=[3],New=[All views of a constant composite object are
constant, except for parts that are of controlled or immutably limited
types; variable views of those parts and their subcomponents
may exist. In this sense, objects of controlled and immutably
limited types are @i<inherently mutable>@Defn{inherently mutable object}. ],Old=[]}A
constant view of @Chg{Version=[3],New=[an],Old=[a variable]} object
cannot be used to modify @Chg{Version=[3],New=[its value],Old=[the value
of the variable]}. The terms constant and variable by themselves
refer to constant and variable views of objects.

@Defn2{Term=[read], Sec=(the value of an object)}
The value of an object
is @i(read) when the value of any part of the object is evaluated,
or when the value of an enclosing object is evaluated.
@Defn2{Term=[update], Sec=(the value of an object)}
The value of a variable
is @i(updated) when an assignment is performed to any part of the
variable, or when an assignment is performed to an enclosing object.
@begin{Ramification}
Reading and updating are intended to include read/write references of
any kind, even if they are not associated with the evaluation of a
particular construct. Consider, for example, the expression
@lquotes@;X.@key[all](F)@rquotes@;,
where X is an access-to-array object, and F is a function.
The implementation is allowed to first evaluate @lquotes@;X.@key[all]@rquotes@;
and then F.
Finally, a read is performed to get the value of the F'th component of
the array.
Note that the array is not necessarily read as part of the evaluation of
@lquotes@;X.@key[all]@rquotes@;.
This is important, because if F were to free X using
Unchecked_Deallocation, we want the execution of the final read to be
erroneous.
@end{Ramification}

@Leading@;Whether a view of an object is constant or variable is determined
by the definition of the view.
The following (and no others) represent constants:
@begin(itemize)
  an object declared by an @nt<object_declaration> with the
  reserved word @key(constant);

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00385-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[We mean the word @key{constant}
  as defined by the grammar for @nt{object_declaration}, not some random word
  @key{constant}. Thus,]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[X : @key{access constant} T;]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[is not a constant.]}
@end{Honest}

  a formal parameter or generic formal object of mode @key(in);

  a discriminant;

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[a loop parameter unless specified to be a variable
  for a generalized loop (see @RefSecNum{Generalized Loop Iteration});]}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
  a @Chg{Version=[3],New=[],Old=[loop parameter, ]}choice
  parameter@Chg{Version=[3],New=[],Old=[,]} or entry index;

  the dereference of an access-to-constant value;

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0015-1]}
  @ChgAdded{Version=[3],Text=[the return object declared by an
  @nt{extended_return_statement} with the reserved word @key[constant];]}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0015-1]}
  the @Chg{Version=[3],New=[object denoted by],Old=[result of evaluating]}
  a @nt<function_call> or an @nt<aggregate>;

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[the result of evaluating a @nt{qualified_expression};]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0120-1]}
  @ChgAdded{Version=[3],Text=[within the body of a protected function (or a
  function declared immediately within a @nt{protected_body}), the current
  instance of the enclosing protected unit;]}

  a @nt<selected_component>, @nt<indexed_component>,
  @nt<slice>, or view conversion of a constant.
  @begin{Honest}
    @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00114-01]}
    @ChgNote{This is just wrong, even for Ada 95.}
    @ChgDeleted{Version=[2],Text=[A noninvertible view conversion to a
    general access type
    is also defined to be a constant @em see @RefSecNum(Type Conversions).]}
  @end{Honest}

@end(itemize)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
@Defn{nominal subtype}
At the place where a view of an
object is defined, a @i(nominal subtype) is associated
with the view.
@Defn{actual subtype}
@IndexSee{Term=[subtype (of an object)],See=(actual subtype of an object)}
The object's @i(actual subtype) (that is, its
subtype) can be more restrictive than
the nominal subtype of the view; it always is if the nominal subtype
is an @i(indefinite subtype).
@Defn{indefinite subtype}
@Defn{definite subtype}
A subtype is an indefinite subtype if it is an unconstrained array
subtype, or if it has unknown discriminants or unconstrained
discriminants without defaults (see @RefSecNum(Discriminants));
otherwise@Chg{Version=[3],New=[,],Old=[]} the subtype is a @i{definite}
subtype @Redundant[(all elementary subtypes are definite subtypes)].
@redundant[A class-wide subtype is defined to have unknown discriminants,
and is therefore an indefinite subtype.
An indefinite subtype does not by itself
provide enough information to create an object;
an additional @nt<constraint> or
explicit initialization @nt<expression>
is necessary (see @RefSecNum(Object Declarations)).
A component cannot have an indefinite nominal subtype.]

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0008-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[A view of a composite object is
@i<known to be constrained> if:]}

@begin{Itemize}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[its nominal subtype is constrained, and is not an
untagged partial view; or]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[its nominal subtype is indefinite; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0008-1],ARef=[AI05-0093-1]}
@ChgAdded{Version=[3],Text=[its type is immutably limited (see @RefSecNum{Limited Types}); or]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[it is part of a stand-alone constant (including a
generic formal object of mode @key[in]); or]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[it is part of a formal parameter of mode @key[in]; or]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[it is part of the object denoted by a
@nt{function_call} or @nt{aggregate}; or]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[it is part of a constant return object of an
@nt{extended_return_statement}; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0008-1],ARef=[AI05-0041-1]}
@ChgAdded{Version=[3],Text=[it is a dereference of a pool-specific access type,
and there is no ancestor of its type that has a constrained partial view.]}

@end{Itemize}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[We do not include dereferences of general access
types because they might denote stand-alone aliased unconstrained variables.
That's true even for access-to-constant types (the denoted object does not
have to be a constant).]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0008-1]}
@ChgAdded{Version=[3],Text=[There are other cases that could have been
included in this definition (view conversions, the current instance of
a type, objects of a formal discriminated
private type), but these are not relevant to the places this term is
used, so they were not included. If this term is used in additional places,
the definition should be checked to see if any of these additional cases
are relevant and appropriate wording added if necessary.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0008-1],ARef=[AI05-0041-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[For the purposes of
determining within a generic body whether an object is known
to be constrained:]}
@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[if a subtype is a descendant of an untagged generic
formal private or derived type, and the subtype is not an unconstrained array
subtype, it is not considered indefinite and is considered to have a
constrained partial view;]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[if a subtype is a descendant of a formal access type, it is not
considered pool-specific.]}

@end{Itemize}


@Defn{named number}
A @i(named number) provides a name for a numeric value known
at compile time. It is declared by a @nt<number_declaration>.
@end{StaticSem}

@begin{Notes}
A constant cannot be the target of an assignment operation, nor be
passed as an @key(in) @key(out) or @key(out)
parameter, between its initialization and finalization, if any.

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0054-2]}
@ChgAdded{Version=[3],Text=[The value of a constant object cannot be
changed after its initialization, except in some cases where the
object has a controlled or immutably limited part (see
@RefSecNum{Limited Types}, @RefSecNum{Assignment and Finalization},
and @RefSecNum{Data Validity}).]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
The nominal and actual subtypes of an elementary object are
always the same. For a discriminated or array object,
if the nominal subtype is constrained@Chg{Version=[3],New=[,],Old=[]} then so is
the actual subtype.
@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
There are additional kinds of objects (choice parameters and
entry indices of entry bodies).

The result of a function and of evaluating an aggregate
are considered (constant) objects. This is necessary to explain
the action of finalization on such things.
Because a @nt<function_call> is also syntactically a @nt<name>
(see @RefSecNum(Names)), the result of a @nt{function_call} can be renamed,
thereby allowing repeated use of the result without calling
the function again.
@end{Extend83}

@begin{DiffWord83}
This clause and its subclauses now follow the clause and
subclauses on types and subtypes, to cut down on the number of
forward references.

The term nominal subtype is new. It is used to distinguish
what is known at compile time about an object's constraint, versus what
its "true" run-time constraint is.

The terms definite and indefinite (which apply to
subtypes) are new.
They are used to aid in the description of generic formal
type matching,
and to specify when an explicit initial value is required
in an @nt<object_declaration>.

We have moved the syntax for @nt<object_declaration> and
@nt<number_declaration> down into their respective subclauses, to
keep the syntax close to the description of the associated semantics.

We talk about variables and constants here, since the discussion is
not specific to @nt<object_declaration>s, and it seems
better to have the list of the kinds of constants juxtaposed
with the kinds of objects.

We no longer talk about indirect updating due to parameter passing.
Parameter passing is handled in 6.2 and 6.4.1 in a way that there
is no need to mention it here in the definition of read and update.
Reading and updating now includes the case of evaluating or
assigning to an enclosing object.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Clarified that the return object is the object
  created by a function call.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0015-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Added wording to allow
  return objects to be declared as constants, and corrected the definition
  of return objects as objects.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0008-1],ARef=[AI05-0041-1],ARef=[AI05-0093-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a definition of
  @i<known to be constrained>, for use in other rules.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0054-2]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> We now recognize the fact
  that not all declared constant objects are immutable; for those that
  a variable view can be constructed, they can be changed via that view.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0120-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added the current instance of
  a protected object to the list of constant views; since the list
  claims to include all possibilities, it had better include that one.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[The result of a
  @nt{qualified_expression} is defined to be a constant view and is defined
  to be an object if the operand of the @nt{qualified_expression} is an object.
  These definitions, combined with some grammar changes, allow
  @nt{qualified_expression}s to be used in more places. See
  @RefSecNum{Names} for details.]}@ChgNote{This is defined an extension in @RefSecNum{Names}.}
@end{DiffWord2005}


@LabeledSubClause{Object Declarations}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@RootDefn{stand-alone object}
@Defn{explicit initial value}
@Defn{initialization expression}
An @nt<object_declaration> declares a @i(stand-alone) object with a given
nominal subtype and, optionally, an explicit initial
value given by an initialization expression.
@Defn{anonymous array type}
@Defn{anonymous task type}
@Defn{anonymous protected type}
For an array, @Chg{Version=[3],New=[access, ],Old=[]}task, or protected object,
the @nt<object_declaration> may include the definition
of the (anonymous) type of the object.
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00385-01],ARef=[AI95-00406-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<object_declaration>,rhs="
    @Syn2{defining_identifier_list} : [@key{aliased}] [@key{constant}] @Syn2{subtype_indication} [:= @Syn2{expression}]@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};@Chg{Version=[2],New=<
  | @Syn2{defining_identifier_list} : [@key{aliased}] [@key{constant}] @Syn2{access_definition} [:= @Syn2{expression}]@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};>,Old=<>}
  | @Syn2{defining_identifier_list} : [@key{aliased}] [@key{constant}] @Syn2{array_type_definition} [:= @Syn2{expression}]@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]};
  | @Syn2{single_task_declaration}
  | @Syn2{single_protected_declaration}"}

@Syn{lhs=<defining_identifier_list>,rhs="
  @Syn2{defining_identifier} {, @Syn2{defining_identifier}}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(object_declaration initialization expression)}
For an @nt<object_declaration> with an @nt<expression> following
the compound delimiter :=,
the type expected for
the @nt<expression> is that of the object.
@Defn{initialization expression}
This @nt<expression> is called the @i(initialization expression).
@IndexSee{Term=[constructor],See=[initialization expression]}
@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
An @nt<object_declaration> without the reserved word @key(constant)
declares a variable object. If it has a @nt<subtype_indication> or
an @nt<array_type_definition> that defines an indefinite subtype,
then there shall be an initialization expression.@Chg{Version=[2],New=[],Old=[
An initialization expression shall not be given if the object is
of a limited type.]}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
An @nt<object_declaration> with the reserved word @key(constant)
declares a constant object.
@Defn{full constant declaration}
If it has an initialization expression,
then it is called a @i(full constant declaration).
@Defn{deferred constant declaration}
Otherwise@Chg{Version=[3],New=[,],Old=[]} it is called
a @i(deferred constant declaration).
The rules for deferred constant declarations are given in clause
@RefSecNum(Deferred Constants). The rules for full constant declarations
are given in this subclause.

Any declaration that includes a @nt{defining_identifier_list}
with more than one @nt{defining_identifier}
is equivalent to a series of declarations each containing one
@nt{defining_identifier} from the list,
with the rest of the text of the declaration copied for each
declaration in the series, in the same order as the list.
The remainder of this International Standard relies
on this equivalence;
explanations are given for
declarations with a single @nt<defining_identifier>.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00385-01]}
@Defn{nominal subtype}
The @nt<subtype_indication>@Chg{Version=[2],New=[, @nt{access_definition},],Old=[]}
or full type definition of an
@nt<object_declaration> defines the nominal subtype of the object.
The @nt<object_declaration> declares an object of the type
of the nominal subtype.
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00385-01]}
The phrase @lquotes@;full type definition@rquotes@; here includes the
case of an anonymous array, @Chg{Version=[2],New=[access, ],Old=[]}task, or
protected type.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00373-01]}
@ChgAdded{Version=[2],Text=[@Defn{requires late initialization}
A component of an object is said to
@i{require late initialization} if it has an access discriminant value
constrained by a per-object expression, or if it has an initialization
expression that includes a name denoting the current instance of the type
or denoting an access discriminant.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Such components can depend on the values of
other components of the object. We want to initialize them as late and as
reproducibly as possible.]}
@end{Reason}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
@Defn2{Term=[constraint], Sec=(of an object)}
If a composite object declared by an
@nt{object_declaration} has an unconstrained nominal subtype,
then if this subtype is indefinite
or the object is constant @Chg{Version=[2],New=[],
Old=[or aliased (see @RefSecNum(Access Types)) ]}the
actual subtype of this object is constrained.
The constraint is determined
by the bounds or discriminants (if any) of its initial value;
@Defn{constrained by its initial value}
the object is said to be @i(constrained by its initial value).
@Defn2{Term=[actual subtype], Sec=(of an object)}
@IndexSee{Term=[subtype (of an object)],See=(actual subtype of an object)}
@Chg{Version=[2],New=[],
Old=[@Redundant[In the case of an aliased object,
this initial value may be either explicit or implicit;
in the other cases, an explicit initial value is required.] ]}When
not constrained by its initial value, the actual and nominal
subtypes of the object are the same.
@Defn2{Term=[constrained], Sec=(object)}
@Defn2{Term=[unconstrained], Sec=(object)}
If its actual subtype is constrained, the object
is called a @i(constrained object).

@Leading@Defn2{Term=[implicit initial values], Sec=(for a subtype)}
For an @nt<object_declaration> without an initialization expression,
any initial values for the object or its subcomponents
are determined by the @i(implicit initial values) defined for
its nominal subtype, as follows:
@begin(itemize)
  The implicit initial value for an access subtype is the
  null value of the access type.

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0228-1]}
  @ChgAdded{Version=[3],Text=[The implicit initial value for a scalar subtype
  that has the Default_Value aspect specified is the value of that aspect
  converted to the nominal subtype (which might raise Constraint_Error @em see
  @RefSec{Type Conversions});@PDefn2{Term=[implicit subtype conversion],
  Sec=(default value of a scalar)}]}

  @begin{Ramification}
    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[This is a @RuntimeTitle rule, so the
    visibility of the @nt{aspect_specification} is not relevant @em if the full
    type for a private type has the Default_Value aspect specified, partial
    views of the type also have this implicit initial value.]}
  @end{Ramification}

  The implicit initial (and only) value for each discriminant
  of a constrained discriminated subtype is defined by the subtype.

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0228-1]}
  For a (definite) composite subtype,
  the implicit initial value of each component
  with a @nt<default_expression> is obtained by
  evaluation of this expression and conversion to the
  component's nominal subtype (which might raise
  Constraint_Error@Chg{Version=[3],New=[],Old=[ @em see
  @RefSec{Type Conversions}]}), unless the component is a
  discriminant of a constrained subtype (the previous case),
  or is in an excluded @nt<variant>
  (see @RefSecNum(Variant Parts and Discrete Choices)).
  @PDefn2{Term=[implicit subtype conversion],Sec=(component defaults)}
  For each component that does not have a @nt<default_expression>,
  @Chg{Version=[3],New=[if the composite subtype has the Default_Component_Value
  aspect specified, the implicit initial value is the value of that aspect
  converted to the component's nominal subtype; otherwise, ],Old=[]}any
  implicit initial values are those determined by the component's
  nominal subtype.

  For a protected or task subtype, there is an implicit component
  (an entry queue) corresponding to each entry, with its implicit
  initial value being an empty queue.
@begin(ImplNote)
    The implementation may add implicit components for its own use,
    which might have implicit initial values.
    For a task subtype, such components might represent the state
    of the associated thread of control. For a type with dynamic-sized
    components, such implicit components might be used to hold the offset to
    some explicit component.
@end(ImplNote)
@end(itemize)

@Leading@PDefn2{Term=[elaboration], Sec=(object_declaration)}
The elaboration of an @nt{object_declaration} proceeds in the following
sequence of steps:
@begin(enumerate)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00385-01]}
  The @nt{subtype_@!indication},@Chg{Version=[2],New=[ @nt{access_@!definition},],Old=[]}
  @nt<array_@!type_@!definition>, @nt{single_@!task_@!declaration},
  or @nt{single_@!protected_@!declaration} is first elaborated.
  This creates the nominal subtype (and the anonymous type in the
  @Chg{Version=[2],New=[last four],Old=[latter three]} cases).

  If the @nt<object_declaration> includes an initialization expression,
  the (explicit) initial value is obtained by evaluating the
  expression and converting it to the nominal subtype (which might
  raise Constraint_Error @em see @RefSecNum(Type Conversions)).
  @PDefn2{Term=[implicit subtype conversion],Sec=(initialization expression)}

  @ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0002],ARef=[AI95-00171-01]}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
  The object is created, and, if there is not an initialization expression,
  @Chg{Version=[2],New=[the object is @i{initialized by default}.
  @Defn{initialized by default}When an object is initialized by
  default, ],Old=[]}any per-object @Chg{New=[constraints],Old=[expressions]}
  (see @RefSecNum(Record Types)) are
  @Chg{New=[elaborated], Old=[evaluated]} and any implicit initial values for
  the object or for its subcomponents are obtained as determined by
  the nominal subtype.@Chg{Version=[2],New=[
  @Defn2{Term=[initialization], Sec=(of an object)}
  @Defn2{Term=[assignment operation], Sec=(during elaboration of an @nt{object_declaration})}
  Any initial values (whether explicit or implicit) are assigned
  to the object or to the corresponding subcomponents.
  As described in @RefSecNum{Assignment Statements}
  and @RefSecNum{Assignment and Finalization},
  Initialize and Adjust procedures can be called.
  @IndexSee{Term=[constructor],See=[initialization]}],Old=[]}
  @begin(Discussion)
    For a per-object constraint that contains some per-object
    expressions and some non-per-object expressions,
    the values used for the constraint consist of the values
    of the non-per-object expressions evaluated at the point
    of the @nt{type_declaration}, and the values of the per-object
    expressions evaluated at the point of the creation of the
    object.

    The elaboration of per-object constraints was presumably performed
    as part of the dependent compatibility check in Ada 83.
    If the object is of a limited type
    with an access discriminant, the @nt<access_definition> is elaborated
    at this time (see @RefSecNum(Discriminants)).
  @end(Discussion)
  @begin{Reason}
    The reason we say that evaluating an explicit initialization
    expression happens before creating the object is that in some cases
    it is impossible to know the size of the object being created until
    its initial value is known, as in
    @lquotes@;X: String := Func_Call(...);@rquotes@;.
    The implementation can create the object early in the
    common case where the size can be known early,
    since this optimization is semantically neutral.
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00373-01]}
  @ChgDeleted{Version=[2],Text=[@Defn2{Term=[initialization], Sec=(of an object)}
  @Defn2{Term=[assignment operation], Sec=(during elaboration of an @nt{object_declaration})}
  Any initial values (whether explicit or implicit) are assigned
  to the object or to the corresponding subcomponents.
  As described in @RefSecNum{Assignment Statements}
  and @RefSecNum{Assignment and Finalization},
  Initialize and Adjust procedures can be called.
  @IndexSee{Term=[constructor],See=[initialization]}]}
  @begin(Ramification)
    Since the initial values have already been converted to the appropriate
    nominal subtype, the only Constraint_Errors that might
    occur as part of these assignments are for values outside their
    base range that are used to initialize unconstrained numeric
    subcomponents. See @RefSecNum{Scalar Types}.
  @end(Ramification)
@end(enumerate)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[]}@Comment{Just to get conditional Leading here.}
For the third step above, @Chg{Version=[2],New=[],Old=[the object creation and
any elaborations and ]}evaluations @Chg{Version=[2],New=[and assignments ],Old=[]}are
performed in an arbitrary
order@Chg{Version=[2],New=[ subject to the following restrictions:],Old=[,
except that if the @nt<default_expression> for a discriminant is
evaluated to obtain its initial value, then this evaluation
is performed before that of
the @nt<default_expression> for any component that depends on the
discriminant,
and also before that of any @nt<default_expression> that
includes the name of the discriminant.
The evaluations of the third step and the assignments of the fourth step
are performed in an arbitrary order,
except that each evaluation is performed before the resulting value is
assigned.]}@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@begin(Itemize)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00373-01]}
@ChgAdded{Version=[2],Text=[Assignment to any part of the object
is preceded by the evaluation of the value that is to be assigned.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Duh. But we ought to say it. Note that, like
any rule in the International Standard, it doesn't prevent
an @lquotes@;as-if@rquotes optimization; as long as the semantics as observed
from the program are correct, the compiler can generate any code it wants.]}
@end{Reason}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00373-01]}
@ChgAdded{Version=[2],Text=[The evaluation of a @nt{default_expression} that
includes the name of a discriminant is preceded by the assignment to that
discriminant.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Duh again. But we have to say this, too. It's
odd that Ada 95 only required the default expressions to be evaluated before
the discriminant is used; it says nothing about discriminant values that
come from @nt{subtype_indication}s.]}
@end{Reason}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00373-01]}
@ChgAdded{Version=[2],Text=[The evaluation of the @nt{default_expression}
for any component that depends on a discriminant is preceded by the assignment
to that discriminant.]}
@begin{Reason}
@Leading@keepnext@;For example:
@begin{Example}
@key[type] R(D : Integer := F) @key[is]
    @key[record]
        S : String(1..D) := (@key[others] => G);
    @key[end] @key[record];

X : R;
@end{Example}

For the elaboration of the declaration of X,
it is important that F be evaluated before the aggregate.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00373-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Text=[The assignments to any components, including
implicit components, not requiring late initialization
@Chg{Version=[3],New=[],Old=[must ]}precede the
initial value evaluations for any components requiring late initialization;
if two components both require late initialization, then assignments to parts
of the component occurring earlier in the order of the component declarations
@Chg{Version=[3],New=[],Old=[must ]}precede the
initial value evaluations of the component occurring later.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Components that require late
initialization can refer to the entire object during their initialization.
We want them to be initialized as late as possible to reduce the chance
that their initialization depends on uninitialized components. For instance:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{type} T (D : Natural) @key{is}
  @key{limited record}
    C1 : T1 (T'Access);
    C2 : Natural := F (D);
    C3 : String (1 .. D) := (others => ' ');
  @key{end record};]}
@end{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[Component C1 requires late initialization. The
initialization could depend on the values of any component of T, including D,
C2, or C3. Therefore, we want to it to be initialized last. Note that C2 and
C3 do not require late initialization; they only have to be initialized after
D.]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[It is possible for there to be more than one
component that requires late initialization. In this case, the language can't
prevent problems, because all of the components can't be the last one
initialized. In this case, we specify the order of initialization for
components requiring late initialization; by doing so, programmers can
arrange their code to avoid accessing uninitialized components, and such
arrangements are portable. Note that if the program accesses an uninitialized
component, @RefSecNum{Data Validity} defines the execution to be erroneous.]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0228-1]}
@Redundant[There is no implicit initial
value defined for a scalar subtype@Chg{Version=[3],New=[ unless the
Default_Value aspect has been specified for the type],Old=[]}.]
@PDefn{uninitialized variables}
In the absence of an explicit initialization@Chg{Version=[3],New=[ or the
specification of the Default_Value aspect],Old=[]}, a newly created
scalar object might have a value that does not belong to its subtype
(see @RefSecNum{Data Validity} and @RefSecNum{Pragma Normalize_Scalars}).
@begin{Honest}
It could even be represented by a bit pattern that doesn't
actually represent any value of the type at all,
such as an invalid internal code for an enumeration type,
or a NaN for a floating point type.
It is a generally a bounded error to reference scalar objects with
such @lquotes@;invalid representations@rquotes@;, as explained in
@RefSec{Data Validity}.
@end{Honest}
@begin{Ramification}
There is no requirement that two objects of the same scalar subtype have
the same implicit initial @lquotes@;value@rquotes@; (or representation).
It might even be the case that two elaborations of the same
@nt{object_declaration} produce two different initial values.
However, any particular uninitialized object is default-initialized to a
single value (or invalid representation).
Thus, multiple reads of such an uninitialized object will produce the
same value each time
(if the implementation chooses not to detect the error).
@end{Ramification}
@end{RunTime}

@begin{Notes}
Implicit initial values are not defined
for an indefinite subtype,
because if an object's nominal subtype is indefinite,
an explicit initial value is required.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0092-1],ARef=[AI05-0255-1]}
@Defn{stand-alone constant}
@Defn{stand-alone variable}
As indicated above,
a stand-alone object is an object declared by an @nt<object_declaration>.
Similar definitions apply to
@lquotes@;stand-alone constant@rquotes@; and @lquotes@;stand-alone variable.@rquotes@;
A subcomponent of an object is not a stand-alone object,
nor is an object that is created by an @nt<allocator>.
An object declared by a
@nt<loop_parameter_specification>,
@Chg{Version=[3],New=[@nt{iterator_specification}, ],Old=[]}@nt<parameter_specification>,
@nt<entry_index_specification>, @nt<choice_parameter_specification>,
@Chg{Version=[3],New=[@nt{extended_return_statement}, ],Old=[]}or
a @nt{formal_object_declaration} @Chg{Version=[3],New=[of mode @key[in out] ],Old=[]}is
not @Chg{Version=[3],New=[considered],Old=[called]} a stand-alone object.

The type of a stand-alone object cannot
be abstract (see @RefSecNum{Abstract Types and Subprograms}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Example of a multiple object declaration:)
@begin(Example)
--@RI[  the multiple object declaration ]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
John, Paul : @Chg{Version=[2],New=[@key{not null} ],Old=[]}Person_Name := @key(new) Person(Sex => M);  --@RI[  see @RefSecNum(Incomplete Type Declarations)]

--@RI[  is equivalent to the two single object declarations in the order given]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
John : @Chg{Version=[2],New=[@key{not null} ],Old=[]}Person_Name := @key(new) Person(Sex => M);
Paul : @Chg{Version=[2],New=[@key{not null} ],Old=[]}Person_Name := @key(new) Person(Sex => M);
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of variable declarations:)
@end{WideAbove}
@begin(Example)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
Count, Sum  : Integer;
Size        : Integer @key(range) 0 .. 10_000 := 0;
Sorted      : Boolean := False;
Color_Table : @key(array)(1 .. Max) @key(of) Color;
Option      : Bit_Vector(1 .. 10) := (@key(others) => True);
Hello       : @Chg{Version=[2],New=[@key(aliased)],Old=[@key(constant)]} String := "Hi, world.";@Chg{Version=[2],New=[
@unicode(952), @unicode(966)        : Float @b<range> -@pi .. +@pi;],Old=[]}
@end(Example)

@begin{WideAbove}
@leading@keepnext@i(Examples of constant declarations:)
@end{WideAbove}
@begin(Example)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
Limit     : @key(constant) Integer := 10_000;
Low_Limit : @key(constant) Integer := Limit/10;
Tolerance : @key(constant) Real := Dispersion(1.15);@Chg{Version=[2],New=[
Hello_Msg : @key(constant access) String := Hello'Access; --@RI[ see @RefSecNum{Operations of Access Types}]],Old=[]}
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax rule for @nt{object_declaration} is modified to allow the
@key{aliased} reserved word.

A variable declared by an @nt<object_declaration> can be constrained
by its initial value; that is, a variable of a nominally unconstrained
array subtype, or discriminated type without defaults, can
be declared so long as it has an explicit initial value.
In Ada 83, this was permitted for constants, and for variables
created by allocators, but not for variables declared by
@nt<object_declaration>s. This is particularly important
for tagged class-wide types, since there is no way to constrain
them explicitly, and so an initial value is the only way
to provide a constraint. It is also important for generic formal
private types with unknown discriminants.

We now allow an @nt{unconstrained_array_definition}
in an @nt{object_declaration}.
This allows an object of an anonymous array type to have its
bounds determined by its initial value.
This is for uniformity: If one can write @lquotes@;X: @key[constant]
@key[array](Integer @key[range] 1..10) @key[of] Integer := ...;@rquotes@; then
it makes sense to also allow
@lquotes@;X: @key[constant] @key[array](Integer @key[range] <>) @key[of] Integer := ...;@rquotes@;.
(Note that if anonymous array types are
ever sensible, a common situation is for a table implemented as an array.
Tables are often constant, and for constants, there's usually no point in
forcing the user to count the number of elements in the value.)
@end{Extend83}

@begin{DiffWord83}
We have moved the syntax for @nt{object_declaration}s into this subclause.

Deferred constants no longer have a separate syntax rule, but rather
are incorporated in @nt<object_declaration> as constants declared
without an initialization expression.
@end{DiffWord83}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  Unconstrained aliased objects of types
  with discriminants with defaults are no longer
  constrained by their initial values. This means that a program that
  raised Constraint_Error from an attempt to change the discriminants
  will no longer do so. The change only affects programs that depended
  on the raising of Constraint_Error in this case, so the inconsistency
  is unlikely to occur outside of the ACATS. This change may however cause
  compilers to implement these objects differently, possibly taking additional
  memory or time. This is unlikely to be worse than the differences caused by
  any major compiler upgrade.]}
@end{Inconsistent95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  A constant may have a limited type; the initialization @nt{expression}
  has to be built-in-place (see @RefSecNum{Limited Types}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00385-01],ARef=[AI95-00406-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  A stand-alone object may have an anonymous access type.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0002],ARef=[AI95-00171-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected wording to say that
  per-object constraints are elaborated (not evaluated).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00373-01]}
  @ChgAdded{Version=[2],Text=[The rules for evaluating default initialization
  have been tightened. In particular, components whose default initialization
  can refer to the rest of the object are required to be initialized last.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00433-01]}
  @ChgAdded{Version=[2],Text=[Added examples of various new constructs.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in an @nt{object_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0228-1]}
  @ChgAdded{Version=[3],Text=[Implicit initial values can now be given
  for scalar types and for scalar array components, using the Default_Value
  (see @RefSecNum{Scalar Types}) and Default_Component_Value
  (see @RefSecNum{Array Types}) aspects; the extension is documented there.]}
@end{DiffWord2005}



@ISOOnlyRMNewPageVer{Version=[3]}@Comment{For ISO version of Ada 2012 Standard}
@LabeledSubClause{Number Declarations}

@begin{Intro}
A @nt<number_declaration> declares a named number.
@begin{Discussion}
@Defn{static}
If a value or other property of a construct is required to be
@i(static) that means it is required to be determined prior
to execution. A @i(static) expression is an expression
whose value is computed
at compile time and is usable in contexts where the actual value
might affect the legality of the construct.
This is fully defined in clause
@RefSecNum(Static Expressions and Static Subtypes).
@end{Discussion}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<number_declaration>,rhs="
     @Syn2{defining_identifier_list} : @key{constant} := @SynI{static_}@Syn2{expression};"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(number_declaration expression)}
The @SynI(static_)@nt{expression} given for
a @nt{number_declaration} is expected to be of any numeric type.
@end{Resolution}

@begin{Legality}
The @i(static_)@nt{expression} given for a number declaration
shall be a static expression, as defined by clause
@RefSecNum(Static Expressions and Static Subtypes).
@end{Legality}

@begin{StaticSem}
The named number denotes a value of type @i(universal_integer) if
the type of the @i(static_)@!@nt{expression} is an integer type.
The named number denotes a value of type @i(universal_real) if
the type of the @i(static_)@!@nt{expression} is a real type.

The value denoted by the named number is the value of the
@i(static_)@nt{expression}, converted to the corresponding
universal type.
@PDefn2{Term=[implicit subtype conversion],Sec=(named number value)}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(number_declaration)}
The elaboration of a @nt<number_declaration> has no effect.
@begin(TheProof)
  Since the @i(static_)@nt<expression> was evaluated at compile time.
@end(TheProof)
@end{RunTime}

@begin{Examples}
@Leading@keepnext@i(Examples of number declarations:)
@begin(Example)
Two_Pi        : @key(constant) := 2.0*Ada.Numerics.Pi;   --@RI[ a real number (see @RefSecNum{The Numerics Packages})]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00433-01]}
Max           : @key(constant) := 500;                   --@RI[ an integer number]
Max_Line_Size : @key(constant) := Max/6@Chg{Version=[2],New=[;],Old=[ ]}                 --@RI[ the integer 83]
Power_16      : @key(constant) := 2**16;                 --@RI[ the integer 65_536]
One, Un, Eins : @key(constant) := 1;                     --@RI[ three different names for 1]
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
We now allow a static expression of any numeric type
to initialize a named
number. For integer types, it was possible in Ada 83
to use 'Pos to define a named number, but there was
no way to use a static expression of some nonuniversal
real type to define a named number. This change is
upward compatible because of the preference rule for
the operators of the root numeric types.
@end{Extend83}

@begin{DiffWord83}
We have moved the syntax rule into this subclause.

AI83-00263 describes the elaboration of a number declaration
in words similar to that of an @nt{object_declaration}. However, since
there is no expression to be evaluated and no object to be created,
it seems simpler to say that the elaboration has no effect.
@end{DiffWord83}


@LabeledClause{Derived Types and Classes}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01],ARef=[AI95-00419-01]}
@Defn{derived type}
A @nt<derived_type_definition> defines a @Chg{Version=[2],
New=[@i{derived type}],Old=[new type]} (and its first subtype)
whose characteristics are @i(derived) from those of a
@Chg{Version=[2],New=[parent type, and possibly from progenitor types],
Old=[@i(parent type)]}.
@IndexSee{Term=[inheritance],See=[derived types and classes]}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Derived type>,
  Text=<A derived type is a type defined in terms of @Chg{Version=[2],
  New=[one or more other types given in a derived type definition. The
  first of those types],Old=[another type, which]}
  is the parent type of the derived type@Chg{Version=[2],New=[ and
  any others are progenitor types],Old=[]}.
  Each class containing the parent type @Chg{Version=[2],New=[or a progenitor
  type ],Old=[]}also contains the derived type.
  The derived type inherits properties such as components and
  primitive operations from the parent@Chg{Version=[2],New=[ and progenitors],Old=[]}.
  A type together with the types derived from it
  (directly or indirectly) form a derivation class.>}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00442-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[class],Sec=[of types]}
@Defn2{Term=[category],Sec=[of types]}
A @i<class of types> is a set of types that is closed under
derivation; that is, if the parent or a progenitor type of a
derived type belongs to a class, then so does the derived type.
By saying that a particular group of types forms a class,
we are saying that all derivatives of a type in the set inherit
the characteristics that define that set. The
more general term @i<category of types> is used for a set of types
whose defining characteristics are not necessarily inherited by
derivatives; for example, limited, abstract, and interface are all
categories of types, but not classes of types.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A class of types is also a category of types.]}
@end{Ramification}

@end{Intro}

@begin{Syntax}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00419-01]}
@Syn{lhs=<derived_type_definition>,rhs="@Chg{Version=[2],New=<
    >,Old=<>}[@key{abstract}] @Chg{Version=[2],New=<[@key{limited}] >,Old=<>}@key{new} @SynI{parent_}@Syn2{subtype_indication} [@Chg{Version=[2],New=<[@key{and} @Syn2{interface_list}] >,Old=<>}@Syn2{record_extension_part}]"}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00401-01],ARef=[AI95-00419-01]}
@Defn{parent subtype}
@Defn{parent type}
The @Syni(parent_)@nt<subtype_indication> defines the @i(parent subtype);
its type is the @Chg{Version=[2],New=[@i(parent type)],Old=[parent type]}.
@Chg{Version=[2],New=[The @nt{interface_list} defines the progenitor types
(see @RefSecNum{Interface Types}). A derived type
has one parent type and zero or more progenitor types.],Old=[]}

@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<Parent>,
  Text=<@ChgAdded{Version=[2],Text=[The parent of a derived type is the
  first type given in the definition of the derived type. The parent
  can be almost any kind of type, including an interface type.]}>}


A type shall be completely defined
(see @RefSecNum(Completions of Declarations))
prior to being specified as the parent type
in a @nt<derived_type_definition> @em @Redundant[the
@nt<full_type_declaration>s for the parent
type and any of its subcomponents have to
precede the @nt<derived_type_definition>.]
@begin{Discussion}
  This restriction does not apply to the ancestor type of a private
  extension @em see @RefSecNum(Private Types and Private Extensions);
  such a type need not be completely defined prior to the
  @nt<private_extension_declaration>. However, the restriction
  does apply to record extensions, so the ancestor type will
  have to be completely defined prior to the @nt<full_type_declaration>
  corresponding to the @nt<private_extension_declaration>.
@end{Discussion}
@begin{Reason}
  We originally hoped we could relax this restriction.
  However, we found it too complex to specify the rules for
  a type derived from an incompletely defined limited type that
  subsequently became nonlimited.
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
@Defn{record extension}
If there is a @nt<record_extension_part>, the derived type is
called a @i(record extension) of the parent type.
A @nt<record_extension_part> shall be provided if and only if
the parent type is a tagged type.@Chg{Version=[2],New=[ @Redundant[An
@nt{interface_list} shall be provided only if
the parent type is a tagged type.]],Old=[]}
@begin(TheProof)
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00401-01]}
  @ChgAdded{Version=[2],Text=[The syntax only allows an @nt{interface_list}
  to appear with a @nt{record_extension_part}, and a @nt{record_extension_part}
  can only be provided if the parent type is a tagged type. We give the last
  sentence anyway for completeness.]}
@end(TheProof)
@begin(ImplNote)
  We allow a record extension to inherit discriminants;
  an early version of Ada 9X did not.
  If the parent subtype is unconstrained, it can be implemented
  as though its discriminants were repeated in a new
  @nt{known_discriminant_part} and then used to constrain the old ones
  one-for-one.
  However, in an extension aggregate, the discriminants in this case
  do not appear in the component association list.
@end(ImplNote)
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[]}@Comment{Conditional leading}
  This rule needs to be rechecked in the visible
  part of an instance of a generic unit@Chg{Version=[2],New=[ because of the
  @lquotes@;only if@rquotes@; part of the rule. For example:],Old=[]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{generic}
   @key{type} T @key{is private};
@key{package} P @key{is}
   @key{type} Der @key{is new} T;
@key{end} P;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} I @key{is new} P (Some_Tagged_Type); -- @RI[illegal]]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00114-01]}
  @ChgAdded{Version=[2],Text=[The instantiation is illegal because a tagged
  type is being extended in the visible part without a
  @nt{record_extension_part}. Note that this is legal in the private part or
  body of an instance, both to avoid a contract model violation, and because
  no code that can see that the type is actually tagged can also see the
  derived type declaration.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[No recheck is needed for derived types with
  a @nt{record_extension_part}, as that has to be derived from something that
  is known to be tagged (otherwise the template is illegal).]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00419-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0096-1]}
@ChgAdded{Version=[2],Text=[If the reserved word @key{limited} appears in a
@nt{derived_type_definition}, the parent type shall be a limited
type.@Chg{Version=[3],New=[ If the parent type is a tagged formal type,
then in addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}), this rule applies also in
the private part of an instance of a
generic unit.@PDefn{generic contract issue}],Old=[]}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[We allow @key{limited} because we don't inherit
  limitedness from interfaces, so we must have a way to derive a limited type
  from interfaces. The word @key{limited} has to be legal when the parent
  @i{could be} an interface, and that includes generic formal abstract types.
  Since we have to allow it in this case, we might as well allow it everywhere
  as documentation, to make it explicit that the type is limited.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[However, we do not want to allow @key{limited}
  when the parent is nonlimited: limitedness cannot change in a derivation
  tree.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If the parent type is an untagged limited formal
  type with an actual type that is nonlimited, we allow derivation as a limited
  type in the private part or body as no place could have visibility on the
  resulting type where it was known to be nonlimited (outside of the instance).
  (See the previous paragraph's annotations for an explanation of this.)
  However, if the parent type is a tagged limited formal type with an actual
  type that is nonlimited, it would be possible to pass a value of the limited
  type extension to a class-wide type of the parent, which would be nonlimited.
  That's too weird to allow (even though all of the extension components would
  have to be nonlimited because the rules of @RefSecNum{Type Extensions} are
  rechecked), so we have a special rule to prevent that in the private part
  (type extension from a formal type is illegal in a generic package body).]}
@end{Reason}
@end{Legality}

@begin{StaticSem}
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
The first subtype of the derived type is
unconstrained if a @nt{known_discriminant_part}
is provided in the declaration of the derived type, or if the
parent subtype is unconstrained.
@Defn{corresponding constraint}
Otherwise, the constraint
of the first subtype @i(corresponds) to that of the parent subtype
in the following sense: it is the same as that of the parent subtype
except that for a range constraint (implicit or explicit), the value of
each bound of its range is replaced by the corresponding value
of the derived type.
@begin(Discussion)
  A @nt<digits_constraint> in a @nt<subtype_indication> for
  a decimal fixed point subtype always imposes a range constraint,
  implicitly if there is no explicit one given.
  See @RefSec(Fixed Point Types).
@end(Discussion)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
@ChgAdded{Version=[2],Text=[The first subtype of the derived type excludes null
(see @RefSecNum{Access Types}) if and only if the parent subtype excludes null.]}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0110-1]}
@Leading@keepnext@;The @Chg{Version=[3],New=[@i{characteristics}@PDefn{characteristics}
and implicitly declared primitive subprograms],Old=[characteristics]} of the
derived type are defined as follows:
@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0110-1]}
  @ChgAdded{Version=[3],Text=[The characteristics of a type do not include its
  primitive subprograms (primitive subprograms include predefined operators).
  The rules governing availability/visibility and inheritance of characteristics
  are separate from those for primitive subprograms.]}
@end{Ramification}

@begin(itemize)
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00401-01],ARef=[AI95-00442-01]}
@Chg{Version=[2],New=[@Redundant[If the parent type or a progenitor type
belongs to a class of types, then the derived type also belongs to that class.]
The following sets of types, as well as any higher-level sets composed from
them, are classes in this sense@Redundant[, and hence the characteristics
defining these classes are inherited by derived types from their parent or
progenitor types]: signed integer, modular integer, ordinary fixed, decimal
fixed, floating point, enumeration, boolean, character, access-to-constant,
general access-to-variable, pool-specific access-to-variable,
access-to-subprogram, array, string, non-array composite, nonlimited, untagged
record, tagged, task, protected, and synchronized tagged],
Old=[Each class of types that includes the parent type also includes
the derived type]}.
@begin{Discussion}
  This is inherent in our notion
  of a @lquotes@;class@rquotes@; of types. It is not mentioned in the
  initial definition of @lquotes@;class@rquotes@; since at that point
  type derivation has not been defined. In any case, this rule
  ensures that every class of types is closed under
  derivation.
@end{Discussion}

If the parent type is an elementary type or an array type, then the
set of possible values of the derived type
is a copy of the set of possible values of the parent type.
For a scalar type, the base range of the derived type is the same
as that of the parent type.
@begin{Discussion}
  The base range of a type defined by an @nt<integer_type_definition>
  or a @nt<real_type_definition> is determined by the @ntf<_definition>,
  and is not necessarily the same as that of the corresponding
  root numeric type from which the newly defined type is implicitly
  derived. Treating numerics types as implicitly derived from one of
  the two root numeric types is simply to link them into a type hierarchy;
  such an implicit derivation does not follow all the rules given here
  for an explicit @nt<derived_type_definition>.
@end{Discussion}

If the parent type is a composite type other than an array type,
then the components,
protected subprograms, and entries
that are declared for the derived type are as follows:
@begin(inneritemize)
  The discriminants specified by a new @nt{known_discriminant_part},
  if there is one;
  otherwise, each discriminant of the parent type
  (implicitly declared in the same order with the same specifications) @em
  @Defn{inherited discriminant}
  @Defn{inherited component}
  in the latter case, the discriminants are said to be @i(inherited),
  or if unknown in the parent, are also unknown in the derived type;

  Each nondiscriminant component, entry, and protected subprogram
  of the parent type, implicitly declared in the same order
  with the same declarations;
  @Defn{inherited component}
  @Defn{inherited protected subprogram}
  @Defn{inherited entry}
  these components, entries, and protected subprograms are said to be @i(inherited);
  @begin{Ramification}

    The profiles of entries and protected subprograms do
    not change upon type derivation, although the type of the
    @lquotes@;implicit@rquotes@; parameter identified by the @nt<prefix> of
    the @nt<name> in a call does.@end{ramification}
  @begin{Honest}

  Any name in the parent @nt{type_declaration} that denotes the
  current instance of the type is replaced with a name denoting the
  current instance of the derived type, converted to the parent type.

  @end{Honest}

  Each component declared in a
  @nt<record_extension_part>, if any.
@end(inneritemize)

@noprefix@;Declarations of components, protected subprograms,
and entries, whether implicit or explicit,
occur immediately within the declarative region of
the type, in the order indicated above,
following the parent @nt<subtype_indication>.
@begin(Discussion)
  The order of declarations within the region matters
  for @nt{record_aggregate}s and @nt<extension_aggregate>s.
@end(Discussion)
@begin{Ramification}
  In most cases, these things are implicitly declared
  @i{immediately} following the parent @nt<subtype_indication>.
  However, @RefSec{Private Operations} defines some cases in which
  they are implicitly declared later, and some cases in which
  the are not declared at all.
@end{Ramification}
@begin{Discussion}
  The place of the implicit declarations of inherited components matters
  for visibility @em they are not visible in the
  @nt<known_discriminant_part> nor in the parent @nt<subtype_indication>,
  but are usually visible within the
  @nt<record_extension_part>, if any
  (although there are restrictions on their use).
  Note that a discriminant specified in a new
  @nt<known_discriminant_part> is
  not considered @lquotes@;inherited@rquotes@; even if it has the same name
  and subtype as a discriminant of the parent type.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00419-01]}
@ChgDeleted{Version=[2],Text=[The derived type is limited if and
only if the parent type is limited.]}
@ChgNote{This rule is normatively in @RefSecNum{Limited Types}, and we don't
want it scattered everywhere.}
@begin{Honest}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00419-01]}
  @ChgDeleted{Version=[2],Text=[The derived type can become nonlimited if
  the derivation takes place in the visible part of a child package,
  and the parent type is nonlimited as viewed from the
  private part of the child package @em see @RefSecNum(Limited Types).]}
@end{Honest}

@Redundant[For each predefined operator of the parent type,
there is a corresponding predefined operator of the derived type.]
@begin(TheProof)
  This is a ramification of the fact that each class that includes
  the parent type also includes the derived type,
  and the fact that the set of predefined operators that is defined for
  a type, as described in
  @RefSecNum(Operators and Expression Evaluation), is determined by
  the classes to which it belongs.
@end(TheProof)
@begin(Reason)
  Predefined operators are handled separately because
  they follow a slightly different rule
  than user-defined primitive subprograms. In particular
  the systematic replacement described below does not apply fully to the
  relational operators for Boolean and the exponentiation
  operator for Integer. The relational operators for a type
  derived from Boolean still return Standard.Boolean. The
  exponentiation operator for a type derived from Integer
  still expects Standard.Integer for the right operand.
  In addition, predefined operators "reemerge" when a type
  is the actual type corresponding to a generic formal type,
  so they need to be well defined even if hidden by user-defined
  primitive subprograms.
@end(Reason)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
@Defn{inherited subprogram}
For each user-defined primitive subprogram (other than a user-defined
equality operator @em see below) of the parent type@Chg{Version=[2],New=[ or of a
progenitor type],Old=[]} that already exists at the place of the
@nt{derived_type_definition}, there exists a corresponding @i(inherited)
primitive subprogram of the derived type
with the same defining name.
@Defn2{Term=[equality operator],Sec=(special inheritance rule for tagged types)}
Primitive user-defined equality operators of the parent
type@Chg{Version=[2],New=[ and any progenitor types],Old=[]} are also
inherited by the derived type, except when
the derived type is a nonlimited record extension, and
the inherited operator would have a profile that is type
conformant with the profile of the corresponding
predefined equality operator; in this case, the user-defined
equality operator is not inherited, but is rather incorporated into
the implementation of the predefined equality operator of the record extension
(see @RefSecNum(Relational Operators and Membership Tests)).
@PDefn{type conformance}
@begin{Ramification}
  We say @lquotes@;...already exists...@rquotes@; rather than @lquotes@;is visible@rquotes@; or @lquotes@;has
  been declared@rquotes@; because there are certain operations that are declared
  later, but still exist at the place of the
  @nt{derived_type_definition},
  and there are operations that are never declared, but still exist.
  These cases are explained in @RefSecNum{Private Operations}.

  Note that nonprivate extensions can appear only after the last
  primitive subprogram of the parent @em the freezing rules ensure this.
@end{Ramification}
@begin{Reason}
  A special case is made for the equality operators on nonlimited
  record extensions
  because their predefined equality operators are already defined in terms
  of the primitive equality operator of their parent type (and of the
  tagged components of the extension part). Inheriting the parent's
  equality operator as is would be undesirable, because it would ignore
  any components of the extension part.
  On the other hand, if the parent type is limited, then any user-defined
  equality operator is inherited as is, since there is no predefined
  equality operator to take its place.
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  Because user-defined equality operators are not inherited
  by @Chg{Version=[2],New=[nonlimited ],Old=[]}record extensions, the
  formal parameter names of = and /=
  revert to Left and Right, even if different formal parameter names
  were used in the user-defined equality operators of the parent type.
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00401-01]}
  @ChgAdded{Version=[2],Text=[This rule only describes what operations
  are inherited; the rules that describe what happens when there are
  conflicting inherited subprograms are found in @RefSecNum{Visibility}.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0164-1],ARef=[AI05-0240-1]}
@noprefix@;The profile of an inherited subprogram
(including an inherited enumeration literal) is obtained
from the profile of the corresponding
(user-defined) primitive subprogram of the parent@Chg{Version=[2],New=[ or progenitor],Old=[]}
type, after systematic replacement of each
subtype of its profile (see
@RefSecNum{Subprogram Declarations})
that is of the parent@Chg{Version=[2],New=[ or progenitor],Old=[]}
type@Chg{Version=[3],New=[, other than those subtypes
found in the designated profile of an @nt{access_definition},],Old=[]}
with a @i(corresponding subtype) of the derived type.
@Defn{corresponding subtype}
For a given subtype of the parent@Chg{Version=[2],New=[ or progenitor],Old=[]} type,
the corresponding subtype of the derived type is defined as follows:
@begin(inneritemize)
  If the declaration of the derived type has neither a
  @nt<known_discriminant_part> nor a @nt<record_extension_part>,
  then the corresponding subtype
  has a constraint that corresponds (as defined above for the first
  subtype of the derived type) to that of the given subtype.

  If the derived type is a record extension, then the
  corresponding subtype is the first subtype of the derived type.

  If the derived type has a new @nt<known_discriminant_part>
  but is not a record extension,
  then the corresponding subtype is constrained
  to those values that when converted to the parent type belong to
  the given subtype (see @RefSecNum(Type Conversions)).
  @PDefn2{Term=[implicit subtype conversion],Sec=(derived type discriminants)}
  @begin{Reason}
    An inherited subprogram of an untagged type has an Intrinsic
    calling convention, which precludes the use of the Access
    attribute.
    We preclude 'Access because correctly performing
    all required constraint checks on an indirect call to such
    an inherited subprogram was felt to impose an undesirable
    implementation burden.

    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0164-1]}
    @ChgAdded{Version=[3],Text=[Note that the exception to substitution of the
    parent or progenitor type applies only in the profiles of anonymous
    access-to-subprogram types. The exception is necessary to avoid calling
    an access-to-subprogram with types and/or constraints different than
    expected by the actual routine.]}
  @end{Reason}
@end(inneritemize)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
@noprefix@;The same formal parameters have @nt<default_expression>s in
the profile of the inherited subprogram. @Redundant[Any type mismatch due
to the systematic replacement of the parent@Chg{Version=[2],New=[ or progenitor],Old=[]}
type by the derived type is handled as part of the normal
type conversion associated with parameter
passing @em see @RefSecNum(Parameter Associations).]
@begin(Reason)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
  We don't introduce the type conversion explicitly here
  since conversions to record extensions or on access parameters
  are not generally legal. Furthermore, any type conversion would
  just be "undone" since the @Chg{Version=[2],New=[],Old=[parent's ]}subprogram
  @Chg{Version=[2],New=[of the parent or progenitor ],Old=[]}is ultimately being
  called anyway.@Chg{Version=[2],New=[ (Null procedures can be inherited from
  a progenitor without being overridden, so it is possible to call subprograms
  of an interface.)],Old=[]}
@end(Reason)

@end(itemize)  @Comment{end of characteristics of derived type}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
If a primitive subprogram of the parent@Chg{Version=[2],New=[ or progenitor],
Old=[]} type is visible at the
place of the @nt{derived_type_definition},
then the corresponding inherited subprogram is implicitly declared
immediately after the @nt{derived_type_definition}.
Otherwise, the inherited subprogram is implicitly declared later
or not at all,
as explained in @RefSecNum{Private Operations}.

@PDefn{derived type}
A derived type can also be defined by a @nt<private_@!extension_@!declaration>
(see @RefSecNum(Private Types and Private Extensions))
or a @nt<formal_@!derived_@!type_@!definition>
(see @RefSecNum(Formal Private and Derived Types)).
Such a derived type is a partial
view of the corresponding full or actual type.

All numeric types are derived types, in that
they are implicitly derived from a corresponding
root numeric type (see @RefSecNum(Integer Types) and @RefSecNum(Real Types)).

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(derived_type_definition)}
The elaboration of a @nt<derived_type_definition>
creates the derived type and its first subtype,
and consists of the elaboration of the @nt<subtype_@!indication>
and the @nt<record_@!extension_@!part>, if any.
If the @nt{subtype_@!indication} depends on a discriminant,
then only those expressions that do not depend on a discriminant
are evaluated.
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[We don't mention the @nt{interface_list}, because
  it does not need elaboration (see @RefSecNum{Interface Types}). This is
  consistent with the handling of @nt{discriminant_part}s, which aren't
  elaborated either.]}
@end{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00391-01],ARef=[AI95-00401-01]}
@PDefn2{Term=[execution], Sec=(call on an inherited subprogram)}
For the execution of a call on an inherited subprogram,
a call on the corresponding primitive subprogram of the
parent@Chg{Version=[2],New=[ or progenitor],Old=[]} type is
performed; the normal conversion of each actual parameter
to the subtype of the corresponding formal parameter
(see @RefSecNum(Parameter Associations))
performs any necessary type conversion as well.
If the result type of the inherited subprogram
is the derived type, the result of calling the
@Chg{Version=[2],New=[],Old=[parent's ]}subprogram@Chg{Version=[2],
New=[ of the parent or progenitor],Old=[]} is converted to the
derived type@Chg{Version=[2],New=[, or in the case of a
null extension, extended to the derived type using the equivalent of an
@nt{extension_aggregate} with the original result as the @nt{ancestor_part}
and @key{null record} as the @nt{record_component_association_list}],Old=[]}.
@PDefn2{Term=[implicit subtype conversion],Sec=(result of inherited function)}
@begin(Discussion)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00391-01]}
  If an inherited function returns the derived type, and the type
  is a @Chg{Version=[2],New=[nonnull ],Old=[]}record extension, then the
  inherited function @Chg{Version=[2],New=[shall be overridden, unless
  the type is abstract (in which case the function ],Old=[]}is abstract,
  and (unless overridden) cannot be called except via
  a dispatching call@Chg{Version=[2],New=[)],Old=[]}.
  See @RefSecNum(Abstract Types and Subprograms).
@end(Discussion)
@end{RunTime}

@begin{Notes}
@Defn{closed under derivation}
Classes are closed under derivation @em
any class that contains a type also contains its derivatives.
Operations available for a given class of types
are available for the derived types in that class.

Evaluating an inherited enumeration literal
is equivalent to evaluating the corresponding enumeration literal
of the parent type, and then converting the result to
the derived type. This follows from their equivalence to
parameterless functions.
@PDefn2{Term=[implicit subtype conversion],Sec=(inherited enumeration literal)}

A generic subprogram is not a subprogram,
and hence cannot be a primitive subprogram and
cannot be inherited by a derived type. On the other hand,
an instance of a generic subprogram can be a primitive subprogram,
and hence can be inherited.

If the parent type is an access type, then the parent
and the derived type share the same storage pool;
there is a @key{null} access value for the derived type
and it is the implicit initial value for the type.
See @RefSecNum(Access Types).

If the parent type is a boolean type, the predefined relational operators
of the derived type deliver a result of the predefined type Boolean
(see @RefSecNum(Relational Operators and Membership Tests)).
If the parent type is an integer type, the right operand of the
predefined exponentiation operator is of the predefined type Integer
(see @RefSecNum(Highest Precedence Operators)).

Any discriminants of the parent type are either all inherited, or
completely replaced with a new set of discriminants.

For an inherited subprogram, the subtype of a formal parameter
of the derived type need not have any value in common with the first
subtype of the derived type.
@begin(TheProof)
  @Leading@;This happens when the parent subtype is constrained to a range
  that does not overlap with the range of a subtype of the parent
  type that appears in the profile of
  some primitive subprogram of the parent type.
  For example:
@begin(example)
@key(type) T1 @key(is range) 1..100;
@key(subtype) S1 @key(is) T1 @key(range) 1..10;
@key(procedure) P(X : @key[in] S1);  @RI{-- P is a primitive subprogram}
@key(type) T2 @key(is new) T1 @key(range) 11..20;
@RI(-- implicitly declared:)
@RI{-- @key(procedure) P(X : @key[in] T2'Base @key(range) 1..10);}
@RI{--      X cannot be in T2'First .. T2'Last}
@end(example)
@end(TheProof)

If the reserved word @key{abstract} is given in the declaration of a
type, the type is abstract (see @RefSecNum{Abstract Types and Subprograms}).

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01],ARef=[AI95-00401-01]}
@ChgAdded{Version=[2],Text=[An interface type that has a progenitor type
@lquotes@;is derived from@rquotes@; that type.
A @nt{derived_type_definition}, however, never defines an interface type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00345-01]}
@ChgAdded{Version=[2],Text=[It is illegal for the parent type of a
@nt{derived_type_definition} to be a synchronized tagged type.]}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[@RefSecNum{Type extensions} prohibits record
  extensions whose parent type is a synchronized tagged type, and this clause
  requires tagged types to have a record extension. Thus there are no legal
  derivations. Note that a synchronized interface can be used as a progenitor
  in an @nt{interface_type_definition} as well as in task and protected types,
  but we do not allow concrete extensions of any synchronized tagged type.]}
@end{TheProof}

@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of derived type declarations:)
@begin(Example)
@key(type) Local_Coordinate @key(is) @key(new) Coordinate;   --@RI[  two different types]
@key(type) Midweek @key(is) @key(new) Day @key(range) Tue .. Thu;  --@RI[  see @RefSecNum(Enumeration Types)]
@key(type) Counter @key(is) @key(new) Positive;              --@RI[  same range as Positive ]

@key(type) Special_Key @key(is) @key(new) Key_Manager.Key;   --@RI[  see @RefSecNum(Private Operations)]
  --@RI[ the inherited subprograms have the following specifications: ]
  --@RI[         procedure Get_Key(K : out Special_Key);]
  --@RI[         function "<"(X,Y : Special_Key) return Boolean;]
@end(Example)
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
When deriving from a (nonprivate, nonderived) type in the same
visible part in which it is defined, if a predefined
operator had been overridden prior to the derivation,
the derived type will inherit the user-defined operator rather
than the predefined operator. The work-around (if the new behavior
is not the desired behavior) is to move the definition of the
derived type prior to the overriding of any predefined operators.

@end{Inconsistent83}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
@Leading@;When deriving from a (nonprivate, nonderived) type in the same
visible part in which it is defined, a primitive subprogram of the
parent type declared before the derived type will be inherited by the
derived type. This can cause upward incompatibilities in cases like
this:
@begin{Example}
   @key[package] P @key[is]
      @key[type] T @key[is] (A, B, C, D);
      @key[function] F( X : T := A ) @key[return] Integer;
      @key[type] NT @key[is] @key[new] T;
      --@RI{ inherits F as}
      --@RI{ function F( X : NT := A ) return Integer;}
      --@RI{ in Ada 95 only}
      ...
   @key[end] P;
   ...
   @key[use] P;  --@RI{ Only one declaration of F from P is use-visible in}
           --@RI{ Ada 83;  two declarations of F are use-visible in}
           --@RI{ Ada 95.}
@key[begin]
   ...
   @key[if] F > 1 @key[then] ... --@RI{ legal in Ada 83, ambiguous in Ada 95}
@end{Example}
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax for a @nt{derived_type_definition} is amended to
include an optional @nt{record_extension_part}
(see @RefSecNum(Type Extensions)).

A derived type may override the discriminants of the parent by giving a
new @nt{discriminant_part}.

The parent type in a @nt<derived_type_definition>
may be a derived type defined
in the same visible part.

When deriving from a type in the same visible part in which it is defined,
the primitive subprograms declared prior to the derivation
are inherited as primitive subprograms of the derived type.
See @RefSecNum(Classification of Operations).
@end{Extend83}

@begin{DiffWord83}
We now talk about the classes to which a type belongs, rather than
a single class.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0190-1]}@ChgNote{Irrelevant here,
and broken since 7.6.1 defined "collection"}
@ChgDeleted{Version=[3],Text=[As explained in Section 13, the concept of
"storage pool" replaces the Ada 83 concept of "collection." These concepts are
similar, but not the same.]}
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00401-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  A derived type may inherit from multiple (interface) progenitors,
  as well as the parent type @em see @RefSec{Interface Types}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[A derived type may specify that it is a limited
  type. This is required for interface ancestors (from which limitedness is
  not inherited), but it is generally useful as documentation of limitedness.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00391-01]}
  @ChgAdded{Version=[2],Text=[Defined the result of functions for
  null extensions (which we no longer require to be overridden - see
  @RefSecNum{Abstract Types and Subprograms}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00442-01]}
  @ChgAdded{Version=[2],Text=[Defined the term @lquotes@;category of
  types@rquotes and used it in wording elsewhere; also specified the
  language-defined categories that form classes of types (this was never
  normatively specified in Ada 95).]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0096-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added a (re)check that limited type extensions never are derived from
  nonlimited types in generic private parts. This is disallowed as it would
  make it possible to pass a limited object to a nonlimited class-wide type,
  which could then be copied. This is only possible using Ada 2005 syntax,
  so examples in existing programs should be rare.]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0110-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to clarify that
  the characteristics of derived types are formally defined here. (This is the
  only place in the Standard that actually spells out what sorts of things
  are actually characteristics, which is rather important.)]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0164-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added wording to ensure that
  anonymous access-to-subprogram types don't get modified on derivation.]}
@end{DiffWord2005}


@LabeledSubClause{Derivation Classes}

@begin{Intro}
In addition to the various language-defined classes of types,
types can be grouped into @i(derivation classes).
@end{Intro}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00401-01]}
@Defn2{Term=[derived from], Sec=(directly or indirectly)}
A derived type is @i(derived from) its parent type @i(directly);
it is derived
@i(indirectly) from any type from which its parent type is derived.@Chg{Version=[2],
New=[ A derived type, interface type, type extension, task type, protected type,
or formal derived type is also derived from every ancestor of each of its
progenitor types, if any.],Old=[]}
@Defn2{Term=[derivation class], Sec=(for a type)}
@Defn2{Term=[root type], Sec=(of a class)}
@Defn{rooted at a type}
The derivation class of types for a type @i(T) (also called
the class @i(rooted) at @i(T)) is
the set consisting of @i(T) (the @i(root type) of the class)
and all types derived from @i(T) (directly or indirectly) plus
any associated universal or class-wide types (defined below).
@begin{Discussion}
  Note that the definition of @lquotes@;derived from@rquotes@; is a recursive
  definition.
  We don't define a root type for all interesting
  language-defined classes, though presumably we could.
@end{Discussion}
@begin{Honest}
  By the class-wide type @lquotes@;associated@rquotes@; with a type @i(T),
  we mean the type @i(T)'Class.
  Similarly, the universal type associated with
  @i{root_integer}, @i{root_real}, and @i{root_fixed} are
  @i{universal_integer}, @i{universal_real}, and @i{universal_fixed},
  respectively.
@end{Honest}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
Every type is either a @i(specific) type, a @i(class-wide) type,
or a @i(universal) type.
@Defn{specific type}
A specific type is
one defined by a @nt<type_declaration>,
a @nt<formal_type_declaration>, or a full type definition
embedded in @Chg{Version=[2],New=[another construct],Old=[a declaration
for an object]}.
Class-wide and universal types are implicitly defined, to act
as representatives for an entire class of types, as follows:
@begin(Honest)
  The root types @i(root_integer), @i(root_real), and
  @i(root_fixed) are also specific
  types. They are declared in the specification of package Standard.
@end(Honest)
@begin(Description)
@Defn{class-wide type}Class-wide types
@\Class-wide types are defined for @Redundant[(and belong to)]
each derivation class rooted
at a tagged type (see @RefSecNum(Tagged Types and Type Extensions)).
Given a subtype S of a tagged type @i(T),
S'Class is the @nt<subtype_mark> for a corresponding
subtype of the tagged class-wide
type @i(T)'Class. Such types are called
@lquotes@;class-wide@rquotes@; because when a formal parameter is defined
to be of a class-wide type @i(T)'Class, an actual parameter
of any type in the derivation class rooted at @i(T) is acceptable
(see @RefSecNum(The Context of Overload Resolution)).

@NoPrefix@Defn{first subtype}
The set of values for a class-wide type @i(T)'Class is the discriminated
union of the set of values of each specific type in the
derivation class rooted at @i(T) (the tag acts as the implicit discriminant
@em see @RefSecNum(Tagged Types and Type Extensions)).
Class-wide types have no primitive subprograms of their own.
However, as explained in @RefSecNum(Dispatching Operations of Tagged Types),
operands of a class-wide type @i(T)'Class can be used as part
of a dispatching call on a primitive subprogram of the type @i(T).
The only components @Redundant[(including discriminants)] of
@i(T)'Class that are visible are those of @i(T).
If S is a first subtype,
then S'Class is a first subtype.
@begin{Reason}
We want S'Class to be a first subtype when S is,
so that an @nt{attribute_@!definition_@!clause} like
@lquotes@key[for] S'Class'Output @key[use] ...;@rquotes@;
will be legal.
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
@Defn{universal type}Universal types
@\Universal types
are defined for @Redundant[(and belong to)] the integer,
real, @Chg{Version=[2],New=[],Old=[and ]}fixed point@Chg{Version=[2],
New=[, and access],Old=[]} classes,
and are referred to in this standard as respectively,
@i(universal_integer), @i(universal_real), @Chg{Version=[2],New=[],
Old=[and ]}@i(universal_fixed)@Chg{Version=[2],
New=[, and @i(universal_access)@Chg{Version=[3],
New=[@PDefn{universal_integer}@PDefn{universal_real}@PDefn{universal_fixed}@PDefn{universal_access}],Old=[]}],Old=[]}.
These are analogous to class-wide types for these language-defined
@Chg{Version=[2],New=[elementary],Old=[numeric]} classes.
As with class-wide types, if a formal parameter is of a universal type,
then an actual parameter of any type in the corresponding class
is acceptable. In addition, a value of a universal type
(including an integer or real @nt<numeric_literal>@Chg{Version=[2],New=[, or
the literal @key{null}],Old=[]}) is @lquotes@;universal@rquotes@;
in that it is acceptable where some particular type in the
class is expected
(see @RefSecNum(The Context of Overload Resolution)).

@NoPrefix@;The set of values of a universal type is the undiscriminated union
of the set of values possible for any definable type in the associated class.
Like class-wide types, universal types have no
primitive subprograms of their own. However, their @lquotes@;universality@rquotes@; allows
them to be used as operands with the primitive subprograms of any
type in the corresponding class.
@begin(Discussion)
  A class-wide type is only class-wide in one direction,
  from specific to class-wide, whereas
  a universal type is class-wide (universal) in both directions,
  from specific to universal and back.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
  We considered defining class-wide or perhaps universal types for
  all derivation classes, not just tagged classes and these @Chg{Version=[2],
  New=[four elementary],Old=[three numeric]} classes. However, this was felt
  to overly weaken the strong-typing model in some situations. Tagged types
  preserve strong type distinctions thanks to the run-time tag. Class-wide
  or universal types for untagged types would weaken the compile-time
  type distinctions without providing a compensating run-time-checkable
  distinction.

  We considered defining standard names for the universal
  numeric types so they could be used in formal parameter specifications.
  However, this was felt to impose an undue implementation burden for
  some implementations.
@end(Discussion)
@begin(Honest)
  Formally, the set of values of a universal type is actually a @i(copy) of
  the undiscriminated union of the values of the types in its
  class. This is because we
  want each value to have exactly one type, with explicit or implicit
  conversion needed to go between types. An alternative,
  consistent model would be to associate a class, rather than
  a particular type, with a value,
  even though any given expression would have a particular type.
  In that case, implicit type conversions would not generally need to
  change the value, although an associated subtype conversion might
  need to.
@end(Honest)
@end(Description)

@PDefn{root_integer}
@PDefn{root_real}
The integer and real numeric classes each have a specific root type in
addition to their universal type, named respectively @i(root_integer)
and @i(root_real).

@Defn2{Term=[cover], Sec=(a type)}
A class-wide or universal type is said to @i(cover) all of the types
in its class. A specific type covers only itself.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00251-01]}
@Defn2{Term=[descendant], Sec=(of a type)}
A specific type @i(T2) is defined to be a @i(descendant) of a
type @i(T1) if @i(T2) is the same as @i(T1), or if @i(T2) is derived
(directly or indirectly) from @i(T1). A class-wide type @i(T2)'Class is
defined to be a descendant of type @i(T1) if @i(T2) is a descendant of @i(T1).
Similarly, the @Chg{Version=[2],New=[numeric ],Old=[]}universal types are
defined to be descendants of the root types of their classes.
@Defn2{Term=[ancestor], Sec=(of a type)}
If a type @i(T2) is a descendant of a type @i(T1),
then @i(T1) is called an @i(ancestor) of @i(T2).
@Defn2{Term=[ultimate ancestor], Sec=(of a type)}
@Defn2{Term=[ancestor], Sec=(ultimate)}
@Chg{Version=[2],New=[An],Old=[The]} @i(ultimate ancestor) of a type
is @Chg{Version=[2],New=[an],Old=[the]} ancestor of
@Chg{Version=[2],New=[that],Old=[the]} type that is not
@Chg{Version=[2],New=[itself ],Old=[]}a descendant of any other
type.@Chg{Version=[2],New=[ Every untagged type
has a unique ultimate ancestor.],Old=[]}
@begin{Ramification}
  A specific type is a descendant of itself.
  Class-wide types are considered descendants of the corresponding
  specific type, and do not have any descendants of their own.

  A specific type is an ancestor of itself.
  The root of a derivation class is an ancestor of all types in the
  class, including any class-wide types in the class.
@end{Ramification}
@begin(Discussion)
  @Leading@keepnext@;The terms root, parent, ancestor, and ultimate ancestor
  are all related. For example:
  @begin(Itemize)
    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
    Each type has at most one parent, and one
    or more ancestor types; each @Chg{Version=[2],New=[untagged ],Old=[]}type
    has exactly one ultimate ancestor.
    In Ada 83, the term @lquotes@;parent type@rquotes@; was sometimes used
    more generally to include any ancestor type
    (e.g. RM83-9.4(14)). In Ada 95, we restrict
    parent to mean the immediate ancestor.

    A class of types has at most one root type; a derivation class
    has exactly one root type.

    The root of a class is an ancestor of all of the types in the class
    (including itself).

    The type @i(root_integer) is the root of the integer class,
    and is the ultimate ancestor of all integer types.
    A similar statement applies to @i(root_real).
  @end(Itemize)
@end(Discussion)

@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<Ancestor>,
  Text=<@ChgAdded{Version=[2],Text=[An ancestor of a type is the type itself
  or, in the case of a type derived from other types, its parent type or one
  of its progenitor types or one of their ancestors. Note that ancestor and
  descendant are inverse relationships.]}>}

@ChgToGlossary{Version=[2],Kind=[AddedNormal],Term=<Descendant>,
  Text=<@ChgAdded{Version=[2],Text=[A type is a descendant of itself, its
  parent and progenitor types, and their ancestors. Note that descendant and
  ancestor are inverse relationships.]}>}

@Defn2{Term=[inherited], Sec=(from an ancestor type)}
An inherited component @Redundant[(including an inherited discriminant)] of a
derived type is inherited @i(from) a given
ancestor of the type
if the corresponding component was inherited by each derived type in the
chain of derivations going back to the given ancestor.

@end{StaticSem}

@begin{Notes}
Because operands of a universal type are acceptable to the
predefined operators of any type in their class, ambiguity can
result. For @i(universal_integer) and @i(universal_real), this
potential ambiguity is resolved by giving a preference
(see @RefSecNum{The Context of Overload Resolution})
to the predefined operators
of the corresponding root types (@i(root_integer)
and @i(root_real), respectively).
Hence, in an apparently ambiguous expression like
@begin(Display)
1 + 4 < 7
@end(Display)

@NoPrefix@;where each of the literals is of type @i(universal_integer),
the predefined operators of @i(root_integer) will be preferred over those
of other specific integer types, thereby resolving the ambiguity.
@begin(Ramification)
  Except for this preference, a root numeric type
  is essentially like any other specific type in the
  associated numeric class. In particular, the result of a
  predefined operator of a root numeric type is not @lquotes@;universal@rquotes@;
  (implicitly convertible) even if both operands were.
@end(Ramification)
@end{Notes}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[Updated the wording to define the
  @i{universal_access} type. This was defined to make @key{null} for
  anonymous access types sensible.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00401-01]}
  @ChgAdded{Version=[2],Text=[The definitions of ancestors and descendants
  were updated to allow multiple ancestors (necessary to support interfaces).]}
@end{DiffWord95}


@LabeledClause{Scalar Types}

@begin{Intro}
@Defn{scalar type}
@i(Scalar) types comprise enumeration types, integer types, and real types.
@Defn{discrete type}
Enumeration types and integer types are called @i(discrete) types;
@Defn{position number}
each value of a discrete type has a @i(position number) which is an integer
value.
@Defn{numeric type}
Integer types and real types are called @i(numeric) types.
@Redundant[All scalar types are ordered, that is, all
relational operators are predefined for their values.]

@end{Intro}

@begin{Syntax}
@Syn{lhs=<range_constraint>,rhs=" @key{range} @Syn2{range}"}


@Syn{lhs=<range>,rhs=" @Syn2{range_attribute_reference}
   | @Syn2{simple_expression} .. @Syn2{simple_expression}"}
@end{Syntax}
@begin(Discussion)
        These need to be @nt<simple_expression>s rather than
        more general @nt<expression>s because ranges appear in
        membership tests and other contexts where
        @nt<expression> .. @nt<expression> would
        be ambiguous.
@end(Discussion)

@begin{Intro}
@Defn{range}
@Defn2{Term=[lower bound], Sec=(of a range)}
@Defn2{Term=[upper bound], Sec=(of a range)}
@Defn{type of a range}
A @i(range) has a @i(lower bound) and an @i(upper bound) and
specifies a subset of the values of some scalar type
(the @i(type of the range)).
A range with lower bound L and upper bound R is described by @lquotes@;L .. R@rquotes@;.
@Defn{null range}
If R is less than L, then
the range is a @i(null range), and specifies an
empty set of values.
Otherwise, the range specifies the values of the type from
the lower bound to the upper bound, inclusive.
@Defn2{Term=[belong], Sec=(to a range)}
A value @i(belongs) to a range if it is of the type of the
range, and is in the subset of values specified by the range.
@PDefn2{Term=[satisfies], Sec=(a range constraint)}
A value @i(satisfies) a range constraint if it belongs to
the associated range.
@Defn2{Term=[included], Sec=(one range in another)}
One range is @i(included) in another if all values that
belong to the first range also belong to the second.
@end{Intro}

@begin{Resolution}
@PDefn2{Term=[expected type], Sec=(range_constraint range)}
For a @nt<subtype_indication> containing a @nt<range_constraint>, either
directly or as part of some other @nt<scalar_constraint>,
the type of the @nt<range> shall resolve to that of the type determined by
the @nt<subtype_mark> of the @nt<subtype_indication>.
@PDefn2{Term=[expected type], Sec=(range simple_expressions)}
For a @nt<range> of a given type,
the @nt<simple_expression>s of the @nt<range> (likewise, the
@nt<simple_expression>s of the equivalent @nt<range> for a
@nt<range_attribute_reference>)
are expected to be of the type of the @nt<range>.
@begin(Discussion)
  In Ada 95, @nt<constraint>s
  only appear within @nt<subtype_indication>s; things that look
  like constraints that appear in type declarations are called
  something else like @nt<real_range_specification>s.

  We say "the expected type is ..." or "the type is expected to be ..."
  depending on which reads better. They are fundamentally equivalent,
  and both feed into the type resolution rules of clause
  @RefSecNum(The Context of Overload Resolution).

  In some cases, it doesn't work to use expected types.
  For example, in the above rule, we say that
  the @lquotes@;type of the @nt<range> shall resolve to ...@rquotes@;
  rather than @lquotes@;the expected type for the @nt<range> is ...@rquotes@;.
  We then use @lquotes@;expected type@rquotes@; for the bounds.
  If we used @lquotes@;expected@rquotes@; at both points, there
  would be an ambiguity, since one could apply the rules of
  @RefSecNum{The Context of Overload Resolution}
  either on determining the type of the range, or on determining the
  types of the individual bounds. It is clearly important
  to allow one bound to be of a universal type, and the other of
  a specific type, so we need to use @lquotes@;expected type@rquotes@; for the bounds.
  Hence, we used @lquotes@;shall resolve to@rquotes@; for the type of the range as a
  whole.
  There are other situations where @lquotes@;expected type@rquotes@; is not quite
  right, and we use @lquotes@;shall resolve to@rquotes@; instead.
@end(Discussion)
@end{Resolution}

@begin{StaticSem}
@RootDefn2{Term=[base range], Sec=(of a scalar type)}
The @i(base range) of a scalar type is the range of
finite values of the type that can be represented
in every unconstrained object of the type;
it is also the range supported at a minimum for
intermediate values during the evaluation of expressions involving
predefined operators of the type.
@begin{ImplNote}
Note that in some machine architectures intermediates
  in an expression (particularly if static),
  and register-resident variables might accommodate
  a wider range. The base range does not include the values
  of this wider range that are not assignable without overflow to
  memory-resident objects.@end{implnote}
@begin(Ramification)
  @PDefn2{Term=[base range], Sec=(of an enumeration type)}
  The base range of an enumeration type is the range of values
  of the enumeration type.
@end(Ramification)
@begin{Reason}

  If the representation supports infinities,
  the base range is nevertheless restricted
  to include only the representable finite values,
  so that 'Base'First and 'Base'Last are always guaranteed to be finite.@end{reason}
@begin(Honest)
  By a "value that can be assigned without overflow" we don't mean
  to restrict ourselves to values that can be represented exactly.
  Values between machine representable values can be assigned,
  but on subsequent reading, a slightly different value might
  be retrieved, as (partially) determined by the number of digits of
  precision of the type.
@end(Honest)

@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
@Redundant[A constrained scalar subtype is one to which a range constraint
applies.]
@Defn2{Term=[range], Sec=(of a scalar subtype)}
The @i(range) of a constrained scalar subtype
is the range associated with the range constraint of the subtype.
The @i(range) of an unconstrained scalar subtype is the base range of
its type.
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[compatibility], Sec=(range with a scalar subtype)}
A range is @i(compatible) with a scalar subtype if and only
if it is either a null range
or each bound of the range belongs to the range of the subtype.
@PDefn2{Term=[compatibility], Sec=(range_constraint with a scalar subtype)}
A @nt<range_constraint> is @i(compatible) with a scalar subtype if and only if
its range is compatible with the subtype.
@begin(Ramification)
  Only @nt<range_constraint>s (explicit or implicit) impose conditions
  on the values of a scalar subtype. The other @nt<scalar_constraint>s,
  @nt<digits_constraint>s and @nt<delta_constraint>s impose conditions
  on the subtype denoted by the @nt<subtype_mark> in a @nt<subtype_indication>,
  but don't impose a condition on the values of the subtype being
  defined. Therefore, a scalar subtype is not called @i(constrained)
  if all that applies to it is a @nt<digits_constraint>.
  Decimal subtypes are subtle, because a @nt<digits_constraint> without
  a @nt<range_constraint> nevertheless includes an implicit
  @nt<range_constraint>.
@end(Ramification)

@PDefn2{Term=[elaboration], Sec=(range_constraint)}
The elaboration of a @nt{range_constraint} consists of the
evaluation of the @nt{range}.
@PDefn2{Term=[evaluation], Sec=(range)}
The evaluation of a @nt{range} determines a lower bound and an upper bound.
If @nt<simple_expression>s are given to specify bounds, the evaluation of
the @nt<range> evaluates these @nt<simple_expression>s in an arbitrary order,
and converts them to the type of the @nt<range>.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
@PDefn2{Term=[implicit subtype conversion],Sec=(bounds of a range)}
If a @nt<range_attribute_reference> is given, the evaluation
of the @nt<range>
consists of the evaluation of the @nt<range_attribute_reference>.

@keepnext@i(Attributes)

@Leading@keepnext@;For @PrefixType{every scalar subtype S},
the following attributes are defined:
@begin(description)
@Attribute{Prefix=<S>, AttrName=<First>,
  Text=[S'First denotes the lower bound of
     the range of S. The value of this attribute is of the type
     of S.]}
     @begin{Ramification}
Evaluating S'First never raises Constraint_Error.@end{ramification}

@Attribute{Prefix=<S>, AttrName=<Last>,
  Text=[S'Last denotes the upper bound of
     the range of S. The value of this attribute is of the type
     of S.]}
     @begin{Ramification}
Evaluating S'Last never raises Constraint_Error.@end{ramification}

@Attribute{Prefix=<S>, AttrName=<Range>,
  Text=[S'Range is equivalent to the @nt<range> S'First .. S'Last.]}

@Attribute{Prefix=<S>, AttrName=<Base>,
  Text=[S'Base denotes an
     unconstrained subtype of the type of S.
     This unconstrained subtype is called the @i(base subtype) of the type.
     ]}@Defn2{Term=(base subtype), Sec=(of a type)}

@AttributeLeading{Prefix=<S>, AttrName=<Min>,
  Text=[S'Min denotes a function with the following specification:
@begin(Descexample)
@b(function) S'Min(@RI(Left), @RI(Right) : S'Base)
  @b(return) S'Base
@end(Descexample)

     @NoPrefix@;The function returns the lesser of the values
     of the two parameters.]}
     @begin{Discussion}
     @Defn2{Term=[italics],Sec=(formal parameters of attribute functions)}
     The formal parameter names are italicized because they cannot be
     used in calls @em
     see @RefSecNum{Subprogram Calls}.
     Such a specification
     cannot be written by the user because an @nt<attribute_reference>
     is not permitted as the designator of a user-defined function, nor
     can its formal parameters be anonymous.
     @end{Discussion}

@AttributeLeading{Prefix=<S>, AttrName=<Max>,
  Text=[S'Max denotes a function with the following specification:
@begin(Descexample)
@b(function) S'Max(@RI(Left), @RI(Right) : S'Base)
  @b(return) S'Base
@end(Descexample)

     @NoPrefix@;The function returns the greater of the values of the two parameters.]}

@AttributeLeading{Prefix=<S>, AttrName=<Succ>,
  Text=[S'Succ denotes a function with the following specification:
@begin(Descexample)
@b(function) S'Succ(@RI(Arg) : S'Base)
  @b(return) S'Base
@end(Descexample)

     @NoPrefix@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
     For an enumeration type, the function returns the value
     whose position number is one more than that of the value of @i(Arg);
     @IndexCheck{Range_Check}
     Constraint_Error is raised if there is no such value of the type.
     For an integer type, the function returns the result of
     adding one to the value of @i(Arg).
     For a fixed point type, the function returns the result of
     adding @i(small) to the value of @i(Arg).
     For a floating point type, the
     function returns the machine number (as defined
     in @RefSecNum(Floating Point Types))
     immediately above the value of @i(Arg);
     @IndexCheck{Range_Check}
     Constraint_Error is raised if there is no such machine number.]}
     @begin{Ramification}
S'Succ for a modular integer subtype wraps around
       if the value of @i(Arg) is S'Base'Last. S'Succ for a signed integer
       subtype might raise Constraint_Error if the value of @i(Arg) is
       S'Base'Last, or it might return the out-of-base-range value
       S'Base'Last+1, as is permitted for all predefined numeric operations.@end{ramification}

@AttributeLeading{Prefix=<S>, AttrName=<Pred>,
  Text=[S'Pred denotes a function with
     the following specification:
@begin(Descexample)
@b(function) S'Pred(@RI(Arg) : S'Base)
  @b(return) S'Base
@end(Descexample)

     @NoPrefix@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
     For an enumeration type, the function returns the value
     whose position number is one less than that of the value of @i(Arg);
     @IndexCheck{Range_Check}
     Constraint_Error is raised if there is no such value of the type.
     For an integer type, the function returns the result of
     subtracting one from the value of @i(Arg).
     For a fixed point type, the function returns the result of
     subtracting @i(small) from the value of @i(Arg).
     For a floating point type, the
     function returns the machine number (as defined
     in @RefSecNum(Floating Point Types))
     immediately below the value of @i(Arg);
     @IndexCheck{Range_Check}
     Constraint_Error is raised if there is no such machine number.]}
     @begin{Ramification}
S'Pred for a modular integer subtype wraps around
       if the value of @i(Arg) is S'Base'First. S'Pred for a signed integer
       subtype might raise Constraint_Error if the value of @i(Arg) is
       S'Base'First, or it might return the out-of-base-range value
       S'Base'First@en@;1, as is permitted for all predefined numeric operations.@end{ramification}

@ChgAttribute{Version=[2],Kind=[Added],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Wide_Wide_Image>, ARef=[AI95-00285-01],
  Text=[@Chg{Version=[2],New=[S'Wide_Wide_Image denotes a function
     with the following specification:],Old=[]}
@begin(Descexample)
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@b(function) S'Wide_Wide_Image(@RI(Arg) : S'Base)
  @b(return) Wide_Wide_String]}
@end(Descexample)

     @ChgRef{Version=[2],Kind=[Added]}
     @ChgAdded{Version=[2],NoPrefix=[T],Text=[@Defn2{Term=[image], Sec=(of a value)}
     The function returns an @i(image) of the value of @i(Arg),
     that is, a sequence of characters representing the value in display
     form.]}]}@Comment{End of Annex text here.}
     @ChgAdded{Version=[2],NoPrefix=[T],Text=[The lower bound of the result is one.]}

     @ChgRef{Version=[2],Kind=[Added]}
     @ChgAdded{Version=[2],NoPrefix=[T],Text=[The image of an integer value is
     the corresponding decimal literal,
     without underlines, leading zeros, exponent, or trailing spaces, but
     with a single leading character that is either a minus sign or
     a space.]}
     @begin{ImplNote}
         @ChgRef{Version=[2],Kind=[AddedNormal]}
         @ChgAdded{Version=[2],Text=[
         If the machine supports negative zeros for signed integer types,
         it is not specified whether "@ 0" or "@en@;0" should be returned
         for negative zero. We don't have enough experience with
         such machines to know what is appropriate, and what other
         languages do. In any case, the implementation should be
         consistent.]}
     @end{implnote}

     @ChgRef{Version=[2],Kind=[Added]}
     @ChgAdded{Version=[2],NoPrefix=[T],Text=[@Defn{nongraphic character}
     The image of an enumeration value is either the corresponding
     identifier in upper case or the corresponding character literal
     (including the two apostrophes); neither leading nor trailing
     spaces are included.
     For a @i(nongraphic character) (a value of
     a character type that has no
     enumeration literal associated with it), the
     result is a corresponding language-defined
     name in upper case (for example, the image
     of the nongraphic character identified as @i(nul) is @lquotes@;NUL@rquotes@; @em the
     quotes are not part of the image).]}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[AddedNormal]}
       @ChgAdded{Version=[2],Text=[
       For an enumeration type T that has @lquotes@;holes@rquotes@;
       (caused by an @nt{enumeration_@!representation_@!clause}),
       @Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
       T'Wide_Image should raise Program_Error if the value
       is one of the holes (which is a bounded error anyway,
       since holes can be generated only via uninitialized variables and
       similar things).]}
     @end{ImplNote}

     @ChgRef{Version=[2],Kind=[Added]}
     @ChgAdded{Version=[2],NoPrefix=[T],Text=[The image of a
     floating point value is a decimal real literal
     best approximating the value (rounded away from zero if halfway between)
     with a single leading character that is either a minus sign
     or a space, a single digit (that is nonzero unless the value is zero),
     a decimal point, S'Digits@en@;1
     (see @RefSecNum(Operations of Floating Point Types)) digits
     after the decimal point (but one if S'Digits is one),
     an upper case E, the sign of the
     exponent (either + or @en), and two or more digits
     (with leading zeros if necessary)
     representing the exponent.
     If S'Signed_Zeros is True, then the leading character is a minus
     sign for a negatively signed zero.]}
     @begin{Honest}
       @ChgRef{Version=[2],Kind=[AddedNormal]}
       @ChgAdded{Version=[2],Text=[
       Leading zeros are present in the exponent only if necessary to make
       the exponent at least two digits.]}
     @end{Honest}
     @begin{Reason}
       @ChgRef{Version=[2],Kind=[AddedNormal]}
       @ChgAdded{Version=[2],Text=[
       This image is intended to conform to that produced by
       Text_IO.Float_IO.Put in its default format.]}
     @end{reason}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[AddedNormal]}
       @ChgAdded{Version=[2],Text=[The rounding direction is specified here
       to ensure portability of output results.]}
     @end{implnote}

     @ChgRef{Version=[2],Kind=[Added]}
     @ChgAdded{Version=[2],NoPrefix=[T],Text=[The image of a fixed point value
     is a decimal real literal
     best approximating the value (rounded away from zero if halfway between)
     with a single leading character that is either a minus sign
     or a space, one or more digits before the decimal point
     (with no redundant leading zeros),
     a decimal point, and S'Aft (see @RefSecNum(Operations of Fixed Point Types))
     digits after the decimal point.]}
     @begin{Reason}
       @ChgRef{Version=[2],Kind=[AddedNormal]}
       @ChgAdded{Version=[2],Text=[This image is intended to conform to
       that produced by Text_IO.Fixed_IO.Put.]}
     @end{reason}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[AddedNormal]}
       @ChgAdded{Version=[2],Text=[The rounding direction is specified here
       to ensure portability of output results.]}
     @end{implnote}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[AddedNormal]}
       @ChgAdded{Version=[2],Text=[For a machine that supports negative zeros,
       it is not specified whether "@ 0.000" or "@en@;0.000" is returned.
       See corresponding comment above about integer types with
       signed zeros.]}
     @end{implnote}

@AttributeLeading{Prefix=<S>, AttrName=<Wide_Image>,
  Text=[S'Wide_Image denotes a function
     with the following specification:
@begin(Descexample)
@b(function) S'Wide_Image(@RI(Arg) : S'Base)
  @b(return) Wide_String
@end(Descexample)

     @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
     @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1],ARef=[AI05-0264-1]}
     @NoPrefix@Defn2{Term=[image], Sec=(of a value)}
     The function returns an @Chg{Version=[2],New=[image],Old=[@i(image)]} of
     the value of @i(Arg)@Chg{Version=[2],New=[ as a Wide_String],Old=[,
     that is, a sequence of characters representing the value in display
     form]}.]}
     The lower bound of the result is one.@Chg{Version=[2],
     New=[ The image has the same sequence of @Chg{Version=[3],New=[graphic
     characters],Old=[character]} as
     defined for S'Wide_Wide_Image if all the graphic characters are defined in
     Wide_Character; otherwise@Chg{Version=[3],New=[,],Old=[]}
     the sequence of characters is
     implementation defined (but no shorter than that of S'Wide_Wide_Image for
     the same value of Arg).],Old=[]}
     @ChgImplDef{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
     Text=[The sequence of characters of the value returned by
     S'Wide_Image when some of the graphic characters of S'Wide_Wide_Image
     are not defined in Wide_Character.]}]}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 31
through 34 were moved to Wide_Wide_Image.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}

     @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
     @ChgDeleted{Version=[2],NoPrefix=[T],Text=[The image of an integer value is
     the corresponding decimal literal,
     without underlines, leading zeros, exponent, or trailing spaces, but
     with a single leading character that is either a minus sign or
     a space.]}
     @begin{ImplNote}
         @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
         @ChgDeleted{Version=[2],Text=[
         If the machine supports negative zeros for signed integer types,
         it is not specified whether "@en@;0" or " 0" should be returned
         for negative zero. We don't have enough experience with
         such machines to know what is appropriate, and what other
         languages do. In any case, the implementation should be
         consistent.]}
     @end{implnote}

     @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
     @ChgDeleted{Version=[2],NoPrefix=[T],Text=[@Defn{nongraphic character}
     The image of an enumeration value is either the corresponding
     identifier in upper case or the corresponding character literal
     (including the two apostrophes); neither leading nor trailing
     spaces are included.
     For a @i(nongraphic character) (a value of
     a character type that has no
     enumeration literal associated with it), the
     result is a corresponding language-defined or implementation-defined
     name in upper case (for example, the image
     of the nongraphic character identified as @i(nul) is @lquotes@;NUL@rquotes@; @em the
     quotes are not part of the image).]}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
       @ChgDeleted{Version=[2],Text=[For an enumeration type T
       that has @lquotes@;holes@rquotes@;
       (caused by an @nt{enumeration_representation_clause}),
       @Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
       T'Wide_Image should raise Program_Error if the value
       is one of the holes (which is a bounded error anyway,
       since holes can be generated only via uninitialized variables and
       similar things).]}
     @end{ImplNote}

     @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
     @ChgDeleted{Version=[2],NoPrefix=[T],Text=[The image of a
     floating point value is a decimal real literal
     best approximating the value (rounded away from zero if halfway
     between)
     with a single leading character that is either a minus sign
     or a space, a single digit (that is nonzero unless the value is zero),
     a decimal point, S'Digits@en@;1
     (see @RefSecNum(Operations of Floating Point Types)) digits
     after the decimal point (but one if S'Digits is one),
     an upper case E, the sign of the
     exponent (either + or @en), and two or more digits
     (with leading zeros if necessary)
     representing the exponent.
     If S'Signed_Zeros is True, then the leading character is a minus
     sign for a negatively signed zero.]}
     @begin{Honest}
       @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
       @ChgDeleted{Version=[2],Text=[Leading zeros are present in the
       exponent only if necessary to make the exponent at least two digits.]}
     @end{Honest}
     @begin{Reason}
       @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
       @ChgDeleted{Version=[2],Text=[This image is intended to conform to
       that produced by
        Text_IO.Float_IO.Put in its default format.]}
     @end{reason}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
       @ChgDeleted{Version=[2],Text=[The rounding direction is specified here
       to ensure portability of output results.]}
     @end{implnote}

     @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
     @ChgDeleted{Version=[2],NoPrefix=[T],Text=[The image of a
     fixed point value is a decimal real literal
     best approximating the value (rounded away from zero if halfway between)
     with a single leading character that is either a minus sign
     or a space, one or more digits before the decimal point
     (with no redundant leading zeros),
     a decimal point, and S'Aft (see @RefSecNum(Operations of Fixed Point Types))
     digits after the decimal point.]}
     @begin{Reason}
       @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
       @ChgDeleted{Version=[2],Text=[This image is intended to conform to
       that produced by Text_IO.Fixed_IO.Put.]}
     @end{reason}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
       @ChgDeleted{Version=[2],Text=[The rounding direction is specified here
       to ensure portability of output results.]}
     @end{implnote}
     @begin{ImplNote}
       @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
       @ChgDeleted{Version=[2],Text=[For a machine that supports negative zeros,
       it is not specified whether "@en@;0.000" or " 0.000" is returned.
       See corresponding comment above about integer types with
       signed zeros.]}
    @end{implnote}

@AttributeLeading{Prefix=<S>, AttrName=<Image>,
  Text=[S'Image denotes a function with
    the following specification:
@begin(Descexample)
@b(function) S'Image(@RI(Arg) : S'Base)
  @b(return) String
@end(Descexample)

     @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
     @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
     @NoPrefix@;The function returns an image of the value of @i(Arg)
     as a String.]}
     The lower bound of the result is one. The image has the
     same sequence of graphic characters as that defined
     for S'@Chg{Version=[2],New=[Wide_Wide_Image],Old=[Wide_Image]} if all
     the graphic characters are defined in Character;
     otherwise@Chg{Version=[3],New=[,],Old=[]}
     the sequence of characters is implementation defined (but
     no shorter than that of S'@Chg{Version=[2],New=[Wide_Wide_Image],
     Old=[Wide_Image]} for the same value of @i(Arg)).
     @ChgImplDef{Version=[2],Kind=[Revised],InitialVersion=[0],Text=[The
     sequence of characters of the value returned by
     S'Image when some of the graphic characters of
     S'@Chg{Version=[2],New=[Wide_Wide_Image],Old=[Wide_Image]} are not
     defined in Character.]}

@ChgAttribute{Version=[2],Kind=[Added],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Wide_Wide_Width>, ARef=[AI95-00285-01],
  Text=[@Chg{Version=[2],New=[S'Wide_Wide_Width denotes the
     maximum length of a Wide_Wide_String
     returned by S'Wide_Wide_Image over all values of the
     subtype S. It denotes zero for a subtype that has
     a null range. Its type is @i(universal_integer).],Old=[]}]}

@Attribute{Prefix=<S>, AttrName=<Wide_Width>,
  Text=[S'Wide_Width denotes the maximum length of a Wide_String
     returned by S'Wide_Image over all values of the
     subtype S. It denotes zero for a subtype that has
     a null range. Its type is @i(universal_integer).]}

@Attribute{Prefix=<S>, AttrName=<Width>,
  Text=[S'Width denotes the maximum length of a String
     returned by S'Image over all values of the
     subtype S. It denotes zero for a subtype that has
     a null range. Its type is @i(universal_integer).]}

@ChgAttribute{Version=[2],Kind=[Added],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Wide_Wide_Value>, ARef=[AI95-00285-01],
  Text=[@Chg{Version=[2],New=[S'Wide_Wide_Value denotes a function
     with the following specification:],Old=[]}
@begin(Descexample)
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@b(function) S'Wide_Wide_Value(@RI(Arg) : Wide_Wide_String)
  @b(return) S'Base]}
@end(Descexample)
    @ChgRef{Version=[2],Kind=[Added]}
    @ChgAdded{Version=[2],NoPrefix=[T],Text=[This function returns
    a value given an image of the value
    as a Wide_Wide_String, ignoring any leading or trailing spaces.]}]}

    @ChgRef{Version=[2],Kind=[Added]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
    @ChgAdded{Version=[2],NoPrefix=[T],Text=[@PDefn2{Term=[evaluation], Sec=(Wide_Wide_Value)}
    @Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
    For the evaluation of a call on S'Wide_Wide_Value
    for an enumeration subtype S,
    if the sequence of characters of the parameter (ignoring
    leading and trailing spaces) has the syntax
    of an enumeration literal and if it corresponds to a literal of the
    type of S (or corresponds to the result of S'Wide_Wide_Image
    for a nongraphic character of the type),
    the result is the corresponding enumeration value;
    @IndexCheck{Range_Check}
    otherwise@Chg{Version=[3],New=[,],Old=[]}
    Constraint_Error is raised.]}
    @begin{Discussion}
      @ChgRef{Version=[2],Kind=[Added]}
      @ChgAdded{Version=[2],Text=[It's not crystal clear that Range_Check
      is appropriate here,
      but it doesn't seem worthwhile to invent a whole new check name
      just for this weird case, so we decided to lump it in with
      Range_Check.]}
    @end{discussion}
    @begin{Honest}
      @ChgRef{Version=[2],Kind=[Added],Ref=[8652/0096],ARef=[AI95-00053-01]}
      @ChgAdded{Version=[2],Text=[A sequence of
      characters corresponds to the result of
      S'Wide_Wide_Image if it is the same ignoring case. Thus, the case of an
      image of a nongraphic character does not matter. For example,
      Character'Wide_Wide_Value("nul") does not raise Constraint_Error, even though
      Character'Wide_Wide_Image returns "NUL" for the nul character.]}
    @end{Honest}

    @ChgRef{Version=[2],Kind=[Added]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
    @ChgAdded{Version=[2],NoPrefix=[T],Text=[@Defn2{Term=(Constraint_Error),
    Sec=(raised by failure of run-time check)}
    For the evaluation of a call on S'Wide_Wide_Value for an integer
    subtype S, if the sequence of characters of the
    parameter (ignoring leading and trailing spaces)
    has the syntax of an integer literal,
    with an optional leading sign character
    (plus or minus for a signed type;
    only plus for a modular type), and the
    corresponding numeric value belongs to the base range of the
    type of S, then that value is the result;
    @IndexCheck{Range_Check}
    otherwise@Chg{Version=[3],New=[,],Old=[]}
    Constraint_Error is raised.]}

    @begin(Discussion)
      @ChgRef{Version=[2],Kind=[Added]}
      @ChgAdded{Version=[2],Text=[We considered allowing 'Value
      to return a representable but out-of-range
      value without a Constraint_Error. However, we currently require
      (see @RefSecNum(Static Expressions and Static Subtypes))
      in an @nt{assignment_statement} like "X := <numeric_literal>;" that
      the value of the
      numeric-literal be in X's base range (at compile time), so it seems
      unfriendly and confusing to have a different range allowed for 'Value.
      Furthermore, for modular types, without the requirement for being
      in the base range, 'Value would have to handle arbitrarily long
      literals (since overflow never occurs for modular types).]}
    @end(Discussion)

    @ChgRef{Version=[2],Kind=[Added]}
    @ChgAdded{Version=[2],NoPrefix=[T],Type=[Leading],Text=[For the
    evaluation of a call on S'Wide_Wide_Value for a
    real subtype S, if the sequence of characters of the
    parameter (ignoring leading and trailing spaces)
    has the syntax of one of the following:]}
@begin[itemize]
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@nt[numeric_literal]]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=<@nt[numeral].[@nt[exponent]]>}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=<.@nt[numeral][@nt[exponent]]>}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=<@nt[base]#@nt[based_numeral].#[@nt[exponent]]>}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=<@nt[base]#.@nt[based_numeral]#[@nt[exponent]]>}
@end{Itemize}

    @ChgRef{Version=[2],Kind=[Added]}
    @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0264-1]}
    @ChgAdded{Version=[2],NoPrefix=[T],Text=[@Defn2{Term=(Constraint_Error),
    Sec=(raised by failure of run-time check)}
    with an optional leading sign character (plus or minus), and if the
    corresponding numeric value belongs to the base range of the
    type of S, then that value is the result;
    @IndexCheck{Range_Check}
    otherwise@Chg{Version=[3],New=[,],Old=[]}
    Constraint_Error is raised.
    The sign of a zero value is preserved
    (positive if none has been specified)
    if S'Signed_Zeros is True.]}


@AttributeLeading{Prefix=<S>, AttrName=<Wide_Value>,
  Text=[S'Wide_Value denotes a function with
     the following specification:
@begin(Descexample)
@b(function) S'Wide_Value(@RI(Arg) : Wide_String)
  @b(return) S'Base
@end(Descexample)

    @NoPrefix@;This function returns a value given an image of the value
    as a Wide_String, ignoring any leading or trailing
    spaces.]}@Comment{This marks the end of the Annex text.}

    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
    @NoPrefix@PDefn2{Term=[evaluation], Sec=(Wide_Value)}
    @Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
    For the evaluation of a call on S'Wide_Value
    for an enumeration subtype S,
    if the sequence of characters of the parameter (ignoring
    leading and trailing spaces) has the syntax
    of an enumeration literal and if it corresponds to a literal of the
    type of S (or corresponds to the result of S'Wide_Image
    for a @Chg{Version=[2],New=[value],Old=[nongraphic character]} of the type),
    the result is the corresponding enumeration value;
    @IndexCheck{Range_Check}
    otherwise@Chg{Version=[3],New=[,],Old=[]} Constraint_Error is raised.
    @Chg{Version=[2],New=[For a numeric subtype S,
    the evaluation of a call on S'Wide_Value with @i(Arg) of type Wide_String
    is equivalent to a call on S'Wide_Wide_Value for a corresponding
    @i(Arg) of type Wide_Wide_String.],Old=[]}
    @begin{Discussion}
      @ChgRef{Version=[2],Kind=[Deleted]}
      @ChgDeleted{Version=[2],Text=[It's not crystal clear that Range_Check
      is appropriate here,
      but it doesn't seem worthwhile to invent a whole new check name
      just for this weird case, so we decided to lump it in with
      Range_Check.]}
    @end{discussion}
    @begin{Honest}
      @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0096],ARef=[AI95-00053-01]}
      @ChgRef{Version=[2],Kind=[DeletedAdded]}
      @ChgDeleted{Version=[2],Text=[
      @Chg{Version=[1],New=[A sequence of characters corresponds to the result of
      S'Wide_Image if it is the same ignoring case. Thus, the case of an
      image of a nongraphic character does not matter. For example,
      Character'Wide_Value("nul") does not raise Constraint_Error, even though
      Character'Wide_Image returns "NUL" for the nul character.],Old=[]}]}
    @end{Honest}
    @begin(Reason)
      @ChgRef{Version=[2],Kind=[AddedNormal]}
      @ChgAdded{Version=[2],Text=[S'Wide_Value is subtly different from
       S'Wide_Wide_Value for enumeration
       subtypes since S'Wide_Image might produce a different sequence of
       characters than S'Wide_Wide_Image if the enumeration literal
       uses characters outside of the predefined type Wide_Character.
       That is why we don't just define S'Wide_Value in terms of
       S'Wide_Wide_Value for enumeration subtypes.
       S'Wide_Value and S'Wide_Wide_Value for numeric subtypes yield
       the same result given the same sequence of characters.]}
    @end(Reason)

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 44
through 51 were moved to Wide_Wide_Value.>}]}@Comment{This message should be deleted if the
paragraphs are ever renumbered.}
@end{NotIso}

    @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
    @ChgDeleted{Version=[2],NoPrefix=[T],Text=[@Defn2{Term=(Constraint_Error),
    Sec=(raised by failure of run-time check)}
    For the evaluation of a call on S'Wide_Value (or S'Value) for an integer
    subtype S, if the sequence of characters of the
    parameter (ignoring leading and trailing spaces)
    has the syntax of an integer literal,
    with an optional leading sign character
    (plus or minus for a signed type;
    only plus for a modular type), and the
    corresponding numeric value belongs to the base range of the
    type of S, then that value is the result;
    @IndexCheck{Range_Check}
    otherwise Constraint_Error is raised.]}

    @begin(Discussion)
      @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
      @ChgDeleted{Version=[2],Text=[We considered allowing 'Value
      to return a representable but out-of-range
      value without a Constraint_Error. However, we currently require
      (see @RefSecNum(Static Expressions and Static Subtypes))
      in an @nt{assignment_statement} like "X := <numeric_literal>;" that
      the value of the
      numeric-literal be in X's base range (at compile time), so it seems
      unfriendly and confusing to have a different range allowed for 'Value.
      Furthermore, for modular types, without the requirement for being
      in the base range, 'Value would have to handle arbitrarily long
      literals (since overflow never occurs for modular types).]}
    @end(Discussion)

    @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
    @ChgDeleted{Version=[2],NoPrefix=[T],Type=[Leading],Text=[For the
    evaluation of a call on S'Wide_Value (or S'Value) for a
    real subtype S, if the sequence of characters of the
    parameter (ignoring leading and trailing spaces)
    has the syntax of one of the following:]}
@begin[itemize]
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=<@nt[numeric_literal]>}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=<@nt[numeral].[@nt[exponent]]>}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=<.@nt[numeral][@nt[exponent]]>}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=<@nt[base]#@nt[based_numeral].#[@nt[exponent]]>}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[@nt[base]#.@nt[based_numeral]#[@nt[exponent]]]}
@end{Itemize}

    @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
    @ChgDeleted{Version=[2],NoPrefix=[T],Text=[@Defn2{Term=(Constraint_Error),
    Sec=(raised by failure of run-time check)}
    with an optional leading sign character (plus or minus), and if the
    corresponding numeric value belongs to the base range of the
    type of S, then that value is the result;
    @IndexCheck{Range_Check}
    otherwise Constraint_Error is raised.
    The sign of a zero value is preserved
    (positive if none has been specified)
    if S'Signed_Zeros is True.]}

@AttributeLeading{Prefix=<S>, AttrName=<Value>,
  Text=[S'Value denotes a function with
     the following specification:
@begin(Descexample)
@b(function) S'Value(@RI(Arg) : String)
  @b(return) S'Base
@end(Descexample)

    @NoPrefix@;This function returns a value given an image of the value
    as a String, ignoring any leading or trailing spaces.]}

    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
    @NoPrefix@PDefn2{Term=[evaluation], Sec=(Value)}
    @Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
    For the evaluation of a call on S'Value
    for an enumeration subtype S,
    if the sequence of characters of the parameter (ignoring
    leading and trailing spaces) has the syntax
    of an enumeration literal and if it corresponds to a literal of the
    type of S (or corresponds to the result of S'Image
    for a value of the type),
    the result is the corresponding enumeration value;
    @IndexCheck{Range_Check}
    otherwise@Chg{Version=[3],New=[,],Old=[]} Constraint_Error is raised.
    For a numeric subtype S,
    the evaluation of a call on S'Value with @i(Arg) of type String
    is equivalent to a call on S'@Chg{Version=[2],New=[Wide_Wide_Value],Old=[Wide_Value]} for a corresponding
    @i(Arg) of type @Chg{Version=[2],New=[Wide_Wide_String],Old=[Wide_String]}.
    @begin(Reason)
      @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
       S'Value is subtly different from S'@Chg{Version=[2],New=[Wide_Wide_Value],Old=[Wide_Value]} for enumeration
       subtypes@Chg{Version=[2],New=[; see the discussion under S'Wide_Value],
       Old=[since S'Image might produce a different sequence of
       characters than S'Wide_Image if the enumeration literal
       uses characters outside of the predefined type Character.
       That is why we don't just define S'Value in terms of S'Wide_Value
       for enumeration subtypes.
       S'Value and S'Wide_Value for numeric subtypes yield
       the same result given the same sequence of characters]}.
    @end(Reason)

@end(description)
@EndPrefixType{}
@end{RunTime}

@begin{ImplPerm}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
An implementation may extend the @Chg{Version=[2],New=[Wide_Wide_Value, ],
Old=[Wide_Value, ]}
@Redundant[@Chg{Version=[2],New=[Wide_Value, ],Old=[]}Value,
@Chg{Version=[2],New=[Wide_Wide_Image, ],
Old=[]}Wide_Image, and Image] attributes
of a floating point type
to support special values such as infinities and NaNs.

@begin{TheProof}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
The permission is really only necessary for @Chg{Version=[2],New=[Wide_Wide_Value],
Old=[Wide_Value]},
because Value @Chg{Version=[2],New=[and Wide_Value are],Old=[is]} defined in
terms of @Chg{Version=[2],New=[Wide_Wide_Value],
Old=[Wide_Value]},
and because the behavior of @Chg{Version=[2],New=[Wide_Wide_Image, ],
Old=[]}Wide_Image@Chg{Version=[2],New=[,],Old=[]} and Image is already
unspecified for things like infinities and NaNs.
@end{TheProof}
@begin{Reason}
This is to allow implementations to define full support for IEEE
arithmetic.
See also the similar permission for Get in
@RefSecNum{Input-Output for Real Types}.
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0182-1],ARef=[AI05-0262-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[An implementation may extend the Wide_Wide_Value,
Wide_Value, and Value attributes of a character type to accept
strings of the form @ldquote@;Hex_@i<hhhhhhhh>@rdquote (ignoring case) for any
character (not just the ones for which Wide_Wide_Image would produce that form
@em see @RefSecNum{Character Types}), as well as three-character strings of
the form @ldquote@;'@i<X>'@rdquote, where @i<X> is any character, including
nongraphic characters.]}
@end{ImplPerm}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0228-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a scalar type,
the following language-defined representation aspect
may be specified with an @nt{aspect_specification} (see
@RefSecNum{Aspect Specifications}):]}
@begin{Description}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[Default_Value@\This aspect
shall be specified by a static expression, and that
expression shall be explicit, even if the aspect has a boolean type.
Default_Value shall be specified only on a
@nt{full_type_declaration}.@AspectDefn{Default_Value}]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The part about requiring an explicit expression is
  to disallow omitting the value for this aspect, which would otherwise be
  allowed by the rules of @RefSecNum{Aspect Specifications}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is a representation aspect in order to
  disallow specifying it on a derived type that has inherited primitive
  subprograms; that is necessary as the sizes of @key[out] parameters could be
  different whether or not a Default_Value is specified (see @RefSecNum{Parameter Associations}).]}
@end{Reason}
@ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Default_Value],
  Text=[@ChgAdded{Version=[3],Text=[Default value for a scalar subtype.]}]}
@end{Description}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0228-1]}
@ChgAdded{Version=[3],Text=[If a derived type with no primitive subprograms
inherits a boolean Default_Value aspect, the aspect may be specified to have any
value for the derived type.]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This overrides the
  @RefSecNum{Aspect Specifications} rule that says that a boolean aspect
with a value True cannot be changed.]}
@end{Reason}
@end{StaticSem}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0228-1]}
@ChgAdded{Version=[3],Text=[The expected type for the @nt{expression}
specified for the Default_Value aspect is the type defined by the
@nt{full_type_declaration} on which it
appears.@PDefn2{Term=[expected type],Sec=[@nt{expression} of a Default_Value aspect]}]}
@end{Resolution}

@begin{Notes}
The evaluation of S'First or S'Last never raises an exception.
If a scalar subtype S has a nonnull range, S'First and S'Last
belong to this range. These values can, for example, always be
assigned to a variable of subtype S.
@begin(Discussion)
  This paragraph addresses an issue that came up with Ada 83,
  where for fixed point types, the end points of the range
  specified in the type definition were not necessarily within
  the base range of the type. However, it was later clarified (and
  we reconfirm it in @RefSec(Fixed Point Types)) that the First and
  Last attributes reflect the true bounds chosen for the type, not the
  bounds specified in the type definition (which might be outside
  the ultimately chosen base range).
@end(Discussion)

For a subtype of a scalar type, the result delivered by the attributes
Succ, Pred, and Value might not belong to the subtype; similarly,
the actual parameters
of the attributes Succ, Pred, and Image need not belong to the subtype.

For any value V (including any nongraphic character) of an
enumeration subtype S, S'Value(S'Image(V)) equals V,
as @Chg{Version=[2],New=[do],Old=[does]} S'Wide_Value(S'Wide_Image(V))@Chg{Version=[2],
New=[ and S'Wide_Wide_Value(S'Wide_Wide_Image(V))],Old=[]}.
@Chg{Version=[2],New=[None of these
expressions],Old=[Neither expression]} ever @Chg{Version=[2],New=[raise],Old=[raises]}
Constraint_Error.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of ranges:)
@begin{Example}
-10 .. 10
X .. X + 1
0.0 .. 2.0*Pi
Red .. Green     --@RI[ see @RefSecNum{Enumeration Types}]
1 .. 0           --@RI[ a null range]
Table'Range      --@RI[ a range attribute reference (see @RefSecNum{Array Types})]

@end{Example}
@Leading@keepnext@i(Examples of range constraints:)
@begin{Example}
@key(range) -999.0 .. +999.0
@key(range) S'First+1 .. S'Last-1
@end{Example}
@end{Examples}

@begin{Incompatible83}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{To be consistent with 8652/0006}
@Defn{incompatibilities with Ada 83}
S'Base is no longer defined for nonscalar types.
One conceivable existing use of S'Base for nonscalar types is
S'Base'Size where S is a generic formal private type.
However, that is not generally useful because the actual
subtype corresponding to S might be a constrained array
or discriminated type, which would mean that S'Base'Size might
very well overflow (for example, S'Base'Size where S is
a constrained subtype of String will generally be 8 * (Integer'Last + 1)).
For derived discriminated types that are packed, S'Base'Size might not even
be well defined if the first subtype is constrained, thereby allowing
some amount of normally required @lquotes@;dope@rquotes@; to have been squeezed out
in the packing. Hence our conclusion is that S'Base'Size is
not generally useful in a generic, and does not justify keeping
the attribute Base for nonscalar types just so it can be used
as a @Chg{New=[@nt{prefix}],Old=[prefix]}.

@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The attribute S'Base for a scalar subtype is now permitted
anywhere a @nt{subtype_mark} is permitted.
S'Base'First .. S'Base'Last
is the base range of the type.
Using an @nt{attribute_@!definition_@!clause},
one cannot specify any subtype-specific attributes
for the subtype denoted by S'Base
(the base subtype).

The attribute S'Range is now allowed for scalar subtypes.

The attributes S'Min and S'Max are now defined, and made available for all
scalar types.

The attributes S'Succ, S'Pred, S'Image, S'Value, and S'Width are
now defined for real types as well as discrete types.

Wide_String versions of S'Image and S'Value are defined.
These are called S'Wide_Image and S'Wide_Value to avoid
introducing ambiguities involving uses of these attributes
with string literals.
@end{Extend83}

@begin{DiffWord83}
We now use the syntactic category @nt<range_attribute_reference> since
it is now syntactically distinguished from other attribute references.

The definition of S'Base has been moved here from
3.3.3 since it now applies only to scalar types.

More explicit rules are provided for nongraphic characters.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The attributes Wide_Wide_Image, Wide_Wide_Value, and Wide_Wide_Width are new.
  Note that Wide_Image and Wide_Value are now defined in terms of
  Wide_Wide_Image and Wide_Wide_Value, but the image of types other than
  characters have not changed.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[
  The Wide_Image and Wide_Value attributes are now defined in terms of
  Wide_Wide_Image and Wide_Wide_Value, but the images of numeric types
  have not changed.]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0181-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction:>
  Soft hyphen (code point 173) is nongraphic in ISO/IEC 10646:2011 (and also
  in the 2003 version of that standard). Thus, we
  have given it the language-defined name @i{soft_hyphen}. This changes the
  result of Character'Image (and all of the related types and Image attributes)
  for this character, and changes the behavior of Character'Value (and all of
  the related types and Value attributes) for this character, and
  (in unusual circumstances), changes the result for Character'Width (and all
  of the related types and Width attributes). The vast majority of programs
  won't see any difference, as they are already prepared to handle nongraphic
  characters.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0182-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:>
  Added an @ImplPermTitle to let Wide_Wide_Value, Wide_Value, and Value accept
  strings in the form of literals containing nongraphic characters and
  "Hex_hhhhhhhh" for Latin-1 and graphic characters. These were required to
  raise Constraint_Error in Ada 2005. Since these attributes aren't very
  useful, implementations were inconsistent as to whether these were accepted,
  and since code that would care why the attribute failed seems unlikely,
  this should not be a problem in practice.]}
@end{Inconsistent2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0228-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The new aspect Default_Value allows defining implicit initial values (see
  @RefSecNum{Object Declarations}) for scalar types.]}
@end{Extend2005}



@LabeledSubClause{Enumeration Types}

@begin{Intro}
@Redundant[@Defn{enumeration type}
An @nt<enumeration_type_definition> defines an enumeration type.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<enumeration_type_definition>,rhs="
   (@Syn2{enumeration_literal_specification} {, @Syn2{enumeration_literal_specification}})"}


@Syn{lhs=<enumeration_literal_specification>,
  rhs=" @Syn2{defining_identifier} | @Syn2{defining_character_literal}"}

@Syn{lhs=<defining_character_literal>,rhs="@Syn2{character_literal}"}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0227-1]}
The @nt<defining_identifier>s@Chg{Version=[3],New=[ in upper case],Old=[]}
@Redundant[and@Chg{Version=[3],New=[ the],Old=[]}
@nt<defining_character_literal>s] listed in an
@nt<enumeration_type_definition> shall be distinct.]
  @begin{TheProof}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0227-1]}
    @Chg{Version=[3],New=[For character literals, this],Old=[This]}
    is a ramification of the normal disallowance
    of homographs explicitly declared immediately in the same
    declarative region.
  @end{TheProof}
  @begin{Reason}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0227-1]}
    @ChgAdded{Version=[3],Text=[To ease implementation of the attribute
    Wide_Wide_Value, we require that all enumeration literals have distinct
    images.]}
  @end{Reason}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0006-1]}
@Defn{enumeration literal}
Each @nt<enumeration_literal_specification> is the explicit declaration
of the corresponding @i(enumeration literal): it declares
a parameterless function,
whose defining name is the @nt<defining_@!identifier>
or @nt<defining_@!character_@!literal>, and whose result
@Chg{Version=[3],New=[subtype is the base subtype of],Old=[type
is]} the enumeration type.
@begin{Reason}
  This rule defines the profile of the enumeration literal,
  which is used in the various types of conformance.
@end{Reason}
@begin{Ramification}
  The parameterless function associated with an enumeration literal
  is fully defined by the @nt<enumeration_type_definition>;
  a body is not permitted for it,
  and it never fails the Elaboration_Check when called.
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0006-1]}
  @ChgAdded{Version=[3],Text=[The result subtype is primarily a concern
  when an enumeration literal is used as the @nt{expression} of a
  case statement, due to the full coverage requirement based on the
  nominal subtype.]}
@end{Discussion}

Each enumeration literal corresponds to a distinct value
of the enumeration type, and to a distinct position number.
@PDefn2{Term=[position number], Sec=(of an enumeration value)}
The position number of the value of
the first listed enumeration literal
is zero; the position number of the value of each
subsequent enumeration literal
is one more than that of its predecessor in the list.

@redundant[The predefined order relations between values of
the enumeration type follow the
order of corresponding position numbers.]

@redundant[@PDefn2{Term=[overloaded], Sec=(enumeration literal)}
If the same @nt<defining_identifier> or
@nt<defining_character_literal> is specified in more than one
@nt<enumeration_@!type_@!definition>, the corresponding enumeration literals
are said to be @i(overloaded). At any place where an overloaded
enumeration literal occurs in the text of a program, the type
of the enumeration literal has to be determinable from the context
(see @RefSecNum(The Context of Overload Resolution)).]
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(enumeration_type_definition)}
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
The elaboration of an @nt<enumeration_type_definition> creates
the enumeration type and its first subtype,
which is constrained to the base range of the type.
@begin{Ramification}
The first subtype of a discrete type is always constrained,
except in the case of a derived type whose parent subtype
is Whatever'Base.
@end{Ramification}

When called, the parameterless function associated with an enumeration literal
returns the corresponding value of the enumeration type.
@end{RunTime}

@begin{Notes}
If an enumeration literal occurs in a context that does not
otherwise suffice to determine the type of the literal, then qualification
by the name of the enumeration type is one way to resolve
the ambiguity (see @RefSecNum(Qualified Expressions)).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of enumeration types and subtypes: )
@begin(Example)
@key(type) Day    @key(is) (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
@key(type) Suit   @key(is) (Clubs, Diamonds, Hearts, Spades);
@key(type) Gender @key(is) (M, F);
@key(type) Level  @key(is) (Low, Medium, Urgent);
@key(type) Color  @key(is) (White, Red, Yellow, Green, Blue, Brown, Black);
@key(type) Light  @key(is) (Red, Amber, Green); --@RI[ Red and Green are overloaded]

@key(type) Hexa   @key(is) ('A', 'B', 'C', 'D', 'E', 'F');
@key(type) Mixed  @key(is) ('A', 'B', '*', B, None, '?', '%');

@key(subtype) Weekday @key(is) Day   @key(range) Mon .. Fri;
@key(subtype) Major   @key(is) Suit  @key(range) Hearts .. Spades;
@key(subtype) Rainbow @key(is) Color @key(range) Red .. Blue;  --@RI[  the Color Red, not the Light]
@end(Example)
@end{Examples}

@begin{DiffWord83}
The syntax rule for @nt{defining_character_literal} is new.
It is used for the defining occurrence of a @nt{character_literal},
analogously to @nt{defining_identifier}.
Usage occurrences use the @nt{name} or @nt{selector_name}
syntactic categories.

We emphasize the fact that an enumeration literal denotes
a function, which is called to produce a value.
@end{DiffWord83}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0227-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Required that all enumeration
  literals in a type have distinct images; this might not be the case since
  upper case conversion can map distinct characters to the same upper case
  character. This can only happen for identifiers using Unicode characters first
  allowed by Ada 2005; moreover, the original definition of Ada 2005 was
  confused and appeared to require inconsistent results from the Image
  attribute, so implementations that allowed problematic cases are rare; the
  problematic cases are very rare; so it is expected that this change would
  only affect test programs.]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0006-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Defined the result
  subtype of an enumeration literal to close a minor language hole.]}
@end{DiffWord2005}


@LabeledSubClause{Character Types}

@begin{StaticSem}
@Defn{character type}
An enumeration type is said to be a @i(character type) if at least
one of its enumeration literals is a @nt<character_literal>.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0181-1],ARef=[AI05-0262-1],ARef=[AI05-0266-1]}
@Defn{Latin-1}
@Defn{BMP}
@Chg{Version=[2],New=[@Chg{Version=[3],New=[@Defn{ISO/IEC 10646:2011}],
Old=[@Defn{ISO/IEC 10646:2003}]}],Old=[@Defn{ISO 10646}]}
@Defn{Character}
The predefined type Character is a character type whose values
correspond to the 256 code @Chg{Version=[3],New=[points],Old=[positions]}
of Row 00 (also known as Latin-1) of the
@Chg{Version=[2],New=[ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]}],Old=[ISO 10646]}
Basic Multilingual Plane (BMP).
Each of the graphic characters of Row 00 of the BMP has
a corresponding @nt<character_literal> in Character.
Each of the nongraphic @Chg{Version=[3],New=[characters],Old=[positions]}
of Row 00@Chg{Version=[3],New=[],Old=[ (0000-001F and 007F-009F)]}
has a corresponding language-defined name, which is not usable as an
enumeration literal,
but which is usable with the attributes @Chg{Version=[2],New=[Image,
Wide_Image, Wide_Wide_Image, Value, Wide_Value, and Wide_Wide_Value],
Old=[(Wide_)Image and (Wide_)Value]};
these names are given in the definition of type Character
in @RefSec{The Package Standard}, but are set in @i{italics}.
@Defn2{Term=[italics],Sec=(nongraphic characters)}
@Chg{Version=[3],New=[@PDefn2{Term=[code point],Sec=[for characters]}],Old=[]}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[@i{Code point} is defined in ISO/IEC 10646:2011.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@Defn{Wide_Character}
@Defn{BMP}
@Chg{Version=[2],New=[@Chg{Version=[3],New=[@Defn{ISO/IEC 10646:2011}],
Old=[@Defn{ISO/IEC 10646:2003}]}],Old=[@Defn{ISO 10646}]}
The predefined type Wide_Character is a character type whose
values correspond to the 65536 code
@Chg{Version=[3],New=[points],Old=[positions]} of the @Chg{Version=[2],
New=[ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]}],Old=[ISO 10646]}
Basic Multilingual Plane (BMP).
Each of the graphic characters of the BMP has
a corresponding @nt<character_literal> in Wide_Character.
The first 256 values of Wide_Character
have the same @nt<character_literal> or language-defined
name as defined for Character. @Chg{Version=[2],New=[Each of the
@ntf{graphic_character}s has],Old=[The last 2 values
of Wide_Character correspond to the nongraphic
positions FFFE and FFFF of the BMP,
and are assigned
the language-defined names @i(FFFE) and @i(FFFF). As with the other
language-defined names for nongraphic characters,
the names @i(FFFE) and @i(FFFF) are usable only with the attributes
(Wide_)Image and (Wide_)Value; they are not usable as enumeration
literals.
All other values of Wide_Character are considered graphic characters,
and have]} a corresponding @nt<character_literal>.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[@Defn{Wide_Wide_Character}
@Chg{Version=[2],New=[@Chg{Version=[3],New=[@Defn{ISO/IEC 10646:2011}],
Old=[@Defn{ISO/IEC 10646:2003}]}],Old=[@Defn{ISO 10646}]}
The predefined type Wide_Wide_Character is a character type whose values
correspond to the 2147483648 code @Chg{Version=[3],New=[points],Old=[positions]}
of the ISO/IEC 10646:@Chg{Version=[3],New=[2011],Old=[2003]} character set.
Each of the @ntf{graphic_character}s
has a corresponding @nt{character_literal} in
Wide_Wide_Character. The first 65536 values of Wide_Wide_Character have the
same @nt{character_literal} or language-defined name as defined for
Wide_Character.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00285-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
@ChgAdded{Version=[2],Text=[The characters whose code
@Chg{Version=[3],New=[point],Old=[position]} is larger
than 16#FF# and which are not @ntf{graphic_character}s have language-defined
names which are formed by appending to the string "Hex_" the
representation of their code @Chg{Version=[3],New=[point],Old=[position]}
in hexadecimal as eight extended digits.
As with other language-defined names, these names are usable only with the
attributes (Wide_)Wide_Image and (Wide_)Wide_Value; they are not usable as
enumeration literals.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00285-01]}
  The language-defined names are not usable as
  enumeration literals to avoid "polluting" the name space. Since
  Wide_Character @Chg{Version=[2],New=[and Wide_Wide_Character ],Old=[]}are
  defined in Standard, if the @Chg{Version=[2],New=[language-defined ],
  Old=[]}names @Chg{Version=[2],New=[],Old=[FFFE and FFFF ]}were usable
  as enumeration literals, they would hide other nonoverloadable declarations
  with the same names in @key[use]-d packages.]}

  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
  @ChgDeleted{Version=[2],Text=[ISO 10646 has not defined the meaning of
  all of the code positions
  from 0100 through FFFD, but they are all considered graphic characters by
  Ada to simplify the implementation, and to allow for revisions to ISO 10646.
  In ISO 10646, FFFE and FFFF are special, and will never be associated
  with graphic characters in any revision.]}
@end{Reason}
@end{StaticSem}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
@ChgDeleted{Version=[2],Text=[@Defn{localization}
In a nonstandard mode, an implementation may provide
other interpretations for the predefined types Character and
Wide_Character@Redundant[, to conform to local conventions].]}
@end{ImplPerm}
@begin{NotIso}
@ChgAdded{Version=[2],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 6 and 7
were deleted.>}]}@Comment{This message should be deleted if the paragraphs
are ever renumbered.}
@end{NotIso}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00285-01]}
@ChgDeleted{Version=[2],Text=[@Defn{localization}
If an implementation supports a mode with alternative interpretations
for Character and Wide_Character, the set of graphic characters
of Character should nevertheless remain
a proper subset of the set of graphic characters of Wide_Character.
Any character set @lquotes@;localizations@rquotes@; should be reflected in the results of
the subprograms defined in the language-defined package Characters.Handling
(see @RefSecNum{Character Handling}) available in such a mode.
In a mode with an alternative interpretation of Character, the
implementation should also support a corresponding change in what is
a legal @ntf<identifier_letter>.]}
@ChgNote{We won't add an ChgImplAdvice here, because we would need to add
and remove it in the same command.}
@end{ImplAdvice}

@begin{Notes}
The language-defined library package Characters.Latin_1
(see @RefSecNum(The Package Characters.Latin_1))
includes the declaration of constants
denoting control characters, lower case characters, and special characters
of the predefined type Character.
@begin{Honest}
  The package ASCII does the same, but only for the first
  128 characters of Character. Hence, it is an obsolescent
  package, and we no longer mention it here.
@end{Honest}

A conventional character set such as @i(EBCDIC) can be declared as
a character type; the internal codes of the characters can be specified
by an @nt<enumeration_representation_clause> as explained in
clause @RefSecNum(Enumeration Representation Clauses).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Example of a character type: )
@begin(Example)
@key(type) Roman_Digit @key(is) ('I', 'V', 'X', 'L', 'C', 'D', 'M');
@end(Example)
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
The declaration of Wide_Character in package Standard hides
use-visible declarations with the same defining identifier.
In the unlikely event that an Ada 83 program had depended on
such a use-visible declaration, and the program remains
legal after the substitution of Standard.Wide_Character,
the meaning of the program will be different.
@end{Inconsistent83}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
The presence of Wide_Character in package Standard means that
an expression such as
@begin(Example)
'a' = 'b'
@end(Example)

is ambiguous in Ada 95, whereas in Ada 83 both
literals could be resolved to be of type Character.

The change in visibility rules (see @RefSecNum(Literals))
for character literals means
that additional qualification might be necessary to resolve
expressions involving overloaded subprograms and
character literals.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
The type Character has been extended to have 256 positions,
and the type Wide_Character has been added.
Note that this change was already approved by the ARG
for Ada 83 conforming compilers.

The rules for referencing character literals are changed
(see @RefSecNum(Literals)),
so that the declaration of the character type need
not be directly visible to use its literals,
similar to @b(null) and string literals.
Context is used to resolve their type.
@end{Extend83}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  Ada 95 defined most characters in Wide_Character to be graphic characters,
  while Ada 2005 uses the categorizations from ISO-10646:2003. It also
  provides language-defined names for all nongraphic characters. That
  means that in Ada 2005, Wide_Character'Wide_Value will raise Constraint_Error
  for a string representing a @nt{character_literal} of a nongraphic character,
  while Ada 95 would have accepted it. Similarly, the result of
  Wide_Character'Wide_Image will change for such nongraphic characters.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00395-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[2],Text=[The language-defined names FFFE and FFFF were
  replaced by a consistent set of language-defined names for all nongraphic
  characters with @Chg{Version=[3],New=[code points],Old=[positions]}
  greater than 16#FF#. That means that
  in Ada 2005, Wide_Character'Wide_Value("FFFE") will raise Constraint_Error
  while Ada 95 would have accepted it. Similarly, the result of
  Wide_Character'Wide_Image will change for the position numbers 16#FFFE#
  and 16#FFFF#. It is very unlikely that this will matter in practice,
  as these names do not represent @Chg{Version=[3],New=[usable],Old=[useable]}
  characters.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01],ARef=[AI95-00395-01]}
  @ChgAdded{Version=[2],Text=[Because of the previously mentioned changes to
  the Wide_Character'Wide_Image of various character values, the value of
  attribute Wide_Width will change for some subtypes of Wide_Character.
  However, the new language-defined names were chosen so that the value of
  Wide_Character'Wide_Width itself does not change.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[The declaration of Wide_Wide_Character in
  package Standard hides use-visible declarations with the same defining
  identifier. In the (very) unlikely event that an Ada 95 program had
  depended on such a use-visible declaration, and the program remains
  legal after the substitution of Standard.Wide_Wide_Character,
  the meaning of the program will be different.]}
@end{Inconsistent95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The type Wide_Wide_Character is new.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgAdded{Version=[2],Text=[Characters are now defined in terms of
  the entire ISO/IEC 10646:2003 character set.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00285-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0248-1]}
  @ChgAdded{Version=[2],Text=[We dropped the @ImplAdviceTitle for
  @Chg{Version=[3],New=[nonstandard],Old=[non-standard]} interpretation of
  character sets; an implementation can do what it wants in a
  @Chg{Version=[3],New=[nonstandard],Old=[non-standard]} mode, so there isn't
  much point to any advice.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0181-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Removed the position numbers
  of nongraphic characters from the text, as it is wrong and thus
  misleading.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[Changed "code position" to "code point"
  consistently throughout the standard, as ISO/IEC 10646:2011 prefers
  "code point" and we are referring to the definition in that Standard.
  This change also reduces confusion between "code point"
  and "position number"; while these have the same values for the predefined
  character types, there is no required relationship for other character
  types.@PDefn2{Term=[code point],Sec=[for characters]}]}
@end{DiffWord2005}


@LabeledSubClause{Boolean Types}

@begin{StaticSem}
@Defn{Boolean}
There is a predefined enumeration type named Boolean,
@Redundant[declared in the visible part of package Standard].
@Defn{False}
@Defn{True}
It has the two enumeration literals False and True ordered
with the relation False < True.
@Defn{boolean type}
Any descendant of the predefined type Boolean is called
a @i(boolean) type.
@begin{ImplNote}
  An implementation is not required to support
  enumeration representation clauses on boolean types that
  impose an unacceptable implementation burden.
  See @RefSec(Enumeration Representation Clauses).
  However, it is generally straightforward to support representations
  where False is zero and True is 2**n @en@; 1 for some n.
@end{ImplNote}
@end{StaticSem}

@LabeledSubClause{Integer Types}

@begin{Intro}
@Defn{integer type}
@Defn{signed integer type}
@Defn{modular type}
An @nt<integer_type_definition> defines an integer type;
it defines either a @i(signed)
integer type, or a @i(modular) integer type.
The base range
of a signed integer type includes at
least the values of the specified
range.
A modular type is an integer type with all arithmetic modulo
a specified positive @i(modulus);
such a type corresponds to an unsigned
type with wrap-around semantics.
@IndexSee{Term=[unsigned type],See=(modular type)}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<integer_type_definition>,
  rhs="@Syn2{signed_integer_type_definition} | @Syn2{modular_type_definition}"}

@Syn{lhs=<signed_integer_type_definition>,
rhs="@key(range) @SynI{static_}@Syn2{simple_expression} .. @SynI{static_}@Syn2{simple_expression}"}
@begin{Discussion}
  We don't call this a @nt<range_constraint>,
  because it is rather different @em not only is
  it required to be static, but the associated overload resolution rules are
  different than for normal range constraints. A similar comment applies to
  @nt{real_range_specification}.
  This used to be @ntf<integer_range_specification> but when we
  added support for modular types, it seemed overkill to have three levels
  of syntax rules, and just calling these
  @ntf<signed_integer_range_specification>
  and @ntf<modular_range_specification> loses the fact that they
  are defining different classes of types, which is important for
  the generic type matching rules.
@end{Discussion}

@Syn{lhs=<modular_type_definition>,
  rhs="@key(mod) @SynI{static_}@Syn2{expression}"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(signed_integer_type_definition simple_expression)}
Each @nt<simple_expression> in a
@nt<signed_integer_type_definition> is expected to be of any integer type;
they need not be of the same type.
@PDefn2{Term=[expected type],
  Sec=(modular_type_definition expression)}
The @nt<expression> in a
@nt<modular_type_definition> is likewise expected to be of any integer type.
@end{Resolution}

@begin{Legality}
The @nt<simple_expression>s of a
@nt<signed_integer_type_definition>
shall be static, and their values shall be in the
range System.Min_Int .. System.Max_Int.

@Defn2{Term=[modulus], Sec=(of a modular type)}
@Defn{Max_Binary_Modulus}
@Defn{Max_Nonbinary_Modulus}
The @nt<expression> of a @nt<modular_type_definition> shall be static,
and its value (the @i(modulus)) shall be positive,
and shall be no greater than System.Max_Binary_Modulus if a power of 2,
or no greater than System.Max_Nonbinary_Modulus if not.
@begin(Reason)
  For a 2's-complement machine, supporting nonbinary moduli greater
  than System.Max_Int can be quite difficult, whereas essentially any
  binary moduli are straightforward to support, up to 2*System.Max_Int+2,
  so this justifies having two separate limits.
@end(Reason)
@end{Legality}

@begin{StaticSem}
The set of values for a signed integer type is the (infinite)
set of mathematical
integers@Redundant[, though only values of the base range of the type
are fully supported for run-time operations].
The set of values for a modular integer type are the values from
0 to one less than the modulus,
inclusive.

@PDefn2{Term=[base range], Sec=(of a signed integer type)}
A @nt<signed_integer_type_definition> defines an integer type whose
base range
includes at least the values of the @nt<simple_expression>s and
is symmetric about zero, excepting possibly an extra negative value.
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
A @nt<signed_integer_type_definition> also defines a constrained first
subtype of the type, with a range whose bounds are given by
the values of the @nt<simple_expression>s, converted to the type being defined.
@begin{ImplNote}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  The base range of a signed integer type might be much larger than is
  necessary to satisfy the @Chg{Version=[2],New=[above],Old=[aboved]}
  requirements.
@end{ImplNote}
@begin{Honest}
  @ChgRef{Version=[1],Kind=[Added]}@ChgNote{This is discussed in AC-00002, which we can't reference here}
  @ChgAdded{Version=[1],Text=[The conversion mentioned above is not
  an @i{implicit subtype conversion} (which is something that happens at
  overload resolution, see
  @RefSecNum{Type Conversions}), although it happens implicitly. Therefore,
  the freezing rules are not invoked on the type (which is important so that
  representation items can be given for the type).
  @PDefn2{Term=[subtype conversion],Sec=(bounds of signed integer type)}]}
@end{Honest}

@PDefn2{Term=[base range], Sec=(of a modular type)}
A @nt<modular_type_definition> defines a modular type whose base range
is from zero to one less than the given modulus.
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
A @nt<modular_type_definition> also defines a constrained first
subtype of the type with a range that is the same as the base range of
the type.

@Defn{Integer}
There is a predefined signed integer subtype named
Integer@Redundant[,
declared in the visible part of
package Standard].
It is constrained to the base range of its type.
@begin{Reason}
  Integer is a constrained subtype, rather than an unconstrained
  subtype. This means that on assignment to an object of subtype Integer,
  a range check is required. On the other hand, an object of subtype
  Integer'Base is unconstrained, and no range check (only overflow check)
  is required on assignment. For example, if the object is held in an
  extended-length register, its value might be outside of
  Integer'First .. Integer'Last. All parameter and result subtypes
  of the predefined integer operators are of such unconstrained subtypes,
  allowing extended-length registers to be used as operands or
  for the result.
  In an earlier version of Ada 95, Integer was unconstrained. However,
  the fact that certain Constraint_Errors might be omitted or appear
  elsewhere was felt to be an undesirable upward inconsistency in this case.
  Note that for Float, the opposite conclusion was reached, partly because
  of the high cost of performing range checks when not actually necessary.
  Objects of subtype Float are unconstrained, and no range checks, only
  overflow checks, are performed for them.
@end{Reason}

@Leading@Defn{Natural}
@Defn{Positive}
Integer has two predefined subtypes,
@Redundant[declared in the visible part of package Standard:]
@begin{Example}
@key[subtype] Natural  @key[is] Integer @key[range] 0 .. Integer'Last;
@key[subtype] Positive @key[is] Integer @key[range] 1 .. Integer'Last;
@end{Example}

@Defn{root_integer}
@Defn{Min_Int}
@Defn{Max_Int}
A type defined by an @nt<integer_type_definition> is implicitly
derived from @i(root_integer), an anonymous
predefined (specific) integer type, whose base
range is System.Min_Int .. System.Max_Int.
However, the base range of the new type is not inherited from
@i{root_integer}, but is instead determined by the range or modulus
specified by the @nt{integer_type_definition}.
@PDefn{universal_integer}
@Defn{integer literals}
@Redundant[Integer literals are all of the type @i(universal_integer),
the universal type (see @RefSecNum(Derivation Classes)) for the
class rooted at @i(root_integer), allowing their use with
the operations of any integer type.]
@begin{Discussion}
  This implicit derivation is not considered exactly equivalent to
  explicit derivation via a @nt<derived_type_definition>. In particular,
  integer types defined via a @nt<derived_type_definition> inherit their
  base range from their parent type. A type defined by
  an @nt<integer_type_definition> does not necessarily inherit
  its base range from @i(root_integer).
  It is not specified whether the implicit derivation from
  @i(root_integer) is direct or indirect, not that it really matters.
  All we want is for all integer types to be descendants of @i(root_integer).

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0099],ARef=[AI95-00152-01]}
  @Chg{New=[Note that this derivation does not imply any inheritance of
  subprograms. Subprograms are inherited only for types derived by a
  @nt{derived_@!type_@!definition} (see @RefSecNum{Derived Types and Classes}),
  or a @nt{private_@!extension_@!declaration} (see
  @RefSecNum{Private Types and Private Extensions}, @RefSecNum{Private Operations},
  and @RefSecNum{Formal Private and Derived Types}).],Old=[]}
@end{Discussion}
@begin{ImplNote}
  It is the intent that even nonstandard integer
  types (see below) will be descendants of @i(root_integer), even though they
  might have a base range that exceeds that of @i(root_integer).
  This causes no problem for static calculations, which
  are performed without range restrictions
  (see @RefSecNum(Static Expressions and Static Subtypes)). However
  for run-time calculations, it is possible that Constraint_Error
  might be raised when using an operator of @i(root_integer)
  on the result of 'Val applied to a value of a nonstandard integer type.
@end{ImplNote}

@PDefn2{Term=[position number], Sec=(of an integer value)}
The @i(position number) of an integer value is equal to the value.

@begin{WideAbove}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00340-01]}
@Leading@keepnext@;For @PrefixType{every modular subtype S},
the following @Chg{Version=[2],New=[attributes are],Old=[attribute is]} defined:
@end{WideAbove}
@begin(description)
@ChgAttribute{Version=[2],Kind=[Added],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Mod>, ARef=[AI95-00340-01],
  Text=[@Chg{Version=[2],New=[S'Mod denotes a function with the following specification:],Old=[]}

@begin(Descexample)
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[@key{function} S'Mod (@RI{Arg} : @RI{universal_integer})
  @key{return} S'Base]}
@end(Descexample)

  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],NoPrefix=[T],Text=[This function returns @i{Arg}
  @key{mod} S'Modulus, as a value of the type of S.]}]}@Comment{end attribute Mod}

@Attribute{Prefix=<S>, AttrName=<Modulus>,
  Text=[S'Modulus yields the modulus of the type of S, as a value of the
        type @i(universal_integer).]}
@end(description)

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(integer_type_definition)}
The elaboration of an @nt<integer_type_definition> creates the
integer type and its first subtype.

For a modular type, if the result of the execution of a
predefined operator (see @RefSecNum(Operators and Expression Evaluation))
is outside the base range of the type, the result is reduced
modulo the modulus of the type to a value that is within the
base range of the type.

@IndexCheck{Overflow_Check}
@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
For a signed integer type,
the exception Constraint_Error is raised by the execution of
an operation that cannot deliver the correct result because
it is outside the base range of the type.
@redundant[@IndexCheck{Division_Check}
@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
For any integer type, Constraint_Error is raised by the operators
"/", "@key(rem)", and "@key(mod)" if the right operand is zero.]

@end{RunTime}

@begin{ImplReq}
@Defn{Integer}
In an implementation, the range of Integer shall include the range
@en@;2**15+1 .. +2**15@en@;1.

@Defn{Long_Integer}
If Long_Integer is predefined for an implementation, then its
range shall include the range @en@;2**31+1 .. +2**31@en@;1.

System.Max_Binary_Modulus shall be at least 2**16.
@end{ImplReq}

@begin{ImplPerm}
For the execution of a predefined operation of a signed integer type,
the implementation need not raise Constraint_Error if the result is
outside the base range of the type, so long as the correct result
is produced.
@begin{Discussion}
  Constraint_Error is never raised for operations on modular types,
  except for divide-by-zero (and @key[rem]/@key[mod]-by-zero).
@end{Discussion}

@Defn{Long_Integer}
@Defn{Short_Integer}
An implementation may provide additional predefined signed integer
types@Redundant[, declared in the visible part of Standard], whose first
subtypes have names of the form Short_Integer,
Long_Integer, Short_Short_Integer, Long_Long_Integer, etc.
Different predefined integer types are allowed to have the same base range.
However, the range of Integer should be no wider than that of Long_Integer.
Similarly, the range of Short_Integer (if provided) should be no wider
than Integer.
Corresponding recommendations apply to any other predefined integer types.
There need not be a named integer type corresponding to each
distinct base range supported by an implementation.
The range of each first subtype should be the base range of its type.
@ImplDef{The predefined integer types declared in Standard.}

@Defn{nonstandard integer type}
An implementation may provide @i(nonstandard integer types),
descendants of @i(root_integer) that are
declared outside of the specification of package Standard,
which need not have all the standard characteristics
of a type defined by an @nt<integer_type_definition>.
For example, a nonstandard integer type
might have an asymmetric base range
or it might not be allowed as
an array or loop index (a very long integer).
Any type descended from a nonstandard integer type is also nonstandard.
An implementation may place arbitrary restrictions on the use of such types;
it is implementation defined whether operators that are predefined
for @lquotes@;any integer type@rquotes@; are defined for a particular
nonstandard integer type.
@Redundant[In any case, such types are not permitted as
@nt{explicit_generic_actual_parameter}s for formal scalar types @em
see @RefSecNum(Formal Scalar Types).]
@ImplDef{Any nonstandard integer types and the operators defined for them.}

@PDefn2{Term=[one's complement], Sec=(modular types)}
For a one's complement machine, the high bound of the base range
of a modular type whose modulus is one less than a power of 2
may be equal to the modulus, rather than one less than the modulus.
It is implementation defined for which powers of 2, if any, this
permission is exercised.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0003],ARef=[AI95-00095-01]}
@Chg{New=[For a one's complement machine, implementations may support nonbinary
modulus values greater than System.Max_Nonbinary_Modulus. It is implementation
defined which specific values greater than System.Max_Nonbinary_Modulus, if
any, are supported.],Old=[]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added]}
@ChgAdded{Version=[1],Text=[On a one's complement machine, the natural full word
type would have
a modulus of 2**Word_Size@en@;1. However, we would want to allow the all-ones
bit pattern (which represents negative zero as a number) in logical operations.
These permissions are intended to allow that and the natural modulus value
without burdening implementations with supporting expensive modulus values.]}
@end{Reason}
@end{ImplPerm}

@begin{ImplAdvice}
@Defn{Long_Integer}
An implementation should support Long_Integer in addition to
Integer if the target machine supports 32-bit (or longer) arithmetic.
No other named integer subtypes are recommended for package Standard.
Instead, appropriate named integer subtypes should be provided in
the library package Interfaces
(see @RefSecNum{The Package Interfaces}).
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Long_Integer should be declared in Standard if the target supports
32-bit arithmetic. No other named integer subtypes should be declared in
Standard.]}]}
@begin{ImplNote}
To promote portability, implementations should explicitly declare the integer
(sub)types Integer and Long_Integer in Standard, and leave other
predefined integer types anonymous.
For implementations
that already support Byte_Integer, etc., upward compatibility
argues for keeping such declarations in Standard during the
transition period, but perhaps generating a warning on use.
A separate package Interfaces in the predefined environment
is available for pre-declaring types such as Integer_8, Integer_16, etc.
See @RefSecNum(The Package Interfaces).
In any case, if the user declares a subtype (first or not)
whose range fits in, for example, a byte, the implementation can
store variables of the subtype in a single byte, even if the
base range of the type is wider.
@end{ImplNote}

@PDefn2{Term=[two's complement],Sec=(modular types)}
An implementation for a two's complement machine should support
modular types with a binary modulus
up to System.Max_Int*2+2.
An implementation should support a nonbinary modulus up to Integer'Last.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[For a two's complement target, modular types with a binary modulus
up to System.Max_Int*2+2 should be supported. A nonbinary modulus up
to Integer'Last should be supported.]}]}
@begin{Reason}
  Modular types provide bit-wise "@key{and}", "@key{or}", "@key{xor}",
  and "@key{not}" operations.
  It is important for systems programming that these be available for all
  integer types of the target hardware.
@end{Reason}
@begin{Ramification}
  Note that on a one's complement machine,
  the largest supported modular type would normally have a nonbinary
  modulus. On a two's complement machine, the largest supported
  modular type would normally have a binary modulus.
@end{Ramification}
@begin{ImplNote}
  Supporting a nonbinary modulus greater than Integer'Last can
  impose an undesirable implementation burden on some machines.
@end{ImplNote}
@end{ImplAdvice}

@begin{Notes}
@PDefn{universal_integer}
@Defn{integer literals}
Integer literals are of the anonymous predefined
integer type @i(universal_integer). Other integer types
have no literals. However, the overload resolution rules
(see @RefSec(The Context of Overload Resolution))
allow expressions of the type @i(universal_integer)
whenever an integer type is expected.

The same arithmetic operators are predefined for all signed integer types
defined by a @nt<signed_integer_type_definition>
(see @RefSec(Operators and Expression Evaluation)).
For modular types, these same operators are predefined, plus
bit-wise logical operators (@key(and), @key(or), @key(xor), and @key(not)).
In addition, for the unsigned types declared in the language-defined
package Interfaces (see @RefSecNum(The Package Interfaces)),
functions are defined that provide
bit-wise shifting and rotating.

Modular types match a @nt{generic_formal_parameter_declaration} of the
form "@key(type) T @key(is mod) <>;";
signed integer types match "@key(type) T @key(is range) <>;"
(see @RefSecNum{Formal Scalar Types}).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of integer types and subtypes: )
@begin(Example)
@key(type) Page_Num  @key(is) @key(range) 1 .. 2_000;
@key(type) Line_Size @key(is) @key(range) 1 .. Max_Line_Size;

@key(subtype) Small_Int   @key(is) Integer   @key(range) -10 .. 10;
@key(subtype) Column_Ptr  @key(is) Line_Size @key(range) 1 .. 10;
@key(subtype) Buffer_Size @key(is) Integer   @key(range) 0 .. Max;

@key(type) Byte        @key(is) @key(mod) 256; --@RI[ an unsigned byte]
@key(type) Hash_Index  @key(is) @key(mod) 97;  --@RI[ modulus is prime]
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
An implementation is allowed to support any number of distinct
base ranges for integer types, even if fewer
integer types are explicitly declared in Standard.

Modular (unsigned, wrap-around) types are new.
@end{Extend83}

@begin{DiffWord83}
Ada 83's integer types are now called "signed" integer types,
to contrast them with "modular" integer types.

Standard.Integer, Standard.Long_Integer, etc., denote
constrained subtypes of predefined integer types, consistent
with the Ada 95 model that only subtypes have names.

We now impose minimum requirements on the base range of
Integer and Long_Integer.

We no longer explain integer type definition in terms
of an equivalence to a normal type derivation, except to say that all
integer types are by definition implicitly derived from @i(root_integer).
This is for various reasons.

First of all, the equivalence with a type derivation and a subtype
declaration was not perfect, and was the source of various AIs (for example,
is the conversion of the bounds static? Is a numeric type a derived
type with respect to other rules of the language?)

Secondly, we don't want to require that every integer size supported
shall have a corresponding named type in Standard. Adding named
types to Standard creates nonportabilities.

Thirdly, we don't want the set of types that match
a formal derived type "type T is new Integer;" to
depend on the particular underlying integer representation chosen
to implement a given user-defined integer type. Hence, we
would have needed anonymous integer types as parent types for
the implicit derivation anyway. We have simply chosen to identify
only one anonymous integer type @em @i(root_integer), and stated
that every integer type is derived from it.

Finally, the @lquotes@;fiction@rquotes@; that there were distinct
preexisting predefined types
for every supported representation breaks down for fixed point
with arbitrary smalls, and was never exploited for enumeration
types, array types, etc. Hence, there seems little benefit
to pushing an explicit equivalence between integer type
definition and normal type derivation.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00340-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  The Mod attribute is new. It eases mixing of signed and unsigned values in
  an expression, which can be difficult as there may be no type which
  can contain all of the values of both of the types involved.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0003],ARef=[AI95-00095-01]}
  @Chg{Version=[2],New=[@b<Corrigendum:> Added additional permissions for
  modular types on one's complement machines.],Old=[]}
@end{DiffWord95}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledSubClause{Operations of Discrete Types}

@begin{StaticSem}
@Leading@;For @PrefixType{every discrete subtype S},
the following attributes are defined:
@begin(description)
@AttributeLeading{Prefix=<S>, AttrName=<Pos>,
  Text=[S'Pos denotes a function with the following specification:
@begin(Descexample)
@b(function) S'Pos(@RI(Arg) : S'Base)
  @b(return) @RI(universal_integer)
@end(Descexample)

     @NoPrefix@;This function returns the position number of the value
     of @i(Arg), as a value of type @i(universal_integer).]}

@AttributeLeading{Prefix=<S>, AttrName=<Val>,
  Text=[S'Val denotes a function with the following specification:
@begin(Descexample)
@b(function) S'Val(@RI(Arg) : @RI(universal_integer))
  @b(return) S'Base
@end(Descexample)

     @NoPrefix@PDefn2{Term=(evaluation), Sec=(Val)}
     @Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
     This function returns a value of the type of S
     whose position number equals the value of @i(Arg).]}
     @IndexCheck{Range_Check}
     For the evaluation of a call on S'Val, if there
     is no value in the base range of its type with the given
     position number, Constraint_Error is raised.
     @begin{Ramification}

        By the overload resolution rules, a formal parameter of type
        @i(universal_integer) allows an actual parameter of any
        integer type.@end{ramification}
     @begin{Reason}
We considered allowing
        S'Val for a signed integer subtype S to return an out-of-range value,
        but since checks were required for enumeration and modular types
        anyway, the allowance didn't seem worth the complexity of the rule.@end{reason}
@end(description)
@EndPrefixType{}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0297-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For @PrefixType{every static
discrete subtype S for which there exists at least one value belonging to S
that satisfies any predicate of S},
the following attributes are defined:]}

@begin(Description)
@ChgAttribute{Version=[3],Kind=[Added],ChginAnnex=[T],
  Leading=<F>, Prefix=<S>, AttrName=<First_Valid>, ARef=[AI05-0297-1],
  Text=[@Chg{Version=[3],New=[S'First_Valid denotes the smallest value
        that belongs to S and satisfies the predicate of S.
        The value of this attribute is of the type of S.],Old=[]}]}

@ChgAttribute{Version=[3],Kind=[Added],ChginAnnex=[T],
  Leading=<F>, Prefix=<S>, AttrName=<Last_Valid>, ARef=[AI05-0297-1],
  Text=[@Chg{Version=[3],New=[S'Last_Valid denotes the largest value
        that belongs to S and satisfies the predicate of S. The value of
        this attribute is of the type of S.],Old=[]}]}
@end(Description)
@EndPrefixType{}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0297-1]}
@ChgAdded{Version=[3],Text=[@Redundant[First_Valid and Last_Valid
@nt{attribute_reference}s are always static expressions. Any explicit predicate
of S can only have been specified by a Static_Predicate aspect.]]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An @nt{attribute_reference} is static if the
  prefix is a static subtype (see @RefSecNum{Static Expressions and Static Subtypes}),
  (true by definition) and any arguments are static (there are none). Similarly,
  a dynamic predicate always makes a subtype nonstatic. QED.]}
@end{TheProof}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[We require there to be at least one value so that
  these are always values of the subtype. (This sidesteps the question of what
  to return for a subtype with no values.)]}
@end{Reason}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[These attributes are intended primarily for use in
  the case where the Static_Predicate aspect of S has been specified; First and
  Last are equivalent if these are allowed and there is no predicate.]}
@end{Discussion}
@end{StaticSem}

@begin{ImplAdvice}
For the evaluation of a call on S'Pos for an enumeration
subtype, if the value of the operand does not correspond
to the internal code for any enumeration literal of its
type
@Redundant[(perhaps due to an uninitialized variable)],
then the implementation should raise Program_Error.
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
This is particularly important for enumeration types with
noncontiguous internal codes specified by an
@nt<enumeration_@!representation_@!clause>.
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Program_Error should be raised for the evaluation of S'Pos for an
enumeration type, if the value of the operand does not correspond
to the internal code for any enumeration literal of the type.]}]}
@begin{Reason}
We say Program_Error here, rather than Constraint_Error,
because the main reason for such values is uninitialized variables,
and the normal way to indicate such a use (if detected) is to raise
Program_Error.
(Other reasons would involve the misuse of low-level features such as
Unchecked_Conversion.)
@end{Reason}
@end{ImplAdvice}

@begin{Notes}
Indexing and loop iteration use values of discrete types.

@PDefn2{Term=[predefined operations],Sec=(of a discrete type)}
The predefined operations of a discrete type include the assignment
operation, qualification, the membership tests, and the
relational operators; for a boolean type
they include the short-circuit control forms and the
logical operators; for an integer
type they include type conversion to and from other numeric types,
as well as the binary and unary adding operators @en@; and +, the multiplying
operators, the unary operator @key(abs),
and the exponentiation operator.
The assignment operation is described in @RefSecNum(Assignment Statements).
The other predefined operations are described in Section 4.

As for all types, objects of a discrete type
have Size and Address attributes (see @RefSecNum(Operational and Representation Attributes)).

@Leading@;For a subtype of a discrete type, the result delivered by the
attribute Val might not belong to the subtype; similarly, the actual parameter
of the attribute Pos need not belong to the subtype. The following relations
are satisfied (in the absence of an exception) by these attributes:
@begin(Example)
   S'Val(S'Pos(X)) = X
   S'Pos(S'Val(N)) = N
@end(Example)
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of attributes of discrete subtypes: )
@begin(Example)
--@RI[  For the types and subtypes declared in subclause @RefSecNum(Enumeration Types) the following hold: ]

--  Color'First   = White,   Color'Last   = Black
--  Rainbow'First = Red,     Rainbow'Last = Blue

--  Color'Succ(Blue) = Rainbow'Succ(Blue) = Brown
--  Color'Pos(Blue)  = Rainbow'Pos(Blue)  = 4
--  Color'Val(0)     = Rainbow'Val(0)     = White
@end(Example)
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The attributes S'Succ, S'Pred, S'Width, S'Image, and S'Value have
been generalized to apply to real types as well
(see @RefSec{Scalar Types}).
@end{Extend83}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0297-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  The attributes S'First_Valid and S'Last_Valid are new.]}
@end{Extend2005}


@LabeledSubClause{Real Types}

@begin{Intro}
@Defn{real type}
Real types provide approximations to the real numbers, with relative bounds
on errors for floating point types, and with absolute bounds for fixed
point types.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<real_type_definition>,rhs="
   @Syn2{floating_point_definition} | @Syn2{fixed_point_definition}"}
@end{Syntax}

@begin{StaticSem}
@Defn{root_real}
A type defined by a @nt<real_type_definition> is implicitly
derived from @i(root_real), an anonymous
predefined (specific) real type.
@Redundant[Hence, all real types, whether floating point or fixed point,
are in the derivation class rooted at @i(root_real).]
@begin{Ramification}
  It is not specified whether the derivation from @i(root_real) is
  direct or indirect, not that it really matters.
  All we want is for all real types to be descendants of @i(root_real).

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0099],ARef=[AI95-00152-01]}
  @Chg{New=[Note that this derivation does not imply any inheritance of
  subprograms. Subprograms are inherited only for types derived by a
  @nt{derived_@!type_@!definition} (see @RefSecNum{Derived Types and Classes}),
  or a @nt{private_@!extension_@!declaration} (see
  @RefSecNum{Private Types and Private Extensions}, @RefSecNum{Private Operations},
  and @RefSecNum{Formal Private and Derived Types}).],Old=[]}

@end{Ramification}

@redundant[@PDefn{universal_real}
@Defn{real literals}
Real literals are all of the type @i(universal_real),
the universal type (see @RefSecNum(Derivation Classes)) for the
class rooted at @i(root_real), allowing their use with
the operations of any real type.
@PDefn{universal_fixed}
Certain multiplying operators have a result type of @i(universal_fixed)
(see @RefSecNum{Multiplying Operators}),
the universal type for the class of fixed point types, allowing
the result of the multiplication or division to be used where any
specific fixed point type is expected.]
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(real_type_definition)}
The elaboration of a @nt<real_type_definition> consists of the
elaboration of the @nt<floating_point_definition> or the
@nt<fixed_point_definition>.
@end{RunTime}

@begin{ImplReq}
An implementation shall perform the run-time evaluation
of a use of a predefined operator of @i(root_real)
with an accuracy at least as great as that of any floating point
type definable by a @nt<floating_point_definition>.
@begin{Ramification}
  Static calculations using the operators of @i(root_real) are
  exact, as for all static calculations.
  See @RefSecNum(Static Expressions and Static Subtypes).
@end{Ramification}
@begin(ImplNote)
  The Digits attribute of the type used to represent @i(root_real)
  at run time is at least as great as that of any other floating
  point type defined by a @nt<floating_point_definition>,
  and its safe range includes that of any such floating point type
  with the same Digits attribute.
  On some machines, there might be real types with less accuracy but
  a wider range, and hence run-time calculations with @i(root_real)
  might not be able to accommodate all values that can be represented
  at run time in such floating point or fixed point types.
@end(ImplNote)
@end{ImplReq}

@begin{ImplPerm}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
@Redundant[For the execution of a predefined operation of a real type,
the implementation need not raise Constraint_Error if the result is
outside the base range of the type, so long as the correct result
is produced, or the Machine_Overflows attribute of the type is @Chg{Version=[2],
New=[False],Old=[false]}
(see @RefSecNum{Numeric Performance Requirements}).]

@Defn{nonstandard real type}
An implementation may provide @i(nonstandard real types),
descendants of @i(root_real) that are
declared outside of the specification of package Standard,
which need not have all the standard characteristics
of a type defined by a @nt<real_type_definition>. For
example, a nonstandard real type
might have an asymmetric or unsigned base range,
or its predefined operations might wrap around or @lquotes@;saturate@rquotes@; rather
than overflow (modular or saturating arithmetic), or it might not
conform to the accuracy
model (see @RefSecNum{Numeric Performance Requirements}).
Any type descended from a nonstandard real type is also nonstandard.
An implementation may place arbitrary restrictions on the use of such types;
it is implementation defined whether operators that are predefined
for @lquotes@;any real type@rquotes@; are defined for a particular nonstandard real type.
@Redundant[In any case, such types are not permitted as @nt{explicit_generic_actual_parameter}s for formal scalar types @em see @RefSecNum(Formal Scalar Types).]
@ImplDef{Any nonstandard real types and the operators defined for them.}
@end{ImplPerm}

@begin{Notes}
As stated, real literals are of the anonymous predefined
real type @i(universal_real). Other real types
have no literals. However, the overload resolution rules
(see @RefSecNum(The Context of Overload Resolution))
allow expressions of the type @i(universal_real)
whenever a real type is expected.

@end{Notes}

@begin{DiffWord83}
The syntax rule for @nt{real_type_definition} is modified to use
the new syntactic categories
@nt{floating_point_definition} and @nt{fixed_point_definition},
instead of @ntf{floating_point_constraint} and
@ntf{fixed_point_constraint}, because the semantics of a type
definition are significantly different than the semantics of a
constraint.

All discussion of model numbers, safe ranges, and machine numbers
is moved to @RefSecNum{Floating Point Types},
@RefSecNum{Operations of Floating Point Types}, and
@RefSecNum{Numeric Performance Requirements}.
Values of a fixed point type are now described as being multiples of
the @i(small) of the fixed point type, and we have no need for model
numbers, safe ranges, etc. for fixed point types.

@end{DiffWord83}

@LabeledSubClause{Floating Point Types}

@begin{Intro}
@Defn{floating point type}
For floating point types, the error bound is specified as a relative
precision by giving the required minimum number of significant decimal
digits.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<floating_point_definition>,rhs="
  @key{digits} @SynI{static_}@Syn2{expression} [@Syn2{real_range_specification}]"}

@Syn{lhs=<real_range_specification>,rhs="
  @key{range} @SynI{static_}@Syn2{simple_expression} .. @SynI{static_}@Syn2{simple_expression}"}
@end{Syntax}

@begin{Resolution}
@Defn2{Term=[requested decimal precision], Sec=(of a floating point type)}
The @i(requested decimal precision), which is the minimum
number of significant decimal digits required for the floating point type,
is specified by the
value of the @nt<expression> given after
the reserved word @key(digits).
@PDefn2{Term=[expected type], Sec=(requested decimal precision)}
This @nt<expression>
is expected to be of any integer type.

@PDefn2{Term=[expected type], Sec=(real_range_specification bounds)}
Each @nt<simple_expression> of a
@nt<real_range_specification> is expected to be of any real
type@Redundant[; the types need not be the same].
@end{Resolution}

@begin{Legality}
@Defn{Max_Base_Digits}
The requested decimal precision shall be specified by a static @nt<expression>
whose value is positive and no greater than
System.Max_Base_Digits.
Each @nt<simple_expression> of a @nt<real_range_specification>
shall also be static.
@Defn{Max_Digits}
If the @nt<real_range_specification> is omitted,
the requested decimal precision shall be no greater than System.Max_Digits.
@begin(Reason)
  We have added Max_Base_Digits to package System. It corresponds
  to the requested decimal precision of
  @i(root_real). System.Max_Digits
  corresponds to the maximum value for Digits that may be specified
  in the absence of a @nt<real_range_specification>, for upward
  compatibility. These might not be the same if @i<root_real>
  has a base range that does not include @PorM 10.0**(4*Max_Base_Digits).
@end(Reason)

A @nt<floating_point_definition> is illegal if the
implementation does not support a floating point type that
satisfies the requested decimal precision and range.
@ImplDef{What combinations of requested decimal precision and range
  are supported for floating point types.}
@end{Legality}

@begin{StaticSem}
The set of values for a floating point type is the (infinite) set of rational
numbers.
@Defn2{Term=[machine numbers], Sec=(of a floating point type)}
The @i(machine numbers) of a floating point type are the values
of the type that can be represented exactly in every
unconstrained variable of the type.
@PDefn2{Term=[base range], Sec=(of a floating point type)}
The base range (see @RefSecNum{Scalar Types})
of a floating point type is symmetric around zero,
except that it can include some extra negative values
in some implementations.@begin{ImplNote}
For example, if a 2's complement
  representation is used for the mantissa rather than a sign-mantissa or
  1's complement representation, then there is usually one extra
  negative machine number.@end{implnote}
@begin{Honest}

  If the Signed_Zeros attribute is True,
  then minus zero could in a sense be considered a value of the type.
  However, for most purposes, minus zero behaves the same as plus zero.

@end{Honest}

@Defn2{Term=[base decimal precision], Sec=(of a floating point type)}
The @i(base decimal precision) of a floating point type is the number
of decimal digits of precision representable in objects
of the type.
@Defn2{Term=[safe range], Sec=(of a floating point type)}
The @i(safe range) of a floating point type is that part of its
base range for which the accuracy corresponding to the base decimal precision
is preserved by all predefined operations.
@begin{ImplNote}
In most cases,
  the safe range and base range are the same.
  However, for some hardware, values near the boundaries of
  the base range might result in excessive
  inaccuracies or spurious overflows when used with
  certain predefined operations. For such hardware, the safe
  range would omit such values.@end{implnote}

@PDefn2{Term=[base decimal precision], Sec=(of a floating point type)}
A @nt<floating_point_definition> defines a floating point type
whose base decimal precision is no less than the requested
decimal precision.
@PDefn2{Term=[safe range], Sec=(of a floating point type)}
@PDefn2{Term=[base range], Sec=(of a floating point type)}
If a @nt<real_range_specification> is given,
the safe range of the floating point type (and hence, also its base range)
includes at least the
values of the simple expressions
given in the @nt<real_range_specification>.
If a @nt<real_range_specification> is not given,
the safe (and base) range of the type includes at least the values of the range
@en@;10.0**(4*D) .. +10.0**(4*D) where D is the requested decimal precision.
@Redundant[The safe range might include
other values as well. The attributes Safe_First and Safe_Last
give the actual bounds of the safe range.]

A @nt<floating_point_definition> also defines a first
subtype of the type.
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
If a @nt<real_range_specification> is given, then
the subtype is constrained to a range whose bounds are
given by a conversion of the values of the @nt<simple_expression>s
of the @nt<real_range_specification> to the type being defined.
Otherwise, the subtype is unconstrained.

@begin{Honest}
  @ChgRef{Version=[1],Kind=[Added]}@ChgNote{This is discussed in AC-00002, which we can't reference here}
  @ChgAdded{Version=[1],Text=[The conversion mentioned above is not an @i{implicit subtype
  conversion} (which is something that happens at overload resolution, see
  @RefSecNum{Type Conversions}), although it happens implicitly. Therefore,
  the freezing rules are not invoked on the type (which is important so that
  representation items can be given for the type).
  @PDefn2{Term=[subtype conversion],Sec=(bounds of a floating point type)}]}
@end{Honest}

@Defn{Float}
There is a predefined, unconstrained, floating point subtype
named Float@Redundant[, declared in the visible part of
package Standard].
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(floating_point_definition)}
@Redundant[The elaboration of a @nt<floating_point_definition> creates
the floating point type and its first subtype.]
@end{RunTime}

@begin{ImplReq}
@Defn{Float}
In an implementation that supports floating point types with
6 or more digits of precision, the requested
decimal precision for Float shall be at least 6.

@Defn{Long_Float}
If Long_Float is predefined for an implementation, then its
requested decimal precision shall be at least 11.
@end{ImplReq}

@begin{ImplPerm}
@Defn{Short_Float}
@Defn{Long_Float}
An implementation is
allowed to provide additional predefined floating point
types@Redundant[, declared in the visible part of Standard], whose
(unconstrained) first subtypes have names of the form Short_Float,
Long_Float, Short_Short_Float, Long_Long_Float, etc.
Different predefined floating point types are allowed to
have the same base decimal precision.
However, the precision of Float should be no greater than that of Long_Float.
Similarly, the precision of Short_Float (if provided) should be no greater
than Float.
Corresponding recommendations apply to any other predefined floating point
types.
There need not be a named floating point type corresponding to each
distinct base decimal precision supported by an implementation.
@ImplDef{The predefined floating point types declared in Standard.}
@end{ImplPerm}

@begin{ImplAdvice}
@Defn{Long_Float}
An implementation should support Long_Float in addition to
Float if the target machine supports 11 or more digits of precision.
No other named floating point subtypes are recommended for package Standard.
Instead, appropriate named floating point subtypes should be provided in
the library package Interfaces
(see @RefSecNum(The Package Interfaces)).
@ChgImplAdvice{Version=[2],Kind=[Added],Text=[@ChgAdded{Version=[2],
Text=[Long_Float should be declared in Standard if the target supports
11 or more digits of precision. No other named float subtypes should be
declared in Standard.]}]}
@begin{ImplNote}
To promote portability, implementations should explicitly declare the floating
point (sub)types Float and Long_Float in Standard, and leave other
predefined float types anonymous.
For implementations
that already support Short_Float, etc., upward compatibility
argues for keeping such declarations in Standard during the
transition period, but perhaps generating a warning on use.
A separate package Interfaces in the predefined environment
is available for pre-declaring types such as Float_32, IEEE_Float_64, etc.
See @RefSecNum(The Package Interfaces).
@end{ImplNote}
@end{ImplAdvice}

@begin{Notes}
If a floating point subtype is unconstrained,
then assignments to variables of the subtype involve only
Overflow_Checks, never Range_Checks.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of floating point types and subtypes:)
@begin(Example)
@key(type) Coefficient @key(is) @key(digits) 10 @key(range) -1.0 .. 1.0;

@key(type) Real @key(is) @key(digits) 8;
@key(type) Mass @key(is) @key(digits) 7 @key(range) 0.0 .. 1.0E35;

@key(subtype) Probability @key(is) Real @key(range) 0.0 .. 1.0;   --@RI[   a subtype with a smaller range]
@end(Example)
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
No Range_Checks, only Overflow_Checks, are performed on
variables (or parameters) of an
unconstrained floating point subtype. This is upward
compatible for programs that do not raise Constraint_Error.
For those that do raise Constraint_Error, it is possible
that the exception will be raised at a later point, or
not at all, if extended range floating point registers are used to hold
the value of the variable (or parameter).
@begin(Reason)
  This change was felt to be justified by the possibility
  of improved performance on machines with extended-range
  floating point registers. An implementation need not
  take advantage of this relaxation in the range checking; it
  can hide completely the use of extended range registers if desired,
  presumably at some run-time expense.
@end(Reason)
@end{Inconsistent83}

@begin{DiffWord83}
The syntax rules for @ntf{floating_point_constraint} and
@ntf{floating_accuracy_definition} are removed. The syntax rules for
@nt{floating_point_definition} and
@nt{real_range_specification} are new.

A syntax rule for @nt<digits_constraint> is given in
@RefSec{Fixed Point Types}. In @RefSecNum{Reduced Accuracy Subtypes}
we indicate that a @nt<digits_constraint>
may be applied to a floating point @nt<subtype_mark> as well
(to be compatible with Ada 83's @ntf<floating_point_constraint>).

Discussion of model numbers is postponed to
@RefSecNum{Operations of Floating Point Types} and
@RefSecNum{Numeric Performance Requirements}.
The concept of safe numbers has been replaced by the concept
of the safe range of values. The bounds of the safe range are
given by T'Safe_First .. T'Safe_Last, rather than -T'Safe_Large ..
T'Safe_Large, since on some machines the safe range is not
perfectly symmetric.
The concept of machine numbers is new, and is relevant to
the definition of Succ and Pred for floating point numbers.
@end{DiffWord83}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledSubClause{Operations of Floating Point Types}

@begin{StaticSem}
@Leading@;The following attribute is defined for
@PrefixType{every floating point subtype S}:

@begin(description)
@ChgAttribute{Version=[1], Kind=[Revised], ChginAnnex=[F], Leading=[F],
  Prefix=<S>, AttrName=<Digits>, Ref=[8652/0004], ARef=[AI95-00203-01],
  Text=[S'Digits denotes the requested decimal precision
  for the subtype S. The value of this attribute
  is of the type @i(universal_integer).]}
The requested decimal precision of the base subtype of a floating
point type @i{T} is defined to be the largest value of @i{d} for which
@chg{New=[@*],Old=[]}
ceiling(@i{d} * log(10) / log(T'Machine_Radix)) + @Chg{New=[@i{g}],Old=[1]}
<= T'Model_Mantissa@Chg{New=[@*
where g is 0 if Machine_Radix is a positive power of 10 and 1 otherwise],Old=[]}.
@end(description)
@EndPrefixType{}
@end{StaticSem}

@begin{Notes}
@PDefn2{Term=[predefined operations],Sec=(of a floating point type)}
The predefined operations of a floating point type include the assignment
operation, qualification, the membership tests, and
explicit conversion to and from other numeric types. They also
include the relational operators and the following predefined
arithmetic operators:
the binary and unary adding operators @en@; and +,
certain multiplying
operators, the unary operator @key(abs),
and the exponentiation operator.

As for all types, objects of a floating point type
have Size and Address attributes
(see @RefSecNum(Operational and Representation Attributes)).
Other attributes of floating point types are defined in
@RefSecNum{Attributes of Floating Point Types}.
@end{Notes}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0004],ARef=[AI95-00203-01]}
@ChgAdded{Version=[2],Text=[@b<Corrigendum:> Corrected the formula for Digits when
the Machine_Radix is 10.]}
@end{DiffWord95}


@LabeledSubClause{Fixed Point Types}

@begin{Intro}
@Defn{fixed point type}
@Defn{ordinary fixed point type}
@Defn{decimal fixed point type}
A fixed point type is either an ordinary fixed point type,
or a decimal fixed point type.
@Defn2{Term=[delta], Sec=(of a fixed point type)}
The error bound of a fixed point type is specified as an
absolute value, called the @i(delta) of the fixed point type.
@end{Intro}

@begin{Syntax}
@Syn{lhs=<fixed_point_definition>,rhs="@Syn2{ordinary_fixed_point_definition} | @Syn2{decimal_fixed_point_definition}"}


@Syn{lhs=<ordinary_fixed_point_definition>,rhs="
   @key{delta} @SynI{static_}@Syn2{expression}  @Syn2{real_range_specification}"}

@Syn{lhs=<decimal_fixed_point_definition>,rhs="
   @key{delta} @SynI{static_}@Syn2{expression} @key{digits} @SynI{static_}@Syn2{expression} [@Syn2{real_range_specification}]"}

@Syn{lhs=<digits_constraint>,rhs="
   @key{digits} @SynI{static_}@Syn2{expression} [@Syn2{range_constraint}]"}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type], Sec=(fixed point type delta)}
For a type defined by a @nt<fixed_point_definition>,
the @i(delta) of the type is specified by the value of
the @nt<expression> given after the
reserved word @key(delta); this @nt<expression> is expected
to be of any real type.
@PDefn2{Term=[expected type], Sec=(decimal fixed point type digits)}
@Defn2{Term=[digits], Sec=(of a decimal fixed point subtype)}
@Defn{decimal fixed point type}
For a type defined by a @nt<decimal_fixed_point_definition>
(a @i(decimal) fixed point type),
the number of significant decimal digits
for its first subtype (the @i(digits) of the first subtype)
is specified by the @nt<expression> given
after the reserved word @key(digits); this @nt<expression>
is expected to be of any integer type.
@end{Resolution}

@begin{Legality}
In a @nt<fixed_point_definition> or @nt<digits_constraint>,
the @nt<expression>s given after the
reserved words @key(delta) and @key(digits) shall be static; their
values shall be positive.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00100-01]}
@Defn2{Term=[small], Sec=(of a fixed point type)}
The set of values of a fixed point type comprise the integral multiples
of a number called the @i(small) of the
type.@Chg{Version=[2],New=[@PDefn2{Term=[machine numbers],Sec=[of a fixed point type]}
The @i{machine numbers} of a fixed point type are the values of the type that
can be represented exactly in every unconstrained variable of the type.],Old=[]}
@Defn{ordinary fixed point type}
For a type defined by an @nt<ordinary_fixed_point_definition>
(an @i(ordinary) fixed point type), the @i(small) may be specified
by an @nt<attribute_@!definition_@!clause>
(see @RefSecNum{Operational and Representation Attributes});
if so specified, it shall be no greater than the @i(delta) of the type.
If not specified, the @i(small) of an ordinary fixed
point type is an implementation-defined
power of two less than or equal to the @i(delta).
@ImplDef{The @i(small) of an ordinary fixed point type.}

For a decimal fixed point type,
the @i(small) equals the @i(delta);
the @i(delta)
shall be a power of 10.
If a @nt<real_range_specification> is given,
both bounds of the range shall be in
the range @en@;(10**@i(digits)@en@;1)*@i(delta) .. +(10**@i(digits)@en@;1)*@i(delta).

A @nt<fixed_point_definition> is illegal if the implementation
does not support a fixed point type with the given @i(small) and
specified range or @i(digits).
@ImplDef{What combinations of @i(small), range, and @i(digits)
  are supported for fixed point types.}

For a @nt<subtype_indication> with a @nt<digits_constraint>, the
@nt<subtype_mark> shall denote a decimal fixed point subtype.
@begin(Honest)
  Or, as an obsolescent feature, a floating point subtype is permitted
  @em see @RefSecNum(Reduced Accuracy Subtypes).
@end(Honest)
@end{Legality}

@begin{StaticSem}
@PDefn2{Term=[base range], Sec=(of a fixed point type)}
The base range (see @RefSecNum{Scalar Types}) of a fixed point type
is symmetric around zero, except possibly for an extra negative
value in some implementations.

@Leading@PDefn2{Term=[base range], Sec=(of an ordinary fixed point type)}
An @nt<ordinary_fixed_point_definition> defines an
ordinary fixed point type whose base range
includes at least all multiples of @i(small) that are between the
bounds specified
in the @nt<real_range_specification>. The base range of the
type does not necessarily include the specified bounds themselves.
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
An @nt<ordinary_@!fixed_@!point_@!definition> also defines a constrained first
subtype of the type, with each bound of its range
given by the closer to zero of:
@begin(itemize)
  the value of the conversion to the fixed point type
  of the corresponding @nt<expression> of the
  @nt<real_range_specification>;
  @PDefn2{Term=[implicit subtype conversion],Sec=(bounds of a fixed point type)}

  @begin{Honest}
    @ChgRef{Version=[1],Kind=[Added]}@ChgNote{This is discussed in AC-00002, which we can't reference here}
    @ChgAdded{Version=[1],Text=[The conversion mentioned above is not an @i{implicit subtype
    conversion} (which is something that happens at overload resolution, see
    @RefSecNum{Type Conversions}), although it happens implicitly. Therefore,
    the freezing rules are not invoked on the type (which is important so that
    representation items can be given for the type).
    @PDefn2{Term=[subtype conversion],Sec=(bounds of a fixed point type)}]}
  @end{Honest}

  the corresponding bound of the base range.
@end(itemize)

@PDefn2{Term=[base range], Sec=(of a decimal fixed point type)}
A @nt<decimal_fixed_point_definition> defines a decimal fixed point
type whose base range includes at least
the range @en@;(10**@i(digits)@en@;1)*@i(delta) .. +(10**@i(digits)@en@;1)*@i(delta).
@Defn2{Term=[constrained], Sec=(subtype)}
@Defn2{Term=[unconstrained], Sec=(subtype)}
A @nt<decimal_fixed_point_definition> also defines a constrained first
subtype of the type. If a @nt<real_range_specification> is given,
the bounds of the first subtype are given by a conversion
of the values of the @nt<expression>s of the
@nt<real_range_specification>.
@PDefn2{Term=[implicit subtype conversion],Sec=(bounds of a decimal fixed point type)}
Otherwise, the range of the first subtype is
@en@;(10**@i(digits)@en@;1)*@i(delta) .. +(10**@i(digits)@en@;1)*@i(delta).

@begin{Honest}
  @ChgRef{Version=[1],Kind=[Added]}@ChgNote{This is discussed in AC-00002, which we can't reference here}
  @ChgAdded{Version=[1],Text=[The conversion mentioned above is not an @i{implicit subtype
  conversion} (which is something that happens at overload resolution, see
  @RefSecNum{Type Conversions}), although it happens implicitly. Therefore,
  the freezing rules are not invoked on the type (which is important so that
  representation items can be given for the type).
  @PDefn2{Term=[subtype conversion],Sec=(bounds of a decimal fixed point type)}]}
@end{Honest}

@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(fixed_point_definition)}
The elaboration of a @nt<fixed_point_definition>
creates the fixed point type and its first subtype.

For a @nt<digits_constraint> on a decimal fixed point subtype with
a given @i(delta), if it does not have a @nt<range_constraint>,
then it specifies an implicit range
@en@;(10**@i(D)@en@;1)*@i(delta) .. +(10**@i(D)@en@;1)*@i(delta),
where @i(D) is the value of the @nt<expression>.
@Defn2{Term=[compatibility],
  Sec=(digits_constraint with a decimal fixed point subtype)}
A @nt<digits_constraint> is @i(compatible) with a decimal
fixed point subtype if the value of the @nt<expression>
is no greater than the @i(digits) of the subtype,
and if it specifies (explicitly
or implicitly) a range that is compatible with the subtype.
@begin(Discussion)
  Except for the requirement that the @i(digits) specified be
  no greater than the @i(digits) of the subtype being
  constrained, a @nt<digits_constraint> is essentially
  equivalent to a @nt<range_constraint>.

  @Leading@keepnext@;Consider the following example:
@begin{Example}
@key[type] D @key[is] @key[delta] 0.01 @key[digits] 7 @key[range] -0.00 .. 9999.99;
@end{Example}

  @ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00008}
  The compatibility rule implies that the @nt{digits_constraint}
  "@key[digits] 6" specifies an implicit range of
  "@en@;@Chg{New=[9999.99],Old=[99.9999]} .. @Chg{New=[9999.99],Old=[99.9999]}".
  Thus, "@key[digits] 6" is not compatible with the constraint of D, but
  "@key[digits] 6 range 0.00 .. 9999.99" is compatible.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01]}
  A value of a scalar type
  belongs to a constrained subtype of the type if it belongs to the
  range of the subtype. Attributes like Digits and Delta have no
  @Chg{Version=[2],New=[effect],Old=[affect]} on this fundamental rule.
  So the obsolescent forms of
  @nt<digits_constraint>s and @nt<delta_constraint>s that are
  called @lquotes@;accuracy constraints@rquotes@; in RM83 don't really
  represent constraints on the values of the subtype, but rather primarily
  affect compatibility of the @lquotes@;constraint@rquotes@; with the subtype
  being @lquotes@;constrained.@rquotes@; In this sense, they might better
  be called @lquotes@;subtype assertions@rquotes@; rather than @lquotes@;constraints.@rquotes@;

  Note that the @nt<digits_constraint> on a decimal fixed point subtype
  is a combination of an assertion about the @i(digits) of the
  subtype being further constrained, and a constraint on the range of
  the subtype being defined, either explicit or implicit.
@end(Discussion)

@PDefn2{Term=[elaboration], Sec=(digits_constraint)}
The elaboration of a @nt<digits_constraint> consists of the
elaboration of the @nt<range_constraint>, if any.
@IndexCheck{Range_Check}
If a @nt<range_constraint> is given, a check is made that
the bounds of the range are both in the range
@en@;(10**@i(D)@en@;1)*@i(delta) .. +(10**@i(D)@en@;1)*@i(delta),
where @i(D) is the value of the (static) @nt<expression>
given after the reserved word @key(digits).
@Defn2{Term=(Constraint_Error),Sec=(raised by failure of run-time check)}
If this check fails, Constraint_Error is raised.
@end{RunTime}

@begin{ImplReq}
The implementation shall support at least 24 bits of precision
(including the sign bit) for fixed point types.
@begin{Reason}
This is sufficient to represent Standard.Duration with a @i(small)
no more than 50 milliseconds.
@end{Reason}
@end{ImplReq}

@begin{ImplPerm}
Implementations are permitted to support only
@i(small)s that are a power of two. In particular,
all decimal fixed point type declarations can be disallowed.
Note however that conformance with the Information Systems Annex
requires support for decimal @i(small)s, and decimal fixed point
type declarations with @i(digits) up to at least 18.
@begin{ImplNote}
The accuracy requirements for multiplication, division, and conversion
(see @RefSec{Model of Floating Point Arithmetic})
are such that
support for arbitrary @i(small)s should be practical without undue
implementation effort. Therefore, implementations should support fixed point
types with arbitrary values for @i(small) (within reason).
One reasonable limitation would be to limit support to fixed point types
that can be converted to the most precise floating point type
without loss of precision (so that Fixed_IO is implementable in terms
of Float_IO).
@end{ImplNote}
@end{ImplPerm}

@begin{Notes}
  @Leading@;The base range of
  an ordinary fixed point type need not include the specified bounds
  themselves
  so that the range specification can be given in a natural way, such as:
  @begin(example)
   @b(type) Fraction @b(is delta) 2.0**(-15) @b(range) -1.0 .. 1.0;
  @end(example)

  @NoPrefix@;With 2's complement hardware, such a type could have a
  signed 16-bit representation, using 1 bit for the sign
  and 15 bits for fraction, resulting in a base range of
  @en@;1.0 .. 1.0@en@;2.0**(@en@;15).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of fixed point types and subtypes:)
@begin(Example)
@key(type) Volt @key(is) @key(delta) 0.125 @key(range) 0.0 .. 255.0;

  -- @RI[A pure fraction which requires all the available]
  -- @RI[space in a word can be declared as the type Fraction:]
@key(type) Fraction @key(is) @key(delta) System.Fine_Delta @key(range) -1.0 .. 1.0;
  -- @RI[Fraction'Last = 1.0 @en System.Fine_Delta]

@key(type) Money @key(is) @key(delta) 0.01 @key(digits) 15;  -- @RI[decimal fixed point]
@key(subtype) Salary @key(is) Money @key(digits) 10;
  -- @RI[Money'Last = 10.0**13 @en 0.01, Salary'Last = 10.0**8 @en 0.01]
@end(Example)
@end{Examples}

@begin{Inconsistent83}
@Defn{inconsistencies with Ada 83}
In Ada 95, S'Small always equals S'Base'Small,
so if an implementation chooses a @i(small) for a fixed point type smaller
than required by the @i(delta), the value of S'Small in Ada 95 might not be
the same as it was in Ada 83.
@end{Inconsistent83}

@begin{Extend83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@Defn{extensions to Ada 83}
Decimal fixed point types are new, though their
capabilities are essentially similar to that available
in Ada 83 with a fixed point type whose @i(small) equals its @i(delta)
@Chg{Version=[3],New=[and both are],Old=[equals]}
a power of 10. However, in the Information Systems Annex, additional
requirements are placed on the support of decimal fixed point types
(e.g. a minimum of 18 digits of precision).
@end{Extend83}

@begin{DiffWord83}
The syntax rules for @ntf{fixed_point_constraint} and
@ntf{fixed_accuracy_definition} are removed. The syntax rule for
@nt{fixed_point_definition} is new.
A syntax rule for @nt<delta_constraint> is included in the
Obsolescent features (to be compatible with Ada 83's
@ntf<fixed_point_constraint>).
@end{DiffWord83}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00100-01]}
@ChgAdded{Version=[2],Text=[Added wording to define the
machine numbers of fixed point types; this is needed by the static
evaluation rules.]}
@end{DiffWord95}


@LabeledSubClause{Operations of Fixed Point Types}

@begin{StaticSem}
@Leading@;The following attributes are defined for
@PrefixType{every fixed point subtype S}:
@begin(description)
@ChgAttribute{Version=[1], Kind=[Revised], ChginAnnex=[F], Leading=[F],
  Prefix=<S>, AttrName=<Small>, Ref=[8652/0005], ARef=[AI95-00054-01],
  Text=[S'Small
     denotes the @i(small) of the type of S.
     The value of this attribute is of the type @i(universal_real).]}
     @PDefn2{Term=[specifiable], Sec=(of Small for fixed point types)}
     @Defn{Small clause}
     Small may be specified
     for nonderived@Chg{New=[ ordinary],Old=[]} fixed point types
     via an @nt{attribute_@!definition_@!clause}
     (see @RefSecNum{Operational and Representation Attributes});
     the expression of such a clause shall be
     static.@Chg{Version=[3],New=[@AspectDefn{Small}],Old=[]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Small],
    Text=[@ChgAdded{Version=[3],Text=[Scale factor for a fixed point type.]}]}

@Attribute{Prefix=<S>, AttrName=<Delta>,
  Text=[S'Delta
     denotes the @i(delta) of the fixed point subtype S.
     The value of this attribute is of the type @i(universal_real).]}
     @begin{Reason}

       The @i(delta) is associated with the @i(sub)type as opposed
       to the type,
       because of the possibility of an (obsolescent) @nt<delta_constraint>.@end{reason}

@Attribute{Prefix=<S>, AttrName=<Fore>,
  Text=[S'Fore yields the minimum number of characters needed
     before the decimal point
     for the decimal representation of any value of the subtype S, assuming
     that the representation does not include an exponent, but includes
     a one-character prefix that is either a minus sign or a space.
     (This minimum number does not include superfluous zeros or
     underlines, and is at least 2.) The value of this attribute
     is of the type @i(universal_integer).]}

@Attribute{Prefix=<S>, AttrName=<Aft>,
  Text=<S'Aft yields the number of decimal digits needed after
     the decimal point to accommodate the @i(delta) of the subtype
     S, unless the @i(delta) of the subtype S is greater than 0.1,
     in which case the attribute yields the value one. @Redundant[(S'Aft
     is the smallest positive integer N for which (10**N)*S'Delta is
     greater than or equal to one.)] The value of this attribute is of
     the type @i(universal_integer).>}
@end(description)
@EndPrefixType{}

@begin{WideAbove}
@Leading@;The following additional attributes are defined for
@PrefixType{every decimal fixed point subtype S}:
@end{WideAbove}
@begin(description)
@Attribute{Prefix=<S>, AttrName=<Digits>,
  Text=[S'Digits denotes the @i(digits) of the decimal
     fixed point subtype S, which corresponds to the number
     of decimal digits that are representable in objects of the
     subtype.
     The value of this attribute is of the type @i(universal_integer).]}
     Its value is determined as follows:
@Defn2{Term=[digits], Sec=(of a decimal fixed point subtype)}
@begin(itemize)
  For a first subtype or a subtype
  defined by a @nt<subtype_indication> with
  a @nt<digits_constraint>, the digits is the value
  of the expression given after the reserved word @key(digits);

  For a subtype defined by a @nt<subtype_indication> without
  a @nt<digits_constraint>, the digits of the subtype
  is the same as that of the subtype denoted
  by the @nt<subtype_mark> in the @nt<subtype_indication>.
@begin(ImplNote)
  Although a decimal subtype can be both range-constrained
  and digits-constrained, the digits constraint is intended
  to control the Size attribute of the subtype. For decimal
  types, Size can be important because input/output of decimal types
  is so common.
@end(ImplNote)

  The digits of a base subtype is the largest integer
  @i(D) such that the range
  @en@;(10**@i(D)@en@;1)*@i(delta) .. +(10**@i(D)@en@;1)*@i(delta)
  is included in the base range of the type.

@end(itemize)

@Attribute{Prefix=<S>, AttrName=<Scale>,
  Text=[S'Scale denotes the @i(scale) of the subtype S,
 defined as the value N such that S'Delta = 10.0**(@en@;N).
 @Defn2{Term=[scale], Sec=(of a decimal fixed point subtype)}
 @Redundant{The scale indicates the position of the point relative
 to the rightmost significant digits of values of subtype S.}
 The value of this attribute is of the type @i{universal_integer}.]}
 @begin{Ramification}
   S'Scale is negative if S'Delta is greater than one.
   By contrast, S'Aft is always positive.
 @end{Ramification}

@AttributeLeading{Prefix=<S>, AttrName=<Round>,
  Text=[S'Round denotes a function with
     the following specification:
@begin(Descexample)
@b(function) S'Round(@RI(X) : @RI(universal_real))
  @b(return) S'Base
@end(Descexample)

     @NoPrefix@;The function returns the value obtained by rounding X (away
     from 0, if X is midway between two values of the type of S).]}
@end(description)
@EndPrefixType{}
@end{StaticSem}

@begin{Notes}
All subtypes of a fixed point type will have the same value
for the Delta attribute, in the absence of @nt<delta_constraint>s
(see @RefSecNum(Reduced Accuracy Subtypes)).

S'Scale is not always the same as S'Aft for a decimal subtype;
for example, if S'Delta = 1.0 then
S'Aft is 1 while S'Scale is 0.

@PDefn2{Term=[predefined operations],Sec=(of a fixed point type)}
The predefined operations of a fixed point type include the assignment
operation, qualification, the membership tests, and
explicit conversion to and from other numeric types. They also
include the relational operators and the following predefined
arithmetic operators:
the binary and unary adding operators @en@; and +, multiplying
operators, and the unary operator @key(abs).

As for all types, objects of a fixed point type
have Size and Address attributes
(see @RefSecNum(Operational and Representation Attributes)).
Other attributes of fixed point types are defined in
@RefSecNum{Attributes of Fixed Point Types}.
@end{Notes}

@begin{DiffWord95}
@ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0005],ARef=[AI95-00054-01]}
@ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that @i<small> may be
specified only for ordinary fixed point types.]}
@end{DiffWord95}
