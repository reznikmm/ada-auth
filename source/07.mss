@Part(07, Root="ada.mss")

@Comment{$Date: 2012/11/28 23:53:03 $}
@LabeledSection{Packages}

@Comment{$Source: e:\\cvsroot/ARM/Source/07.mss,v $}
@Comment{$Revision: 1.130 $}

@begin{Intro}
@redundant[@ToGlossaryAlso{Term=<Package>,
  Text=<Packages are program units that allow the specification of groups of
  logically related entities.
  Typically, a package contains the declaration of a type
  (often a private type or private extension) along with
  the declarations of primitive subprograms of the type,
  which can be called from outside the package,
  while their inner workings remain hidden from outside users.>}
@IndexSee{Term=[information hiding],See=(package)}
@IndexSee{Term=[encapsulation],See=(package)}
@IndexSee{Term=[module],See=(package)}
@IndexSeeAlso{Term=[class],See=(package)}]
@end{Intro}

@LabeledClause{Package Specifications and Declarations}

@begin{Intro}
@redundant[A package is generally provided in two parts: a
@nt{package_specification} and a @nt{package_body}.
Every package has a @nt{package_specification}, but not all packages
have a @nt{package_body}.]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<package_declaration>,rhs="@Syn2{package_specification};"}


@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<package_specification>,rhs="
    @key{package} @Syn2{defining_program_unit_name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]} @key{is}
      {@Syn2{basic_declarative_item}}
   [@key{private}
      {@Syn2{basic_declarative_item}}]
    @key{end} [[@Syn2{parent_unit_name}.]@Syn2{identifier}]"}

@begin{SyntaxText}
If an @nt{identifier} or @nt{parent_unit_name}.@nt{identifier}
appears at the end of a @nt{package_specification},
then this sequence of lexical elements shall repeat the
@nt{defining_program_unit_name}.
@end{SyntaxText}
@end{Syntax}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00434-01]}
@PDefn2{Term=[requires a completion], Sec=(@nt{package_declaration})}
@PDefn2{Term=[requires a completion], Sec=(@nt{generic_package_declaration})}
A @nt{package_declaration} or @nt{generic_package_declaration}
requires a completion @Redundant[(a body)]
if it contains any @Chg{Version=[2],New=[@nt{basic_declarative_item}],
Old=[@nt<declarative_item>]} that requires a completion,
but whose completion is not in its @nt{package_specification}.
@begin(Honest)
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
  If an implementation supports it,@Chg{Version=[3],New=[],Old=[ a
  @nt{pragma} Import may substitute
  for]} the body of a package or generic package@Chg{Version=[3],New=[
  may be imported (using aspect Import, see @RefSecNum{Interfacing Aspects}),
  in which case no explicit body is allowed],Old=[]}.
@end(Honest)
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00420-01],ARef=[AI95-00434-01]}
@PDefn2{Term=[visible part], Sec=<of a package
(other than a generic formal package)>}
The first list of @Chg{Version=[2],New=[@nt{basic_declarative_item}s],
Old=[@nt{declarative_item}s]} of a
@nt{package_specification} of a package other than a generic formal package
is called the @i{visible part} of the package.
@Redundant[@PDefn2{Term=[private part], Sec=(of a package)}
The optional list of @Chg{Version=[2],New=[@nt{basic_declarative_item}s],
Old=[@nt{declarative_item}s]} after the reserved word
@key{private} (of any @nt{package_specification}) is called the
@i{private part} of the package.
If the reserved
word @key{private} does not appear, the package has an implicit empty
private part.]@Chg{Version=[2],New=[ Each list of @nt{basic_declarative_item}s
of a @nt{package_specification} forms a @i{declaration list} of the
package.@PDefn2{Term=[declaration list],Sec=(package_specification)}],Old=[]}

@begin{Ramification}
This definition of visible part does not apply to generic formal
packages @em @RefSecNum{Formal Packages} defines
the visible part of a generic formal package.

The implicit empty private part is important because certain
implicit declarations occur there if the package is a child package,
and it defines types in its visible part that are derived from,
or contain as components, private types declared within the
parent package. These implicit declarations are visible
in children of the child package.
See @RefSecNum(Compilation Units - Library Units).
@end{Ramification}

@redundant[An entity declared in the private part of a package is visible
only within the declarative region of the package itself
(including any child units @em
see @RefSecNum{Compilation Units - Library Units}).
In contrast, expanded names denoting
entities declared in the visible part can be used even outside the
package; furthermore, direct visibility of such entities can be
achieved by means of @nt{use_clause}s
(see @RefSecNum{Selected Components} and @RefSecNum{Use Clauses}).]
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(package_declaration)}
The elaboration of a @nt{package_declaration} consists of the elaboration of
its @nt{basic_declarative_item}s in the given order.
@end{RunTime}

@begin{Notes}
The visible part of a package contains all the information that
another program unit is able to know about the package.

If a declaration occurs immediately within the specification
of a package, and the declaration has a corresponding completion that
is a body,
then that body has to occur immediately within the body of
the package.
@begin{TheProof}
This follows from the fact that the declaration and completion are
required to occur
immediately within the same declarative region,
and the fact that @ntf{bodies} are disallowed (by the @SyntaxName@;s)
in @nt{package_specification}s.
This does not apply to instances of generic units,
whose bodies can occur in @nt{package_specification}s.
@end{TheProof}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a package declaration:}
@begin{Example}
@key[package] Rational_Numbers @key[is]

   @key[type] Rational @key[is]
      @key[record]
         Numerator   : Integer;
         Denominator : Positive;
      @key[end] @key[record];

   @key[function] "="(X,Y : Rational) @key[return] Boolean;

   @key[function] "/"  (X,Y : Integer)  @key[return] Rational;  --@RI{  to construct a rational number}

   @key[function] "+"  (X,Y : Rational) @key[return] Rational;
   @key[function] "-"  (X,Y : Rational) @key[return] Rational;
   @key[function] "*"  (X,Y : Rational) @key[return] Rational;
   @key[function] "/"  (X,Y : Rational) @key[return] Rational;
@key[end] Rational_Numbers;
@end{Example}

There are also many examples of package declarations in the predefined
language environment
(see @RefSecNum{Predefined Language Environment}).
@end{Examples}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
In Ada 83, a library package is allowed to have a body even if
it doesn't need one.
In Ada 95, a library package body is either
required or forbidden @em never optional.
The workaround is to add @key[pragma] Elaborate_Body,
or something else requiring a body,
to each library package that has a body that isn't otherwise
required.
@end{Incompatible83}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
We have moved the syntax into this @Chg{Version=[3],New=[subclause],Old=[clause]}
and the next @Chg{Version=[3],New=[subclause],Old=[clause]} from
RM83-7.1, @lquotes@;Package Structure@rquotes@;, which we have removed.

RM83 was unclear on the rules about when a package requires a body.
For example, RM83-7.1(4) and RM83-7.1(8) clearly forgot about the
case of an incomplete type declared in a @nt{package_declaration} but
completed in the body.
In addition,
RM83 forgot to make this rule apply to a generic package.
We have corrected these rules.
Finally, since we now allow a @nt{pragma} Import for any explicit
declaration, the completion rules need to take this into account as
well.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00420-01]}
  @ChgAdded{Version=[2],Text=[Defined @lquotes@;declaration list@rquotes
  to avoid ambiguity in other rules as to whether packages are included.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a @nt{package_specification}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}


@LabeledClause{Package Bodies}

@begin{Intro}
@redundant[In contrast to the entities declared in the visible part of a
package, the entities declared in the @nt{package_body} are
visible only
within the @nt{package_body} itself.
As a consequence, a
package with a @nt{package_body} can be used for the construction of
a group of related subprograms in
which the logical operations available to clients are clearly
isolated from the internal entities.]
@end{Intro}

@begin{Syntax}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0267-1]}
@Syn{lhs=<package_body>,rhs="
    @key{package} @key{body} @Syn2{defining_program_unit_name}@Chg{Version=[3],New=<
        [@Syn2{aspect_specification}]>,Old=[]} @key{is}
       @Syn2{declarative_part}
   [@key{begin}
        @Syn2{handled_sequence_of_statements}]
    @key{end} [[@Syn2{parent_unit_name}.]@Syn2{identifier}];"}

@begin{SyntaxText}
If an @nt{identifier} or @nt{parent_unit_name}.@nt{identifier}
appears at the end of a @nt{package_body},
then this sequence of lexical elements shall repeat the
@nt{defining_program_unit_name}.
@end{SyntaxText}
@end{Syntax}

@begin{Legality}
A @nt{package_body} shall be the completion of a previous
@nt{package_@!declaration} or @nt{generic_@!package_@!declaration}.
A library @nt{package_@!declaration} or
library @nt{generic_@!package_@!declaration}
shall not have a body
unless it requires a body@Redundant[;
@key<pragma> Elaborate_Body can be used to require
a @nt<library_@!unit_@!declaration> to have a body
(see @RefSecNum{Elaboration Control})
if it would not otherwise require one].
@begin{Ramification}
The first part of the rule forbids a @nt{package_body} from
standing alone @em it has to belong to some previous
@nt{package_declaration} or @nt{generic_package_declaration}.

A nonlibrary @nt{package_declaration} or
nonlibrary @nt<generic_package_declaration>
that does not require a completion may
have a corresponding body anyway.
@end{Ramification}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
In any @nt{package_body} without @nt{statement}s
there is an implicit @nt{null_@!statement}.
For any @nt{package_@!declaration} without an explicit completion,
there is an implicit @nt{package_@!body} containing a single
@nt{null_statement}.
For a noninstance, nonlibrary package,
this body occurs at the end of the
@nt{declarative_@!part} of the innermost enclosing program unit or
@nt{block_@!statement};
if there are several such packages,
the order of the implicit @ntf{package_@!bodies} is unspecified.
@PDefn{unspecified}
@Redundant[(For an instance, the implicit @nt{package_@!body}
occurs at the place of the instantiation
(see @RefSecNum{Generic Instantiation}).
For a library package, the place is partially determined by the
elaboration dependences (see @Chg{Version=[3],New=[Clause],Old=[Section]}
@RefSecNum{Program Structure and Compilation Issues}).)]
@begin{Discussion}
Thus, for example, we can refer to something happening just
after the @key{begin} of a @nt{package_body},
and we can refer to the @nt{handled_sequence_of_statements}
of a @nt{package_body},
without worrying about all the optional pieces.
The place of the implicit body makes a difference for tasks
activated by the package.
See also RM83-9.3(5).

The implicit body would be illegal if explicit in the case of a library
package that does not require (and therefore does not allow)
a body.
This is a bit strange, but not harmful.
@end{Discussion}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(nongeneric package_body)}
For the elaboration of a nongeneric @nt{package_body},
its @nt{declarative_@!part} is first
elaborated, and its @nt{handled_@!sequence_of_@!statements} is then executed.
@end{RunTime}

@begin{Notes}
A variable declared in the body of a package is only visible
within this body and, consequently, its value can only be
changed within the @nt{package_body}. In the absence of local tasks,
the value of such a variable remains unchanged between calls issued
from outside the package to subprograms declared in the visible part.
The properties of such a variable are similar to those of a @lquotes@;static@rquotes@;
variable of C.

The elaboration of the body of a subprogram explicitly declared
in the visible part of a package is caused by the elaboration of the
body of the package. Hence a call of such a subprogram by an
outside program unit raises the exception Program_Error if the call
takes place before the elaboration of the
@nt{package_body} (see @RefSecNum{Declarative Parts}).
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a package body
(see @RefSecNum{Package Specifications and Declarations}):}
@begin{Example}
@key[package] @key[body] Rational_Numbers @key[is]

   @key[procedure] Same_Denominator (X,Y : @key[in] @key[out] Rational) @key[is]
   @key[begin]
      --@RI{  reduces X and Y to the same denominator:}
      ...
   @key[end] Same_Denominator;

   @key[function] "="(X,Y : Rational) @key[return] Boolean @key[is]
      U : Rational := X;
      V : Rational := Y;
   @key[begin]
      Same_Denominator (U,V);
      @key[return] U.Numerator = V.Numerator;
   @key[end] "=";

   @key[function] "/" (X,Y : Integer) @key[return] Rational @key[is]
   @key[begin]
      @key[if] Y > 0 @key[then]
         @key[return] (Numerator => X,  Denominator => Y);
      @key[else]
         @key[return] (Numerator => -X, Denominator => -Y);
      @key[end] @key[if];
   @key[end] "/";

   @key[function] "+" (X,Y : Rational) @key[return] Rational @key[is] ... @key[end] "+";
   @key[function] "-" (X,Y : Rational) @key[return] Rational @key[is] ... @key[end] "-";
   @key[function] "*" (X,Y : Rational) @key[return] Rational @key[is] ... @key[end] "*";
   @key[function] "/" (X,Y : Rational) @key[return] Rational @key[is] ... @key[end] "/";

@key[end] Rational_Numbers;
@end{Example}
@end{Examples}

@begin{DiffWord83}
The syntax rule for @nt{package_body} now uses the syntactic category
@nt{handled_sequence_of_statements}.

The @nt{declarative_part} of a @nt{package_body} is now required;
that doesn't make any real difference,
since a @nt{declarative_part} can be empty.

RM83 seems to have forgotten to say that a @nt{package_body} can't
stand alone, without a previous declaration.
We state that rule here.

RM83 forgot to restrict the definition of elaboration of
@ntf{package_bodies} to nongeneric ones.
We have corrected that omission.

The rule about implicit bodies (from RM83-9.3(5))
is moved here, since it is more generally applicable.
@end{DiffWord83}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0267-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a @nt{package_body}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}

@LabeledClause{Private Types and Private Extensions}

@begin{Intro}
@redundant[The declaration (in the visible part of a
package) of a type as a private type or private extension
serves to separate the characteristics that can be used
directly by outside program units (that is, the logical properties)
from other characteristics whose direct use is confined to the
package (the details of the definition of the type itself).
See @RefSecNum(Type Extensions) for an overview of type extensions.
@Defn{private types and private extensions}
@IndexSee{Term=[information hiding],See=(private types and private extensions)}
@IndexSee{Term=[opaque type],See=(private types and private extensions)}
@IndexSee{Term=[abstract data type (ADT)],See=(private types and private extensions)}
@IndexSee{Term=[ADT (abstract data type)],See=(private types and private extensions)}]
@end{Intro}

@begin{MetaRules}
A private (untagged) type can be thought of as a record type
with the type of its single (hidden) component being the full view.

A private tagged type can be thought of as a private extension
of an anonymous parent with no components. The only
dispatching operation of the parent is equality
(although the Size attribute, and, if nonlimited, assignment are allowed,
and those will presumably be implemented in terms of dispatching).
@end{MetaRules}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<private_type_declaration>,rhs="
   @key{type} @Syn2{defining_identifier} [@Syn2{discriminant_part}] @key{is} [[@key{abstract}] @key{tagged}] [@key{limited}] @key{private}@Chg{Version=[3],New=<
      [@Syn2{aspect_specification}]>,Old=[]};"}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01],ARef=[AI95-00419-01],ARef=[AI95-00443-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0183-1]}
@Syn{lhs=<private_extension_declaration>,rhs="
   @key{type} @Syn2{defining_identifier} [@Syn2{discriminant_part}] @key{is}
     [@key{abstract}] @Chg{Version=[2],New=<[@key{limited} | @key{synchronized}]>,Old=[]} @key{new} @SynI(ancestor_)@Syn2{subtype_indication}@Chg{Version=[2],New=<
     [@key{and} @Syn2[interface_list]]>,Old=<>} @key{with private}@Chg{Version=[3],New=<
       [@Syn2{aspect_specification}]>,Old=[]};"}
@end{Syntax}

@begin{Legality}
@ChgNote{We don't mark this as a change, since it only involves the AARM}
@Defn2{Term=[partial view], Sec=(of a type)}
@PDefn2{Term=[requires a completion], Sec=(declaration of a partial view)}
A @nt<private_type_declaration>
or @nt<private_extension_declaration>
declares a @i{partial view} of the type;
such a declaration is allowed only as a
@nt{declarative_item} of the visible part of a package,
and it requires a completion,
which shall be a @nt{full_type_declaration} that occurs as
a @nt{declarative_item} of the private part of the package.
@Chg{Version=[2],New=[],Old=[@Defn2{Term=[full view], Sec=(of a type)}]}
@Redundant[@ChgNote{This really should be a change, but that's too hard.}
The view of the type declared by the @nt<full_type_declaration>
is called the @i(full view).]
A generic formal private type or a
generic formal private extension is also a partial view.
@begin(Honest)
  A private type can also be@Chg{Version=[3],New=[
  imported (using aspect Import, see @RefSecNum{Interfacing Aspects}),
  in which case no completion is allowed],Old=[completed by a @nt{pragma}
  Import]}, if supported by an implementation.
@end(Honest)
@begin{Reason}
  We originally used the term @lquotes@;private view,@rquotes@; but this was
  easily confused with the view provided @i(from) the private part, namely the
  full view.
@end{Reason}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00326-01]}
  @ChgAdded{Version=[2],Text=[Full view is now defined in
  @RefSec{Type Declarations}, as all types now have them.]}
@end{TheProof}

@Redundant[A type shall be completely defined before it is frozen
(see @RefSecNum{Completions of Declarations} and
@RefSecNum{Freezing Rules}).
Thus, neither the declaration
of a variable of a partial view of a type, nor the creation by an
@nt{allocator} of an object of the partial view are allowed before
the full declaration of the type.
Similarly, before the full declaration, the name of the partial view
cannot be used in a @nt{generic_instantiation} or in a
representation item.]
@begin{TheProof}
  This rule is stated officially in
  @RefSec{Completions of Declarations}.
@end{TheProof}



@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00419-01],ARef=[AI95-00443-01]}
@Redundant[A private type is limited if its declaration includes
the reserved word @key[limited];
a private extension is limited if its ancestor type is @Chg{Version=[2],
New=[a limited type that is not an interface type, or if the reserved word
@key{limited} or @key{synchronized} appears in its
definition],Old=[limited]}.]
If the partial view is nonlimited, then
the full view shall be nonlimited.
If a tagged partial view is limited,
then the full view shall be limited.
@Redundant[On the other hand,
if an untagged partial view is limited,
the full view may be limited or nonlimited.]

If the partial view is tagged,
then the full view shall be tagged.
@Redundant[On the other hand, if the partial view is untagged,
then the full view may be tagged or untagged.]
In the case where the partial view is untagged and the full view is
tagged,
no derivatives of the partial view are allowed within the immediate
scope of the partial view;
@Redundant[derivatives of the full view are allowed.]
@begin{Ramification}
Note that deriving from a partial view within its immediate scope
can only occur in a package that is a child of the one where the partial
view is declared.
The rule implies that in the visible part of a public child package,
it is impossible to derive from an untagged private type declared in the
visible part of the parent package in the case where the full
view of the parent type turns out to be tagged.
We considered a model in which the derived type was implicitly
redeclared at the earliest place within its immediate scope where
characteristics needed to be added.
However, we rejected that model, because (1) it would imply that (for an
untagged type) subprograms explicitly declared after the derived type
could be inherited, and (2) to make this model work for composite types
as well, several implicit redeclarations would be
needed, since new characteristics can become visible one by one;
that seemed like too much mechanism.
@end{Ramification}
@begin{Discussion}
  The rule for tagged partial views
  is redundant for partial views that are private extensions,
  since all extensions of a given ancestor tagged type are tagged,
  and limited if the ancestor is limited.
  We phrase this rule partially redundantly to keep its structure parallel
  with the other rules.
@end{Discussion}
@begin{Honest}
  This rule is checked in a generic unit,
  rather than using the @lquotes@;assume the best@rquotes@; or @lquotes@;assume the worst@rquotes@;
  method.
@end{Honest}
@begin{Reason}
  @leading@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}Tagged
  limited private types have certain capabilities that are
  incompatible with having assignment for the full view of the type.
  In particular, tagged limited private types can be extended
  with @Chg{Version=[2],New=[],Old=[access discriminants and ]}components
  of a limited type, which works only because assignment is not allowed.
  Consider the following example:
  @begin{Example}
@key[package] P1 @key[is]
    @key[type] T1 @key[is] @key[tagged] @key[limited] @key[private];
    @key[procedure] Foo(X : @key[in] T1'Class);
@key[private]
    @key[type] T1 @key[is] @key[tagged] @key[null] @key[record]; --@RI{ Illegal!}
        --@RI{ This should say @lquotes@;@key[tagged limited null record]@rquotes@;.}
@key[end] P1;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00114}
@key[package] @key[body] P1 @key[is]
    @key[type] A @key[is] @key[access] T1'Class;
    Global : A;
    @key[procedure] Foo(X : @key[in] T1'Class) @key[is]
    @key[begin]
        Global := @key[new] T1'Class'(X);
            --@RI{ This would be illegal if the full view of}
            --@RI{ T1 were limited, like it's supposed to be.}
    @key[end] @Chg{New=[Foo],Old=[A]};
@key[end] P1;

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}@key[with] P1;
@key[package] P2 @key[is]
    @key[type] T2(D : @key[access] Integer)@Chg{Version=[2],New=[],Old=[ --@RI{ Trouble!}]}
            @key[is] @key[new] P1.T1 @key[with]
        @key[record]
            My_Task : Some_Task_Type; --@RI{ @Chg{Version=[2],New=[Trouble],Old=[More trouble]}!}
        @key[end] @key[record];
@key[end] P2;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00114}
@key[with] P1;
@key[with] P2;
@key[procedure] Main @key[is]
    Local : @key[aliased] Integer;
    Y : P2.T2(@Chg{New=[D],Old=[A]} => Local'Access);
@key[begin]
    P1.Foo(Y);
@key[end] Main;
  @end{Example}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
  If the above example were legal,
  we would have succeeded in @Chg{Version=[2],New=[],Old=[making an access
  value that points to Main.Local after Main has been left,
  and we would also have succeeded in ]}doing an assignment of a task
  object, @Chg{Version=[2],New=[],Old=[both of ]}which @Chg{Version=[2],New=[is],
  Old=[are]} supposed to be @Chg{Version=[2],New=[a no-no],Old=[no-no's]}.
  @ChgNote{A runtime check prevents the first from being a problem in Ada 2005.}

  This rule is not needed for private extensions,
  because they inherit their limitedness from their ancestor,
  and there is a separate rule forbidding limited components of the
  corresponding record extension if the parent is nonlimited.
@end{Reason}
@begin{Ramification}
@leading@;A type derived from an untagged private type is untagged,
even if the full view of the parent is tagged,
and even at places that can see the parent:
@begin{Example}
@key[package] P @key[is]
    @key[type] Parent @key[is] @key[private];
@key[private]
    @key[type] Parent @key[is] @key[tagged]
        @key[record]
            X: Integer;
        @key[end] @key[record];
@key[end] P;

@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00114}
@Chg{New=[@key[with] P;
],Old=[]}@key[package] Q @key[is]
    @key[type] T @key[is] @key[new] @Chg{New=[P.],Old=[]}Parent;
@key[end] Q;

@key[with] Q; @key[use] Q;
@key[package] @key[body] P @key[is]
    ... T'Class ... --@RI{ Illegal!}
    Object: T;
    ... Object.X ... --@RI{ Illegal!}
    ... Parent(Object).X ... --@RI{ OK.}
@key[end] P;
@end{Example}

The declaration of T declares an untagged view.
This view is always untagged, so T'Class is illegal,
it would be illegal to extend T, and so forth.
The component name X is never visible for this view,
although the component is still there @em one
can get one's hands on it via a @nt{type_conversion}.

@end{Ramification}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00396-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[If a full type has a partial view
that is tagged, then:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[the partial view shall be a synchronized tagged
type (see @RefSecNum{Interface Types}) if and only if the full type is a
synchronized tagged type;]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Added]}
  @ChgAdded{Version=[2],Text=[ Since we do not allow record extensions of
  synchronized tagged types, this property has to be visible in the partial
  view to avoid privacy breaking. Generic formals do not need a similar rule as
  any extensions are rechecked for legality in the specification, and
  extensions of tagged formals are always illegal in a generic body.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[the partial view shall be a descendant of an
interface type (see 3.9.4) if and only if the full type is a descendant of the
interface type.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],KeepNext=[T],Type=[Leading],Text=[Consider the following example:]}
@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{package} P @key{is}
   @key{package} Pkg @key{is}
      @key{type} Ifc @key{is interface};
      @key{procedure} Foo (X : Ifc) @key{is abstract};
   @key{end} Pkg;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} Parent_1 @key{is tagged null record};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} T1 @key{is new} Parent_1 @key{with private};
@key{private}
   @key{type} Parent_2 @key{is new} Parent_1 @key{and} Pkg.Ifc @key{with null record};
   @key{procedure} Foo (X : Parent_2); -- @RI[Foo #1]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{type} T1 @key{is new} Parent_2 @key{with null record}; -- @RI[Illegal.]
@key{end} P;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} P;
@key{package} P_Client @key{is}
   @key{type} T2 @key{is new} P.T1 @key{and} P.Pkg.Ifc @key{with null record};
   @key{procedure} Foo (X : T2); -- @RI[Foo #2]
   X : T2;
@key{end} P_Client;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{with} P_Client;
@key{package body} P @key{is}
   ...]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key{procedure} Bar (X : T1'Class) @key{is}
   @key{begin}
      Pkg.Foo (X); -- @RI[should call Foo #1 or an override thereof]
   @key{end};]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key{begin}
   Pkg.Foo (Pkg.Ifc'Class (P_Client.X));      -- @RI[should call Foo #2]
   Bar (T1'Class (P_Client.X));
@key{end} P;]}
@end{Example}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This example is illegal because the completion
    of T1 is descended from an interface that the partial view is not descended
    from. If it were legal, T2 would implement Ifc twice, once in the visible
    part of P, and once in the visible part of P_Client. We would need to
    decide how Foo #1 and Foo #2 relate to each other. There are two options:
    either Foo #2 overrides Foo #1, or it doesn't.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If Foo #2 overrides Foo #1,
    we have a problem because the client redefines a behavior that it doesn't
    know about, and we try to avoid this at all costs, as it would lead to a
    breakdown of whatever abstraction was implemented. If the abstraction
    didn't expose that it implements Ifc, there must be a reason, and it should
    be able to depend on the fact that no overriding takes place in clients.
    Also, during maintenance, things may change and the full view might
    implement a different set of interfaces. Furthermore, the situation is even
    worse if the full type implements another interface Ifc2 that happens to
    have a conforming Foo (otherwise unrelated, except for its name and
    profile).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If Foo #2 doesn't override Foo #1,
    there is some similarity with the case of normal tagged private types,
    where a client can declare an operation that happens to conform to some
    private operation, and that's OK, it gets a different slot in the type
    descriptor. The problem here is that T2 would implement Ifc in two
    different ways, and through conversions to Ifc'Class we could end up with
    visibility on both of these two different implementations. This is the
    @lquotes@;diamond inheritance@rquotes problem of C++ all over again, and we
    would need some kind of a preference rule to pick one implementation. We
    don't want to go there (if we did, we might as well provide full-fledged
    multiple inheritance).]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Note that there wouldn't be any difficulty to
    implement the first option, so the restriction is essentially
    methodological. The second option might be harder to implement, depending
    on the language rules that we would choose.]}
@end{Reason}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[This rule also prevents completing a private type
  with an interface. @Chg{Version=[3],New=[An],Old=[A]} interface, like all
  types, is a descendant of itself,
  and thus this rule is triggered. One reason this is necessary is that
  a client of a private extension should be able to inherit limitedness
  without having to look in the private part to see if the type is an
  interface (remember that limitedness of interfaces is never inherited, while
  it is inherited from other types).]}
@end{Ramification}

@end{Itemize}

@Defn2{Term=[ancestor subtype], Sec=(of a @nt<private_extension_declaration>)}
The @i(ancestor subtype) of a @nt<private_extension_declaration>
is the subtype defined by the @i(ancestor_)@!@nt<subtype_@!indication>;
the ancestor type shall be a specific tagged type.
The full view of a private extension shall be derived
(directly or indirectly) from the ancestor type.
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
the requirement that the ancestor be specific applies also in the
private part of an instance of a generic unit.@PDefn{generic contract issue}
@begin{Reason}
  This rule allows the full view to be defined
  through several intermediate derivations,
  possibly from a series of types produced by
  @nt{generic_instantiation}s.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00419-01],ARef=[AI95-00443-01]}
@ChgAdded{Version=[2],Text=[If the reserved word @key{limited} appears in a
@nt{private_extension_declaration}, the ancestor type shall be a limited type.
If the reserved word @key{synchronized} appears in a
@nt{private_extension_declaration}, the ancestor type shall be a limited
interface.]}

If the declaration of a partial view includes
a @nt{known_discriminant_part}, then
the @nt{full_type_declaration} shall have a fully conforming
@Redundant[(explicit)]
@nt{known_discriminant_part}
@Redundant[(see @RefSec(Conformance Rules))].
@Defn2{Term=[full conformance],Sec=(required)}
@Redundant[The ancestor subtype may be unconstrained;
the parent subtype of the full view is required to be constrained
(see @RefSecNum{Discriminants}).]
@begin{Discussion}
  If the ancestor subtype has discriminants,
  then it is usually best to make it unconstrained.
@end{Discussion}

@begin{Ramification}
  If the partial view has a @nt<known_discriminant_part>,
  then the full view has to be a composite, non-array type,
  since only such types may have known discriminants.
  Also, the full view cannot inherit the discriminants in this case;
  the @nt{known_discriminant_part} has to be explicit.

  @leading@keepnext@;That is, the following is illegal:
  @begin{Example}
@key[package] P @key[is]
    @key[type] T(D : Integer) @key[is] @key[private];
@key[private]
    @key[type] T @key[is] @key[new] Some_Other_Type; --@RI{ Illegal!}
@key[end] P;
  @end{Example}
  even if Some_Other_Type has an integer discriminant called D.

  It is a ramification of this and other rules that in order for
  a tagged type to privately inherit unconstrained discriminants,
  the private type declaration has to have an
  @nt{unknown_discriminant_part}.
@end{Ramification}


If a private extension inherits known discriminants from the ancestor subtype,
then the full view shall also inherit its discriminants from the
ancestor subtype,
and the parent subtype of the full view shall be constrained
if and only if the ancestor subtype is constrained.
@begin{Reason}
  The first part ensures that the full view has the same discriminants
  as the partial view.
  The second part ensures that if the partial view is unconstrained,
  then the full view is also unconstrained;
  otherwise, a client might constrain the partial view in a way that
  conflicts with the constraint on the full view.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00419-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0004-1]}
@ChgAdded{Version=[2],Text=[If the @nt{full_type_declaration} for a private
extension @Chg{Version=[3],New=[includes],Old=[is]} a
@Chg{Version=[3],New=[@nt{derived_type_definition}],Old=[@ntf{derived_type_declaration}]},
then the reserved word
@key{limited} shall appear in the @nt{full_type_declaration} if and only if it
also appears in the @nt{private_extension_declaration}.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
  @ChgAdded{Version=[2],Text=[The word @key{limited} is optional (unless the
  ancestor is an interface), but it should be used consistently. Otherwise
  things would be too confusing for the reader. Of course, we only require
  that if the full type @Chg{Version=[3],New=[includes],Old=[is defined by]}
  a @nt{derived_type_definition}, as we
  want to allow task and protected types to complete extensions of synchronized
  interfaces.]}
@end{Reason}

@Redundant[If a partial view has unknown discriminants,
then the @nt{full_type_declaration} may define
a definite or an indefinite subtype, with or without discriminants.]

If a partial view has neither known nor unknown discriminants,
then the @nt{full_type_declaration} shall define a definite subtype.

If the ancestor subtype of a private extension has constrained
discriminants,
then the parent subtype of the full view shall impose a statically
matching constraint on those discriminants.
@PDefn2{Term=[statically matching],Sec=(required)}
@begin{Ramification}
  If the parent type of the full view is not the ancestor type,
  but is rather some descendant thereof, the constraint on
  the discriminants of the parent type might come from
  the declaration of some intermediate type in the derivation
  chain between the ancestor type and the parent type.
@end{Ramification}
@begin{Reason}
@leading@keepnext@;This prevents the following:
@begin{Example}
@key[package] P @key[is]
    @key[type] T2 @key[is] @key[new] T1(Discrim => 3) @key[with] @key[private];
@key[private]
    @key[type] T2 @key[is] @key[new] T1(Discrim => 999) --@RI{ Illegal!}
        @key[with] @key[record] ...;
@key[end] P;
@end{Example}

The constraints in this example do not statically match.

@leading@;If the constraint on the parent subtype of the full view depends on
discriminants of the full view, then the ancestor subtype has to be
unconstrained:
@begin{Example}
@key[type] One_Discrim(A: Integer) @key[is] @key[tagged] ...;
...
@key[package] P @key[is]
    @key[type] Two_Discrims(B: Boolean; C: Integer) @key[is] @key[new] One_Discrim @key[with] @key[private];
@key[private]
    @key[type] Two_Discrims(B: Boolean; C: Integer) @key[is] @key[new] One_Discrim(A => C) @key[with]
        @key[record]
            ...
        @key[end] @key[record];
@key[end] P;
@end{Example}

The above example would be illegal if the private extension said
@lquotes@;is new One_Discrim(A => C);@rquotes@;,
because then the constraints would not statically match.
(Constraints that depend on discriminants are not static.)

@end{Reason}
@end{Legality}

@begin{StaticSem}
@PDefn{private type}
A @nt{private_type_declaration} declares a private type
and its first subtype.
@PDefn{private extension}
Similarly, a @nt{private_@!extension_@!declaration} declares a private
extension and its first subtype.
@begin{Discussion}
@Defn{package-private type}
A @i(package-private type) is one
declared by a @nt<private_type_declaration>;
that is,
a private type other than a generic formal private type.
@Defn{package-private extension}
Similarly, a @i(package-private extension) is one
declared by a @nt<private_extension_declaration>.
These terms are not used in the RM95 version of this document.
@end{Discussion}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0269-1]}
A declaration of a partial view and the corresponding
@nt{full_type_declaration} define two views of a single type.
The declaration of a partial view
together with the visible part define the operations that are
available to outside program units;
the declaration of the full view together with
the private part define other operations whose direct use is
possible only within the declarative region of the package itself.
@Chg{Version=[3],New=[],Old=[@PDefn{characteristics}]}Moreover,
within the scope of the declaration of the full view, the
@Chg{Version=[3],New=[characteristics (see
@RefSecNum{Derived Types and Classes})],Old=[@i{characteristics}]}
of the type are determined by the full view;
in particular, within its scope, the full view determines
the classes that include the type,
which components, entries, and protected subprograms are visible,
what attributes and other predefined operations are allowed,
and whether the first subtype is static.
See @RefSecNum{Private Operations}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0110-1]}
@Chg{Version=[3],New=[For a],Old=[A]}
private extension@Chg{Version=[3],New=[, the characteristics],
Old=[inherits components]} (including
@Chg{Version=[3],New=[components, but excluding ],Old=[]}discriminants
@Chg{Version=[3],New=[if],Old=[unless]} there is a new @nt<discriminant_part>
specified)@Chg{Version=[3],New=[, predefined operators,],Old=[]} and
@Chg{Version=[3],New=[inherited ],Old=[]}user-defined primitive
subprograms @Chg{Version=[3],New=[are determined
by],Old=[from]} its ancestor type@Chg{Version=[2],New=[ and its progenitor
types (if any)],Old=[]}, in the same way that@Chg{Version=[3],New=[ those
of],Old=[]} a record extension @Chg{Version=[3],New=[are determined
by those of],Old=[inherits components and user-defined primitive
subprograms from]} its parent type@Chg{Version=[2],New=[ and its progenitor
types],Old=[]}
(see @RefSecNum{Derived Types and Classes}@Chg{Version=[3],New=[ and
@RefSecNum{Private Operations}],Old=[]}).
@begin{Honest}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0110-1]}
If an operation of the @Chg{Version=[3],New=[ancestor or],Old=[]} parent type
is abstract, then the abstractness of the inherited operation
is different for nonabstract record extensions
than for nonabstract private extensions
(see @RefSecNum{Abstract Types and Subprograms}).
@end{Honest}
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(private_type_declaration)}
The elaboration of a @nt{private_type_declaration} creates a partial
view of a type.
@PDefn2{Term=[elaboration], Sec=(private_extension_declaration)}
The elaboration of a @nt{private_extension_declaration} elaborates
the @i(ancestor_)@nt<subtype_indication>, and creates a
partial view of a type.
@end{RunTime}

@begin{Notes}
The partial view of a type as declared by a @nt<private_type_declaration>
is defined to be a composite view (in @RefSecNum{Types and Subtypes}).
The full view of the type might or might not be composite.
A private extension is also composite,
as is its full view.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
Declaring a private type with an @nt{unknown_discriminant_part} is a
way of preventing clients from creating uninitialized objects of the
type; they are then forced to initialize each object by calling some
operation declared in the visible part of the package.
@Chg{Version=[2],New=[],Old=[If such a type is also limited, then no objects
of the type can be declared outside the scope of the @nt{full_type_declaration},
restricting all object creation to the package defining the type. This
allows complete control over all storage allocation for the type.
Objects of such a type can still be passed as parameters, however.]}
@begin{Discussion}
@Defn{generic contract/private type contract analogy}
Packages with private types are analogous to generic packages with
formal private types,
as follows:
The declaration of a package-private type is like the declaration of
a formal private type.
The visible part of the package is like the generic formal part;
these both specify a contract (that is, a set of operations and other
things available for the private type).
The private part of the package is like an instantiation of the generic;
they both give a @nt{full_type_declaration} that specifies implementation
details of the private type.
The clients of the package are like the body of the generic;
usage of the private type in these places is restricted to the
operations defined by the contract.

In other words, being inside the package is like being outside the
generic, and being outside the package is like being inside the
generic;
a generic is like an @lquotes@;inside-out@rquotes@; package.

This analogy also works for private extensions
in the same inside-out way.

Many of the legality rules are defined with this analogy in mind.
See, for example, the rules relating to operations of [formal]
derived types.

The completion rules for a private
type are intentionally quite similar to the matching rules for a
generic formal private type.

This analogy breaks down in one respect:
a generic actual subtype is a subtype,
whereas the full view for a private type is always a new type.
(We considered allowing the completion of a @nt{private_type_declaration}
to be a @nt{subtype_declaration},
but the semantics just won't work.)
This difference is behind the fact that a generic actual type can be
class-wide, whereas the completion of a private type always declares
a specific type.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00401]}
The ancestor type specified in a @nt<private_extension_declaration>
and the parent type specified in the corresponding declaration
of a record extension given in the private part need not be the
same@Chg{Version=[2],New=[. If the ancestor
type is not an interface type,],Old=[ @em]} the parent type of the full view
can be any descendant of the ancestor type.
In this case, for a primitive subprogram that is inherited from the
ancestor type and
not overridden, the formal parameter names and default expressions (if any)
come from the corresponding primitive subprogram of the specified ancestor
type, while the body comes from the corresponding primitive subprogram
of the parent type of the full view.
See @RefSecNum{Dispatching Operations of Tagged Types}.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00401]}
@ChgAdded{Version=[2],Text=[If the ancestor type specified in a
@nt{private_extension_declaration} is an
interface type, the parent type can be any type so long as the full view is a
descendant of the ancestor type. The progenitor types specified in a
@nt{private_extension_declaration} and the progenitor types specified in the
corresponding declaration of a record extension given in the private part need
not be the same @em the only requirement is that the private extension and the
record extension be descended from the same set of interfaces.]}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of private type declarations:}
@begin{Example}
@key[type] Key @key[is] @key[private];
@key[type] File_Name @key[is] @key[limited] @key[private];
@end{Example}

@begin{WideAbove}
@leading@keepnext@i{Example of a private extension declaration:}
@end{WideAbove}
@begin{Example}
@key[type] List @key[is] @key[new] Ada.Finalization.Controlled @key[with] @key[private];
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The syntax for a @nt{private_type_declaration} is augmented to
allow the reserved word @key{tagged}.

In Ada 83, a private type without discriminants cannot be completed
with a type with discriminants.
Ada 95 allows the full view to have discriminants,
so long as they have defaults
(that is, so long as the first subtype is definite).
This change is made for uniformity with generics,
and because the rule as stated is simpler and easier to remember
than the Ada 83 rule.
In the original version of Ada 83, the same restriction applied
to generic formal private types.
However, the restriction was removed by the ARG for generics.
In order to maintain the @lquotes@;generic contract/private type contract analogy@rquotes@;
discussed above, we have to apply the same rule to
package-private types.
Note that a private untagged type without discriminants can be
completed with a tagged type with discriminants only if the
full view is constrained, because discriminants of tagged types
cannot have defaults.
@end{Extend83}

@begin{DiffWord83}
RM83-7.4.1(4),
@lquotes@;Within the specification of the package that declares a private type
and before the end of the corresponding full type declaration, a
restriction applies....@rquotes@;,
is subsumed (and corrected) by the rule that
a type shall be completely defined before it is frozen,
and the rule that the parent type of a derived type declaration shall be
completely defined, unless the derived type is a private extension.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01],ARef=[AI95-00396-01],ARef=[AI95-00401-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Added @nt{interface_list} to private extensions to
  support interfaces and multiple inheritance
  (see @RefSecNum{Interface Types}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[A private extension may specify that it is a
  limited type. This is required for interface ancestors (from which
  limitedness is not inherited), but it is generally useful as documentation of
  limitedness.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00443-01]}
  @ChgAdded{Version=[2],Text=[A private extension may specify that it is a
  synchronized type. This is required in order so that a regular limited
  interface can be used as the ancestor of a synchronized type (we do not
  allow hiding of synchronization).]}
@end{Extend95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0183-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  An optional @nt{aspect_specification} can be used in a @nt{private_type_declaration} and
  a @nt{private_extension_declaration}.
  This is described in @RefSecNum{Aspect Specifications}.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0110-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> The description of how a
  private extension inherits characteristics was made consistent with the
  way formal derived types inherit characteristics (see
  @RefSecNum{Formal Private and Derived Types}).]}
@end{DiffWord2005}



@LabeledSubClause{Private Operations}

@begin{Intro}
@Redundant[For a type declared in the visible part of a package
or generic package, certain operations on the type
do not become visible until later in the package @em
either in the private part or the body.
@Defn{private operations}
Such @i{private operations} are available only inside the declarative
region of the package or generic package.]
@end{Intro}

@begin{StaticSem}
The predefined operators that exist for a given type are determined by
the classes to which the type belongs.
For example, an integer type has a predefined "+" operator.
In most cases, the predefined operators of a type are declared
immediately after the definition of the type;
the exceptions are explained below.
Inherited subprograms are also implicitly declared
immediately after the definition of the type,
except as stated below.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0019],ARef=[AI95-00033-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0029-1]}
For a composite type, the characteristics
(see @RefSecNum{Private Types and Private Extensions})
of the type are determined in part by the
characteristics of its component types.
At the place where the composite type is declared,
the only characteristics of component types used are those
characteristics visible at that place.
If later @Chg{New=[immediately within the declarative region in which the
composite type is declared],
Old=[within the immediate scope of the composite type]} additional
characteristics become visible for a component type,
then any corresponding characteristics become visible for the composite
type.
Any additional predefined operators are implicitly declared at that
place.@Chg{Version=[3],New=[ If there is no such place, then additional
predefined operators are not declared at all, but they still exist.],Old=[]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0029-1]}
  @ChgAdded{Version=[3],Text=[We say that the predefined operators exist
  because they can emerge in some unusual generic instantiations. See
  @RefSecNum{Formal Types}.]}
@end{Reason}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0029-1]}
  @ChgAdded{Version=[3],Text=[The predefined operators for the underlying class
  of a type always exist, even if there is no visibility on that underlying
  class. This rule is simply about where (if ever) those operators are
  declared (and thus become usable). The @ldquote@;additional predefined
  operators@rdquote defined by this rule are any that are not declared at the
  point of the original type declaration. For instance, a type derived from a
  private type whose full type is type String always will have a ">"
  operator, but where that operator is declared (and thus whether it is
  visible) will depend on the visibility of the full type of the parent type.]}
@end{Discussion}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0019],ARef=[AI95-00033-01]}
The corresponding rule applies to a type defined by a
@nt{derived_type_definition},
if there is a place @Chg{New=[immediately within the declarative region in which
the type is declared],
Old=[within its immediate scope]} where additional
characteristics of its parent type become visible.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0019],ARef=[AI95-00033-01]}
@Defn{become nonlimited}
@Defn2{Term=[nonlimited type],Sec=(becoming nonlimited)}
@Defn2{Term=[limited type],Sec=(becoming nonlimited)}
@Redundant[For example, an array type whose component type is limited
private becomes nonlimited if the full view of the component type is
nonlimited and visible at some later place @Chg{New=[immediately within the declarative region in which
the array type is declared.],
Old=[within the immediate scope of the array type.]}
In such a case, the predefined "=" operator is implicitly declared at
that place, and assignment is allowed after that place.]

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0115-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[A type is a @i<descendant>@Defn2{Term=[descendant],
Sec=[of the full view of a type]} of the full view of some ancestor
of its parent type only if the current view it has of its parent is a
descendant of the full view of that ancestor. More generally, at any given
place, a type is descended from the same view of an ancestor as that from which
the current view of its parent is descended. This view determines what
characteristics are inherited from the ancestor@Redundant[, and, for example,
whether the type is considered to be a descendant of a record type, or a
descendant only through record extensions of a more distant ancestor].]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0115-1]}
@ChgAdded{Version=[3],Text=[@Redundant[It is possible for there to be places
where a derived type is visibly a descendant of an ancestor type, but not a
descendant of even a partial view of the ancestor type, because the parent
of the derived type is not visibly a descendant of the ancestor.  In
this case, the derived type inherits no characteristics from that
ancestor, but nevertheless is within the derivation class of the
ancestor for the purposes of type conversion, the "covers"
relationship, and matching against a formal derived type. In this
case the derived type is considered to be a @i<descendant> of an
incomplete view of the ancestor.@Defn2{Term=[descendant],
Sec=[of an incomplete view]}]]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[Here is an example of this situation:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[package] P @key[is]
   @key[type] T @key[is] @key[private];
   C : @key[constant] T;
@key[private]
   @key[type] T @key[is new] Integer;
   C : @key[constant] T := 42;
@key[end] P;]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[with] P;
@key[package] Q @key[is]
    @key[type] T2 @key[is new] P.T;
@key[end] Q;]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[with] Q;
@key[package] P.Child @key[is]
    @key[type] T3 @key[is new] Q.T2;
@key[private]
    Int : Integer := 52;
    V : T3 := T3(P.C);  -- @Examcom{Legal: conversion allowed}
    W : T3 := T3(Int);  -- @Examcom{Legal: conversion allowed}
    X : T3 := T3(42);   -- @Examcom{Error: T3 is not a numeric type}
    Y : T3 := X + 1;    -- @Examcom{Error: no visible "+" operator}
    Z : T3 := T3(Integer(W) + 1);   -- @Examcom{Legal: convert to Integer first}
@key[end] P.Child;]}
@end{Example}
@end{Discussion}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0019],ARef=[AI95-00033-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0029-1]}
Inherited primitive subprograms follow a different rule.
For a @nt{derived_type_definition},
each inherited primitive subprogram
is implicitly declared at the earliest place, if any,
@Chg{New=[immediately within the declarative region in which],
Old=[within the immediate scope of]} the @nt{type_declaration}@Chg{New=[ occurs],Old=[]},
but after the @nt{type_declaration},
where the corresponding declaration from the parent is
visible.
If there is no such place, then the inherited subprogram
is not declared at all@Chg{Version=[3],New=[, but it still exists],Old=[]}.
@Redundant[@Chg{Version=[3],New=[For a tagged type, it is possible to
dispatch to an],Old=[An]} inherited subprogram that is not declared at
all@Chg{Version=[3],New=[],Old=[cannot be named in a call and cannot be
overridden, but for a tagged type, it is possible to dispatch to it]}.]

For a @nt{private_extension_declaration}, each inherited subprogram is
declared immediately after the @nt{private_extension_declaration}
if the corresponding declaration from the ancestor is visible at that
place.
Otherwise, the inherited subprogram is not declared for the private
extension, @Redundant[though it might be for the full type].
@begin{Reason}
  @ChgRef{Version=[1],Kind=[Revised]}
  There is no need for the @lquotes@;earliest place
  @Chg{New=[immediately within the declarative region],
  Old=[within the immediate scope]}@rquotes@;
  business here, because a @nt{private_extension_declaration} will be
  completed with a @nt{full_type_declaration}, so we can hang the
  necessary private implicit declarations on the @nt{full_type_declaration}.
@end{Reason}
@begin{Discussion}
The above rules matter only when the component type (or parent type) is
declared in the visible part of a package, and the composite type (or
derived type) is declared within the declarative region of that package
(possibly in a nested package or a child package).

@leading@keepnext@;Consider:
@begin{Example}
@key[package] Parent @key[is]
    @key[type] Root @key[is] @key[tagged] @key[null] @key[record];
    @key[procedure] Op1(X : Root);

    @key[type] My_Int @key[is] @key[range] 1..10;
@key[private]
    @key[procedure] Op2(X : Root);

    @key[type] Another_Int @key[is] @key[new] My_Int;
    @key[procedure] Int_Op(X : My_Int);
@key[end] Parent;

@key[with] Parent; @key[use] Parent;
@key[package] Unrelated @key[is]
    @key[type] T2 @key[is] @key[new] Root @key[with] @key[null] @key[record];
    @key[procedure] Op2(X : T2);
@key[end] Unrelated;

@key[package] Parent.Child @key[is]
    @key[type] T3 @key[is] @key[new] Root @key[with] @key[null] @key[record];
    --@RI{ Op1(T3) implicitly declared here.}

    @key[package] Nested @key[is]
        @key[type] T4 @key[is] @key[new] Root @key[with] @key[null] @key[record];
    @key[private]
        ...
    @key[end] Nested;
@key[private]
    --@RI{ Op2(T3) implicitly declared here.}
    ...
@key[end] Parent.Child;

@key[with] Unrelated; @key[use] Unrelated;
@key[package] @key[body] Parent.Child @key[is]
    @key[package] @key[body] Nested @key[is]
        --@RI{ Op2(T4) implicitly declared here.}
    @key[end] Nested;

    @key[type] T5 @key[is] @key[new] T2 @key[with] @key[null] @key[record];
@key[end] Parent.Child;
@end{Example}

Another_Int does not inherit Int_Op,
because Int_Op does not @lquotes@;exist@rquotes@; at the place
where Another_Int is declared.

@ChgRef{Version=[1],Kind=[Revised]}
Type T2 inherits Op1 and Op2 from Root.
However, the inherited Op2 is never declared,
because Parent.Op2 is never visible @Chg{New=[immediately within the declarative region],
Old=[within the immediate scope]} of T2.
T2 explicitly declares its own Op2,
but this is unrelated to the inherited one @em it
does not override the inherited one,
and occupies a different slot in the type descriptor.

T3 inherits both Op1 and Op2. Op1 is implicitly declared immediately
after the type declaration,
whereas Op2 is declared at the beginning of the private part.
Note that if Child were a private child of Parent,
then Op1 and Op2 would both be implicitly declared immediately after the
type declaration.

@ChgRef{Version=[1],Kind=[Revised]}
T4 is similar to T3, except that the earliest place
@Chg{New=[immediately within the declarative region containing T4],
Old=[within T4's immediate scope]}
where Root's Op2 is visible is in the body of Nested.

If T3 or T4 were to declare a type-conformant Op2,
this would override the one inherited from Root.
This is different from the situation with T2.


T5 inherits Op1 and two Op2's from T2.
Op1 is implicitly declared immediately after the declaration of T5,
as is the Op2 that came from Unrelated.Op2.
However, the Op2 that originally came from Parent.Op2 is never
implicitly declared for T5,
since T2's version of that Op2 is never visible (anywhere @em it never
got declared either).

For all of these rules, implicit private parts and bodies are assumed as
needed.

@leading@keepnext@;It is possible for characteristics of a type to be revealed
in more than one place:

@begin{Example}
@key[package] P @key[is]
    @key[type] Comp1 @key[is] @key[private];
@key[private]
    @key[type] Comp1 @key[is] @key[new] Boolean;
@key[end] P;

@key[package] P.Q @key[is]
    @key[package] R @key[is]
        @key[type] Comp2 @key[is] @key[limited] @key[private];
        @key[type] A @key[is] @key[array](Integer @key[range] <>) @key[of] Comp2;
    @key[private]
        @key[type] Comp2 @key[is] @key[new] Comp1;
        --@RI{ A becomes nonlimited here.}
        --@RI{ "="(A, A) return Boolean is implicitly declared here.}
        ...
    @key[end] R;
@key[private]
    --@RI{ Now we find out what Comp1 really is, which reveals}
    --@RI{ more information about Comp2, but we're not within}
    --@RI{ the immediate scope of Comp2, so we don't do anything}
    --@RI{ about it yet.}
@key[end] P.Q;

@key[package] @key[body] P.Q @key[is]
    @key[package] @key[body] R @key[is]
        --@RI{ Things like "@key[xor]"(A,A) return A are implicitly}
        --@RI{ declared here.}
    @key[end] R;
@key[end] P.Q;
@end{Example}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0019],ARef=[AI95-00033-01]}
@ChgAdded{Version=[1],Type=[Leading],Text=[We say @i<immediately> within
the declarative region in order that
types do not gain operations within a nested scope. Consider:]}
@begin{Example}
@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@Key[package] Outer @key[is]
    @key[package] Inner @key[is]
        @key[type] Inner_Type @key[is] @key[private];
    @key[private]
        @key[type] Inner_Type @key[is] @key[new] Boolean;
    @key[end] Inner;
    @key[type] Outer_Type @key[is] @key[array](Natural @key[range] <>) @key[of] Inner.Inner_Type;
@key[end] Outer;],Old=[]}

@ChgRef{Version=[1],Kind=[Added]}
@Chg{New=[@key[package] @key[body] Outer @key[is]
    @key[package] @key[body] Inner @key[is]
        -- At this point, we can see that Inner_Type is a Boolean type.
        -- But we don't want Outer_Type to gain an "and" operator here.
    @key[end] Inner;
@key[end] Outer;],Old=[]}
@end{Example}
@end{Discussion}

@leading@Redundant[The Class attribute is defined for tagged subtypes in
@RefSecNum{Tagged Types and Type Extensions}.
In addition,] for
@PrefixType{every subtype S of an untagged private type
whose full view is tagged},
the following attribute is defined:
@begin(description)
@Attribute{Prefix=<S>, AttrName=<Class>,
  Text=<Denotes the class-wide subtype corresponding to the full
  view of S.
  This attribute is allowed only from the beginning of the private part
  in which the full view is declared, until the declaration of the full
  view.
  @Redundant[After the full view,
  the Class attribute of the full view can be used.]>}
@end(description)
@EndPrefixType{}
@end{StaticSem}

@begin{Notes}
Because a partial view and a full view
are two different views of one and the same type,
outside of the defining package the characteristics of the type are
those defined by the visible part.
Within these outside program units the type is just a private type
or private extension,
and any language rule that applies only to another class of types
does not apply. The fact that the full declaration might implement
a private type with a type of a particular class (for example, as
an array type) is relevant only
within the declarative region of the package itself
including any child units.

@NoPrefix@;The consequences of this actual implementation are, however, valid
everywhere. For example: any default initialization of components
takes place; the attribute Size provides the size of the full view;
finalization is still done for controlled components of the full view;
task dependence rules still apply to components that are task
objects.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
Partial views provide @Chg{Version=[2],New=[initialization],
Old=[assignment (unless the view is limited)]},
membership tests, selected components for the selection of
discriminants and inherited components, qualification,
and explicit conversion.@Chg{Version=[2],New=[ Nonlimited partial views
also allow use of @nt{assignment_statement}s.],Old=[]}

For a subtype S of a partial view, S'Size is defined
(see @RefSecNum{Operational and Representation Attributes}).
For an object A of a partial view,
the attributes A'Size and A'Address are defined
(see @RefSecNum{Operational and Representation Attributes}).
The Position, First_Bit, and Last_Bit attributes
are also defined for discriminants and inherited components.
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a type with private operations:}
@begin{Example}
@key[package] Key_Manager @key[is]
   @key[type] Key @key[is] @key[private];
   Null_Key : @key[constant] Key; --@RI{ a deferred constant declaration (see @RefSecNum{Deferred Constants})}
   @key[procedure] Get_Key(K : @key[out] Key);
   @key[function] "<" (X, Y : Key) @key[return] Boolean;
@key[private]
   @key[type] Key @key[is] @key[new] Natural;
   Null_Key : @key[constant] Key := Key'First;
@key[end] Key_Manager;

@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@key[package] @key[body] Key_Manager @key[is]
   Last_Key : Key := Null_Key;
   @key[procedure] Get_Key(K : @key[out] Key) @key[is]
   @key[begin]
      Last_Key := Last_Key + 1;
      K := Last_Key;
   @key[end] Get_Key;

   @key[function] "<" (X, Y : Key) @key[return] Boolean @key[is]
   @key[begin]
      @key[return] Natural(X) < Natural(Y);
   @key[end] "<";
@key[end] Key_Manager;
@end{Example}
@end{Examples}

@begin{Notes}
@i{Notes on the example:}
Outside of the package Key_Manager, the operations available for
objects of type Key include assignment, the comparison for equality
or inequality, the procedure Get_Key and the operator "<"; they do
not include other relational operators such as ">=", or arithmetic
operators.

@NoPrefix@;The explicitly declared operator "<" hides the predefined operator
"<" implicitly declared by the @nt{full_type_declaration}. Within the
body of the function, an explicit conversion of X and Y to the
subtype Natural is necessary to invoke the "<" operator of the parent
type.
Alternatively, the result of the function could be written as not (X
>= Y), since the operator ">=" is not redefined.

@NoPrefix@;The value of the variable Last_Key, declared in the package body,
remains unchanged between calls of the procedure Get_Key. (See also
the NOTES of @RefSecNum{Package Bodies}.)
@end{Notes}

@begin{DiffWord83}
The phrase in RM83-7.4.2(7), @lquotes@;...after the full type declaration@rquotes@;,
doesn't work in the presence of child units, so we define that rule in
terms of visibility.

The definition of the Constrained attribute for private types
has been moved to @lquotes@;Obsolescent Features.@rquotes@;
(The Constrained attribute of an object has not been moved there.)
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0018],ARef=[AI95-00033-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified when additional
  operations are declared.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Revised the note on operations of partial views
  to reflect that limited types do have an assignment operation, but not
  @nt{assignment_statement}s.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0029-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Revised the wording to say
  that predefined operations still exist even if they are never declared,
  because it is possible to reference them in a generic unit.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0115-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that the characteristics
  of a descendant of a private type depend on the visibility of the full
  view of the direct ancestor. This has to be the case (so that privacy is not
  violated), but it wasn't spelled out in earlier versions of Ada.]}
@end{DiffWord2005}


@LabeledAddedSubClause{Version=[3],Name=[Type Invariants]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[For a private type or private
extension, the following language-defined aspects may be specified with an
@nt{aspect_specification} (see @RefSecNum{Aspect Specifications}):]}

@begin{Description}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0250-1]}
@ChgAdded{Version=[3],Text=[Type_Invariant@\This aspect
   shall be specified by an @nt{expression}, called an @i<invariant
   expression>.@Defn{invariant expression}
   Type_Invariant may be specified on a @nt{private_@!type_@!declaration}, on a
   @nt{private_@!extension_@!declaration}, or on a @nt{full_@!type_@!declaration} that
   declares the completion of a private type or private
   extension.@AspectDefn{Type_Invariant}]}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Type_Invariant],
    Text=[@ChgAdded{Version=[3],Text=[A condition that must hold true for all
      objects of a type.]}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1]}
@ChgAdded{Version=[3],Text=[Type_Invariant'Class@\This aspect
   shall be specified by an @nt{expression}, called an @i<invariant
   expression>.
   Type_Invariant'Class may be specified on a @nt{private_@!type_@!declaration} or a
   @nt{private_@!extension_@!declaration}.@AspectDefn{Type_Invariant'Class}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0254-1]}
  @ChgAdded{Version=[3],Text=[A class-wide type invariant cannot be hidden
  in the private part, as the creator of an extension needs to know about it
  in order to conform to it in any new or overriding operations. On the other
  hand, a specific type invariant is not inherited, so that no operation
  outside of the original package needs to conform to it; thus there is no
  need for it to be visible.]}
@end{Reason}

  @ChgAspectDesc{Version=[3],Kind=[AddedNormal],Aspect=[Type_Invariant'Class],
    Text=[@ChgAdded{Version=[3],Text=[A condition that must hold true for all
      objects in a class of types.]}]}

@end{Description}

@begin{Resolution}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1]}
@ChgAdded{Version=[3],Text=[The expected type for an invariant expression
is any boolean type.@PDefn2{Term=[expected type],
Sec=(invariant expression)}]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1]}
@ChgAdded{Version=[3],Text=[@Redundant[Within an invariant expression, the
identifier of the first subtype of the associated type denotes the current
instance of the type.] Within an invariant expression associated with type
@i<T>, the type of the current instance is @i<T> for the Type_Invariant aspect
and @i<T>'Class for the Type_Invariant'Class aspect.]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The first sentence is given formally in
  @RefSecNum{Aspect Specifications}.]}
@end{TheProof}

@end{Resolution}

@begin{Legality}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1]}
@ChgAdded{Version=[3],Text=[@Redundant[The Type_Invariant'Class aspect shall not be
specified for an untagged type.] The Type_Invariant aspect shall not be specified
for an abstract type.]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The first sentence is given formally in
  @RefSecNum{Aspect Specifications}.]}
@end{TheProof}
@end{Legality}

@begin{StaticSem}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0250-1]}
@ChgAdded{Version=[3],Text=[@Redundant[If the Type_Invariant aspect is
specified for a type @i<T>, then the invariant expression applies to @i<T>.]]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1]}
@ChgAdded{Version=[3],Text=[@Redundant[If the Type_Invariant'Class aspect is
specified for a tagged type @i<T>, then the invariant expression applies to all
descendants of @i<T>.]]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=["Applies" is formally defined in
  @RefSecNum{Aspect Specifications}.]}
@end{TheProof}
@end{StaticSem}
@begin{Runtime}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0247-1],ARef=[AI05-0290-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[If one or more invariant expressions
apply to a type @i<T>, then an invariant check is performed at the
following places, on the specified object(s):@Defn{invariant check}@Defn2{Term=[check, language-defined],
  Sec=[controlled by assertion policy]}]}

@begin{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[After successful default initialization of an
    object of type @i<T>, the check is performed on the new object;]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[After successful conversion to type @i<T>, the
    check is performed on the result of the conversion;]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Text=[For a view conversion, outside the immediate scope
  of @i<T>, that converts from a descendant of @i<T> (including @i<T> itself) to
  an ancestor of type @i<T> (other than @i<T> itself), a check is performed on
  the part of the object that is of type @i<T>:]}

  @begin{InnerItemize}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[after assigning to the view conversion; and]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[after successful return from a call that passes the view conversion
    as an @key[in out] or @key[out] parameter.]}
  @end{InnerItemize}

  @begin{Ramification}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[For a single view conversion that converts
    between distantly related types, this rule could be triggered for
    multiple types and thus multiple invariant checks may be needed.]}
  @end{Ramification}
  @begin{ImplNote}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0299-1]}
    @ChgAdded{Version=[3],Text=[For calls to inherited subprograms (including
    dispatching calls), the implied view conversions mean that a wrapper is
    probably needed. (See the Note at the bottom of this subclause for more
    on the model of checks for inherited subprograms.)]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[For view conversions involving class-wide
    types, the exact checks needed may not be known at compile-time. One
    way to deal with this is to have an implicit dispatching operation
    that is given the object to check and the tag of the target of the
    conversion, and which first checks if the passed tag is not for itself,
    and if not, checks the its invariant on the object and then calls
    the operation of its parent type. If the tag is for itself, the
    operation is complete.]}
  @end{ImplNote}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[After a successful call on the Read or Input
    stream attribute of the type @i<T>, the check is performed on the object
    initialized by the stream attribute;]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[An invariant is checked upon successful return from a call
  on any subprogram or entry that:]}
  @begin{Itemize}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0269-1]}
    @ChgAdded{Version=[3],Text=[is declared within the immediate
      scope of type @i<T> (or by an instance of a generic unit, and the generic
      is declared within the immediate scope of type @i<T>), and]}

    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[is visible outside the immediate scope of type
      @i<T> or overrides an operation that is visible outside the immediate
      scope of @i<T>, and]}

    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0289-1]}
    @ChgAdded{Version=[3],Text=[has a result with a part of type @i<T>, or one
      or more parameters with a part of type @i<T>, or
      an access to variable parameter whose designated type has a part of type
      @i<T>.]}
  @end{Itemize}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],NoPrefix=[T],Text=[The check is performed on each such
  part of type @i<T>.]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0290-1]}
@ChgAdded{Version=[3],Text=[If performing checks is required by the Invariant or
Invariant'Class assertion policies (see
@RefSecNum{Pragmas Assert and Assertion_Policy}) in effect at the point of
corresponding aspect specification applicable to a given type, then the
respective invariant expression is considered
@i(enabled).@Defn2{Term=[enabled],Sec=[invariant expression]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[If a class-wide invariant expression is enabled
  for a type, it remains enabled when inherited by descendants of that type,
  even if the policy in effect is Ignore for the inheriting type.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0250-1],ARef=[AI05-0289-1],ARef=[AI05-0290-1]}
@ChgAdded{Version=[3],Text=[The invariant check consists of the evaluation of
each enabled invariant expression that applies to @i<T>, on each of the objects
specified above. If any of these evaluate to False,
Assertions.Assertion_Error is raised at the point of the object
initialization, conversion, or call.@Defn2{Term=(Assertion_Error),
Sec=(raised by failure of run-time check)} If a given call requires more than one
evaluation of an invariant expression, either for multiple objects of a single
type or for multiple types with invariants, the evaluations are performed in
an arbitrary order, and if one of them evaluates to False, it is not specified whether
the others are evaluated. Any invariant check is performed prior to copying back
any by-copy @key[in out] or @key[out] parameters. Invariant checks,
any postcondition check, and any
constraint or predicate checks associated with @key[in out] or @key[out]
parameters are performed in an arbitrary order.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0247-1],ARef=[AI05-0250-1]}
@ChgAdded{Version=[3],Text=[The invariant checks performed on a call are
determined by the subprogram or entry actually invoked, whether directly, as
part of a dispatching call, or as part of a call through an access-to-subprogram
value.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Invariant checks on subprogram return are not
  performed on objects that are accessible only through access values. It is
  also possible to call through an access-to-subprogram value and reach a
  subprogram body that has visibility on the full declaration of a type, from
  outside the immediate scope of the type. No invariant checks will be performed
  if the designated subprogram is not itself externally visible. These cases
  represent "holes" in the protection provided by invariant checks; but note
  that these holes cannot be caused by clients of the type @i<T> with the
  invariant without help for the designer of the package containing @i<T>.]}
@end{Ramification}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The implementation might want to produce a warning
  if a private extension has an ancestor type that is a visible extension, and
  an invariant expression depends on the value of one of the components from a
  visible extension part.]}
@end{ImplNote}

@end{Runtime}

@begin{Notes}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0250-1],ARef=[AI05-0269-1]}
  @ChgAdded{Version=[3],Text=[For a call of a primitive subprogram of type
  @i<NT> that is inherited from type @i<T>, the specified checks of the specific
  invariants of both the types @i<NT> and @i<T> are performed. For a call of a
  primitive subprogram of type @i<NT> that is overridden for type @i<NT>, the
  specified checks of the specific invariants of only type @i<NT> are
  performed.]}

@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This follows from the definition of a call on
  an inherited subprogram as view conversions of the parameters of the type
  and a call to the original subprogram
  (see @RefSecNum{Derived Types and Classes}), along with the normal invariant
  checking rules. In particular, the call to the original subprogram takes
  care of any checks needed on type @i<T>, and the checks required
  on view conversions take care of any checks needed on type @i<NT>,
  specifically on @key[in out] and @key[out] parameters. We require this in
  order that the semantics of an explicitly defined wrapper that does nothing
  but call the original subprogram is the same as that of an inherited
  subprogram.]}
@end{TheProof}

@end{Notes}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0146-1],ARef=[AI05-0247-1],ARef=[AI05-0250-1],ARef=[AI05-0289-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Type_Invariant aspects are new.]}
@end{Extend2005}



@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledClause{Deferred Constants}

@begin{Intro}
@redundant[Deferred constant declarations may be used to declare constants
in the visible part of a package,
but with the value of the constant given in the private part.
They may also be used to declare constants imported from other
languages (see @RefSecNum{Interface to Other Languages}).]
@end{Intro}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1],ARef=[AI05-0269-1]}
@Redundant[@Defn{deferred constant declaration}
A @i(deferred constant declaration) is an @nt<object_declaration>
with the reserved word @key(constant) but no initialization expression.]
@Defn{deferred constant}
The constant declared by a deferred constant declaration is called
a @i{deferred constant}.
@PDefn2{Term=[requires a completion], Sec=(deferred constant declaration)}
@Chg{Version=[3],New=[@Redundant[Unless the Import aspect
(see @RefSecNum{Interfacing Aspects}) is True for a deferred constant
declaration, the ]],Old=[A]} deferred constant declaration requires a completion,
which shall be a full constant declaration
(called the @i{full declaration} of the deferred
constant)@Chg{Version=[3],New=[],Old=[,
or a @nt{pragma} Import (see @RefSecNum(Interface to Other Languages))]}.
@Defn{full declaration}
@begin{TheProof}
The first sentence is redundant, as it is stated officially in
@RefSecNum(Object Declarations).

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0229-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[The first part of the last sentence is redundant,
as no imported entity may have a completion, as stated in
@RefSecNum{Interfacing Aspects}.]}
@end{TheProof}

@leading@;A deferred constant declaration that is completed
by a full constant declaration shall occur immediately
within the visible part of a @nt<package_specification>.
For this case, the following additional rules apply to the
corresponding full declaration:
@begin(itemize)
  The full declaration shall occur immediately
  within the private part of the same package;

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00385-01]}
  The deferred and full constants shall have the same type@Chg{Version=[2],
  New=[, or shall have statically matching anonymous access subtypes],Old=[]};
  @begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00385-01]}
  This implies that
  both the deferred declaration and the full declaration
  have to have a @nt<subtype_indication>@Chg{Version=[2],New=[ or
  @nt{access_definition}],Old=[]} rather than an
  @nt<array_type_definition>, because each @nt{array_type_definition}
  would define a new type.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00385-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0062-1],ARef=[AI05-0262-1]}
  If the @Chg{Version=[2],New=[deferred constant declaration includes
  a],Old=[subtype defined by the]} @nt<subtype_indication>
  @Chg{Version=[3],New=[@i<S> ],Old=[]}@Chg{Version=[2],
  New=[that defines a],Old=[in the deferred declaration is]}
  constrained@Chg{Version=[2],New=[ subtype],Old=[]}, then the
  @Chg{Version=[3],New=[constraint],Old=[subtype]} defined
  by the @nt<subtype_indication> in the full declaration shall match
  @Chg{Version=[3],New=[the constraint defined by @i<S>],Old=[it]}
  statically.@Redundant[ On the other hand,
  if the subtype of the deferred constant is unconstrained,
  then the full declaration is still allowed to impose a constraint.
  The constant itself will be constrained, like all constants;]

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
  If the deferred constant declaration
  includes the reserved word @key(aliased), then the
  full declaration shall also@Chg{Version=[2],New=[;],Old=[.]}
  @begin{Ramification}
    On the other hand, the full constant can be aliased
    even if the deferred constant is not.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[If the subtype of the deferred constant
  declaration excludes null, the subtype of the full declaration shall also
  exclude null.]}
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[Added]}
    @ChgAdded{Version=[2],Text=[On the other hand, the full constant can
    exclude null even if the deferred constant does not. But that can only
    happen for a @nt{subtype_indication}, as anonymous access types are
    required to statically match (which includes any @nt{null_exclusion}).]}
  @end{Ramification}
@end(itemize)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@Redundant[A deferred constant declaration
  @Chg{Version=[3],New=[for which the],Old=[that is completed by
  a @nt{pragma}]} Import @Chg{Version=[3],New=[aspect is True ],Old=[]}need
  not appear in the visible part of a @nt{package_specification},
  and has no full constant declaration.]


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
The completion of a deferred constant declaration shall occur
before the constant is frozen
(see @Chg{Version=[2],New=[@RefSecNum{Freezing Rules}],
Old=[@RefSecNum{Deferred Constants}]}).

@end{Legality}

@begin{RunTime}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0004-1]}
@PDefn2{Term=[elaboration], Sec=(deferred constant declaration)}
The elaboration of a deferred constant declaration
elaborates the @nt<subtype_indication>@Chg{Version=[3],New=[,
@nt<access_definition>,],Old=[]} or (only allowed in the case of an
imported constant) the @nt<array_type_definition>.
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0004-1]}
  @ChgAdded{Version=[3],Text=[For nonimported constants, these elaborations
  cannot require any code or checks for a legal program, because the given
  @nt<subtype_indication> has to be indefinite or statically match that of
  the full constant, meaning that either it is a @nt<subtype_mark> or it has
  static constraints. If the deferred constant instead has an
  @nt<access_definition>, the designated subtype must be a @nt<subtype_mark>.
  We still say that these are elaborated, however, because part of elaboration
  is creating the type, which is clearly needed for @nt<access_definition>s.
  (A deferred constant and its full constant have different types when
  they are specified by an @nt<access_definition>, although there is no
  visible effect of these types being different as neither can be named.)]}
@end{Ramification}
@end{RunTime}

@begin{Notes}
The full constant declaration for a deferred constant that is of a given
private type or private extension is not allowed before the corresponding
@nt{full_type_declaration}. This is a consequence of the freezing
rules for types
(see @RefSecNum{Freezing Rules}).
@begin{Ramification}
Multiple or single declarations are allowed for the
deferred and the full declarations, provided that the
equivalent single declarations would be allowed.

Deferred constant declarations are useful for declaring constants of
private views, and types with components of private views.
They are also useful for declaring
access-to-constant objects that designate
variables declared in the private part of a package.
@end{Ramification}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Examples of deferred constant declarations:}
@begin{Example}
Null_Key : @key[constant] Key;      --@RI[ see @RefSecNum{Private Operations}]

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
CPU_Identifier : @key[constant] String(1..8)@Chg{Version=[3],New=[],Old=[;]}
@Chg{Version=[3],New=[   @key[with]],Old=[@key[pragma]]} Import@Chg{Version=[3],New=[ => True, Convention => ],Old=[(]}Assembler, @Chg{Version=[3],New=[],Old=[CPU_Identifier, ]}Link_Name => "CPU_ID"@Chg{Version=[3],New=[],Old=[)]};
                              --@RI[ see @RefSecNum{Interfacing Aspects}]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
In Ada 83, a deferred constant is required to be of a private type
declared in the same visible part.
This restriction is removed for Ada 95;
deferred constants can be of any type.

In Ada 83, a deferred constant declaration was not permitted to
include a constraint, nor the reserved word @key(aliased).

In Ada 83, the rules required conformance of type marks; here
we require static matching of subtypes if the deferred constant
is constrained.

A deferred constant declaration can be completed with a @nt{pragma}
Import. Such a deferred constant declaration need not be within a
@nt{package_specification}.

The rules for too-early uses of deferred constants are modified in
Ada 95 to allow more cases, and catch all errors at compile time.
This change is necessary in order to allow deferred constants of a
tagged type without violating the principle that for a dispatching call,
there is always an implementation to dispatch to.
It has the beneficial side effect of catching some Ada-83-erroneous
programs at compile time.
The new rule fits in well with the new freezing-point rules.
Furthermore, we are trying to convert undefined-value problems into
bounded errors, and we were having trouble for the case of deferred
constants.
Furthermore, uninitialized deferred constants cause trouble for the
shared variable / tasking rules, since they are really variable, even
though they purport to be constant.
In Ada 95, they cannot be touched until they become constant.

Note that we do not consider this change to be an upward
incompatibility, because it merely changes an erroneous execution in Ada
83 into a compile-time error.

The Ada 83 semantics are unclear in the case where
the full view turns out to be an access type.
It is a goal of the language design to prevent uninitialized access
objects.
One wonders if the implementation is required to initialize the
deferred constant to null, and then initialize it (again!) to its
real value.
In Ada 95, the problem goes away.
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Since deferred constants can now be of a nonprivate type,
we have made this a stand-alone @Chg{Version=[3],New=[subclause],Old=[clause]},
rather than a subclause of @RefSec{Private Types and Private Extensions}.

Deferred constant declarations used to have their own syntax, but now
they are simply a special case of @nt<object_declaration>s.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00385-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Deferred constants were enhanced to allow
  the use of anonymous access types in them.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[Added matching rules for subtypes that
  exclude null.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0062-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected rules so
  that the intent that a full constant may have a null exclusion even
  if the deferred constant does not is actually met.]}
@end{DiffWord2005}


@LabeledClause{Limited Types}

@begin{Intro}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@redundant[A limited type is (a view of) a type for which
  @Chg{Version=[2],New=[copying (such as for an @nt{assignment_statement})],
    Old=[the assignment operation]} is not allowed.
  A nonlimited type is a (view of a) type for which
  @Chg{Version=[2],New=[copying], Old=[the assignment operation]} is allowed.]
@begin{Discussion}
The concept of the @i(value) of a limited type is difficult
to define, since the abstract value of a limited type often
extends beyond its physical representation. In some
sense, values of a limited type cannot be divorced from
their object. The value @i(is) the object.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
In Ada 83, in the two places where limited types were defined
by the language, namely tasks and files, an implicit
level of indirection was implied by the semantics to
avoid the separation of the value from an associated
object.
In Ada 95, most limited types are passed by reference,
and even return-ed by reference.@Chg{Version=[2],New=[ In Ada 2005,
most limited types are built-in-place upon return, rather than returned
by reference. Thus the object @lquotes@;identity@rquotes is part of the
logical value of most limited types.],Old=[]}
@end{Discussion}
@begin{Honest}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00419-01]}
For a limited partial view whose full view is nonlimited,
@Chg{Version=[2],New=[copying],Old=[assignment]} is possible on parameter
passing and function return. To prevent any copying whatsoever, one should
make both the partial @i{and} full views limited.
@end{Honest}
@Comment{The below was moved from a ChgToGlossaryAlso.}
@ChgToGlossary{Version=[2],Kind=[Revised],Term=<Limited type>,
  Text=<A limited type is @Chg{Version=[2],New=[],Old=[(a view of) ]}a type
  for which
  @Chg{Version=[2],New=[copying (such as in an @nt{assignment_statement})],
    Old=[the assignment operation]} is not allowed.
  A nonlimited type is a @Chg{Version=[2],New=[],Old=[(view of a) ]}type for
  which
  @Chg{Version=[2],New=[copying], Old=[the assignment operation]} is allowed.>}
@end{Intro}

@begin{Legality}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00419-01]}
If a tagged record type has any limited components,
then the reserved word @key[limited] shall
appear in its @nt<record_type_definition>.@Chg{Version=[2],New=[ @Redundant[If
the reserved word @key[limited] appears in the definition of a
@nt{derived_type_definition}, its parent type and any progenitor interfaces
shall be limited.]],Old=[]}
@begin{TheProof}
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[The rule about the parent type being required
  to be limited can be found in @RefSecNum{Derived Types and Classes}. Rules
  about progenitor interfaces can be found in
  @RefSecNum{Interface Types}, specifically, a nonlimited interface can appear
  only on a nonlimited type. We repeat these rules here to gather these
  scattered rules in one obvious place.]}
@end{TheProof}
@begin{Reason}
@leading@;This prevents tagged limited types from becoming nonlimited.
Otherwise, the following could happen:
@begin{Example}
@key[package] P @key[is]
    @key[type] T @key[is] @key[limited] @key[private];
    @key[type] R @key[is] @key[tagged]
        @key[record] --@RI{ Illegal!}
               --@RI{ This should say @lquotes@;@key[limited record]@rquotes@;.}
            X : T;
        @key[end] @key[record];
@key[private]
    @key[type] T @key[is] @key[new] Integer; --@RI{ R becomes nonlimited here.}
@key[end] P;

@ChgRef{Version=[2],Kind=[Revised]}
@key[package] Q @key[is]
    @key[type] R2@Chg{Version=[2],New=[],Old=[(Access_Discrim : @key[access] ...)]} @key[is] @key[new] R @key[with]
        @key[record]
            Y : Some_Task_Type;
        @key[end] @key[record];
@key[end] Q;
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
If the above were legal,
then assignment would be defined for R'Class in the body of P,
which is bad news, given @Chg{Version=[2],New=[],Old=[the access discriminant
and ]}the task.
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00287-01],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0147-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[In the following contexts,
an @nt{expression} of a limited
type is not permitted unless it is an @nt{aggregate}, a @nt{function_call},
@Chg{Version=[3],New=[],Old=[or ]}a parenthesized @nt{expression} or
@nt{qualified_expression} whose operand
is permitted by this rule@Chg{Version=[3],New=[, or a @nt{conditional_expression}
all of whose @Syni{dependent_}@nt{expression}s are permitted by this
rule],Old=[]}:]}
@begin{Itemize}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[the initialization @nt{expression} of an
@nt{object_declaration} (see @RefSecNum{Object Declarations})],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[the @nt{default_expression} of a
@nt{component_declaration} (see @RefSecNum{Record Types})],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[the @nt{expression} of a @nt{record_component_association} (see @RefSecNum{Record Aggregates})],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[the @nt{expression} for an @nt{ancestor_part} of an
@nt{extension_aggregate} (see @RefSecNum{Extension Aggregates})],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[an @nt{expression} of a @nt{positional_array_aggregate}
or the @nt{expression} of an @nt{array_component_association} (see
@RefSecNum{Array Aggregates})],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[the @nt{qualified_expression} of an initialized allocator
(see @RefSecNum{Allocators})],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[the @nt{expression} of a return statement (see
@RefSecNum{Return Statements})],Old=[]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0177-1]}
@Chg{Version=[3],New=[the @nt{expression} of an
@nt{expression_function_declaration} (see @RefSecNum{Expression Functions})],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@ChgNote{Only because the paragraph number has changed}
@Chg{Version=[2],New=[the @nt{default_expression} or actual parameter for a
formal object of mode @b{in} (see @RefSecNum{Formal Objects})],Old=[]}

@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[All of these contexts normally require copying; by
restricting the uses as above, we can require the new object to be
built-in-place.]}
@end{Discussion}
@end{Itemize}


@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00419-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0178-1]}
@leading@keepnext@Defn{limited type}
A @Chg{Version=[3],New=[view of a ],Old=[]}type is @i{limited} if it
is @Chg{Version=[2],New=[],Old=[a descendant of ]}one of the following:
@begin(itemize)
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00411-01],ARef=[AI95-00419-01]}
  a type with the reserved word @key(limited)@Chg{Version=[2],New=[,
  @key(synchronized), @key(task), or @key(protected) ],Old=[]}
  in its definition;
  @begin{Ramification}
  Note that there is always a @lquotes@;definition,@rquotes@; conceptually,
  even if there is no syntactic category called @lquotes@;..._definition@rquotes@;.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[This includes interfaces of the above kinds,
  derived types with the reserved word @key{limited}, as well as task and
  protected types.]}
  @end{Ramification}

  @ChgNote{This should really be deleted in Version 2, but we want to
  reuse the paragraph number, and that is hard, so we add the message manually.}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00419-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0087-1]}
  @Chg{Version=[3],New=[a class-wide type whose specific type is limited;],
  Old=[@Chg{Version=[2],New=[@Shrink{@i<This paragraph was deleted.>}],
  Old=[a task or protected type;]}]}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00419-01]}
  a composite type with a limited component@Chg{Version=[2],New=[;],Old=[.]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0178-1]}
  @ChgAdded{Version=[3],Text=[an incomplete view;]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[a derived type whose parent is limited and is not an
  interface.]}

  @begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[Limitedness is not inherited from interfaces;
  it must be explicitly specified when the parent is an interface.]}
  @end{Ramification}

@end(itemize)

@begin{Honest}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[A derived type can become nonlimited if
  @key{limited} does not appear and the derivation
  takes place in the visible part of a child package,
  and the parent type is nonlimited as viewed from the
  private part or body of the child package.]}
@end{Honest}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[We considered a rule where limitedness was always
  inherited from the parent for derived types, but in the case of a type whose
  parent is an interface, this meant that the first interface is treated
  differently than other interfaces. It also would have forced users to declare
  dummy nonlimited interfaces just to get the limitedness right. We also
  considered a syntax like @key{not limited} to specify nonlimitedness when the
  parent was limited, but that was unsavory. The rule given is more uniform and
  simpler to understand.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[The rules for interfaces are asymmetrical, but
  the language is not: if the parent interface is limited, the presence of the
  word @key{limited} determines the limitedness, and nonlimited progenitors are
  illegal by the rules in @RefSecNum{Interface Types} if @key{limited} is
  present. If the parent interface
  is nonlimited, the word @key{limited} is illegal by the rules in
  @RefSecNum{Derived Types and Classes}. The net effect is that the order of
  the interfaces doesn't matter.]}
@end{Reason}


@Defn{nonlimited type}
Otherwise, the type is nonlimited.

@Redundant[There are no predefined equality operators for
a limited type.]

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0052-1]}
@ChgAdded{Version=[3],Type=[Leading],Keepnext=[T],Text=[A type is
@i{immutably limited} if it is one of the
following:@Defn{immutably limited}@Defn2{Term=[limited type],Sec=[immutably]}]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[An explicitly limited record type;]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0217-1]}
@ChgAdded{Version=[3],Text=[A record extension with the reserved word
@key[limited];]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[A nonformal limited private type that is
tagged or has at least one access discriminant with a @nt{default_expression};]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The full type in both of these cases must
  necessarily be immutably limited. We need to include private types
  as much as possible so that we aren't unintentionally discouraging the
  use of private types.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[A task type, a protected type, or a
synchronized interface;]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[A type derived from an immutably limited type.]}

@end{Itemize}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An immutably limited type is a type that cannot
  become nonlimited subsequently in a private part or in a child unit.
  If a view of the type makes it immutably limited, then no copying (assignment)
  operations are ever available for objects of the type. This allows other
  properties; for instance, it is safe for such objects to have
  access discriminants that have defaults or designate other limited objects.]}
@end{Discussion}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[A nonsynchronized limited interface type is
  not immutably limited; a type derived from it can be nonlimited.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0052-1]}
@ChgAdded{Version=[3],Text=[A descendant of a generic formal
limited private type is
presumed to be immutably limited except within the body
of a generic unit or a body declared within the declarative
region of a generic unit, if the formal type is declared
within the formal part of the generic unit.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[In an instance, a type is descended from
  the actual type corresponding to the formal, and all rules are rechecked
  in the specification. Bodies are excepted so that we assume the worst there;
  the complex wording is required to handle children of generics and
  unrelated bodies properly.]}
@end{Ramification}

@end{StaticSem}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00287-01],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0067-1]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[2],New=[
For an @nt{aggregate} of a limited type used to initialize an object as allowed
above, the implementation shall not create a separate anonymous object for the
@nt{aggregate}. For a @nt{function_call} of a type with a part that is of a
task, protected, or explicitly limited record type that is used to initialize
an object as allowed above, the implementation shall not create a separate
return object (see 6.5) for the @nt{function_call}. The @nt{aggregate} or
@nt{function_call} shall be constructed directly in the new object.],Old=[]}]}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[DeletedNoDelMsg],ARef=[AI05-0067-1]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[2],New=[For a
@nt{function_call}, we only require @i{build-in-place}@PDefn{build-in-place}
for a limited type that would have
been a return-by-reference type in Ada 95. We do this because
we want to minimize disruption to Ada 95 implementations and users.],Old=[]}]}
@end{Discussion}
@end{ImplReq}

@begin{Notes}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0067-1]}
@Chg{Version=[2],New=[While it is allowed to write initializations of limited
objects, such initializations never copy a limited object. The source of such an
assignment operation must be an @nt<aggregate> or @nt<function_call>, and such
@nt<aggregate>s and @nt<function_call>s must be built directly in the target
object@Chg{Version=[3],New=[ (see @RefSecNum{Assignment and Finalization})],Old=[]}.],
Old=[@leading@keepnext@;The following are consequences of the rules for limited types:]}
@begin{Honest}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This isn't quite true if the type can become
nonlimited (see below); @nt{function_call}s only are required to be
build-in-place for @lquotes@;really@rquotes@; limited types.]}
@end{Honest}

@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 10
through 15 were deleted.>}]}@Comment{This message should be deleted if the
paragraphs are ever renumbered.}
@end{NotIso}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[An initialization expression is not allowed in an
@nt{object_declaration} if the type of the object is limited.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[A default expression is not allowed in a
@nt{component_declaration} if the type of the record component is limited.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[An initialized allocator is not allowed if the
designated type is limited.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[A generic formal parameter of mode @key[in] must
not be of a limited type.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[@nt{Aggregate}s are not available for a limited
composite type. Concatenation is not available for a limited array type.]}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[The rules do not exclude a @nt{default_expression}
for a formal parameter of a limited type; they do not exclude a deferred
constant of a limited type if the full declaration of the constant is of a
nonlimited type.]}

@Defn{become nonlimited}
@Defn2{Term=[nonlimited type],Sec=(becoming nonlimited)}
@Defn2{Term=[limited type],Sec=(becoming nonlimited)}
As illustrated in @RefSecNum{Private Operations},
an untagged limited type can become nonlimited under certain
circumstances.
@begin{Ramification}
  Limited private types do not become nonlimited;
  instead, their full view can be nonlimited,
  which has a similar effect.

  It is important to remember
  that a single nonprivate type can be both limited and nonlimited
  in different parts of its scope. In other words, @lquotes@;limited@rquotes@; is a property
  that depends on where you are in the scope of the type.
  We don't call this a @lquotes@;view property@rquotes@; because there is no particular
  declaration to declare the nonlimited view.

  Tagged types never become nonlimited.
@end{Ramification}
@end{Notes}

@begin{Examples}
@leading@keepnext@i{Example of a package with a limited type:}
@begin{Example}
@key[package] IO_Package @key[is]
   @key[type] File_Name @key[is] @key[limited] @key[private];

   @key[procedure] Open (F : @key[in] @key[out] File_Name);
   @key[procedure] Close(F : @key[in] @key[out] File_Name);
   @key[procedure] Read (F : @key[in] File_Name; Item : @key[out] Integer);
   @key[procedure] Write(F : @key[in] File_Name; Item : @key[in]  Integer);
@key[private]
   @key[type] File_Name @key[is]
      @key[limited] @key[record]
         Internal_Name : Integer := 0;
      @key[end] @key[record];
@key[end] IO_Package;


@key[package] @key[body] IO_Package @key[is]
   Limit : @key[constant] := 200;
   @key[type] File_Descriptor @key[is] @key[record]  ...  @key[end] @key[record];
   Directory : @key[array] (1 .. Limit) @key[of] File_Descriptor;
   ...
   @key[procedure] Open (F : @key[in] @key[out] File_Name) @key[is]  ...  @key[end];
   @key[procedure] Close(F : @key[in] @key[out] File_Name) @key[is]  ...  @key[end];
   @key[procedure] Read (F : @key[in] File_Name; Item : @key[out] Integer) @key[is] ... @key[end];
   @key[procedure] Write(F : @key[in] File_Name; Item : @key[in]  Integer) @key[is] ... @key[end];
@key[begin]
   ...
@key[end] IO_Package;
@end{Example}
@end{Examples}

@begin{Notes}
@i{Notes on the example:}
In the example above, an outside subprogram making use of IO_Package
may obtain a file name by calling Open and later use it in calls to
Read and Write. Thus, outside the package, a file name obtained from
Open acts as a kind of password; its internal properties (such as
containing a numeric value) are not known and no other operations
(such as addition or comparison of internal names) can be performed
on a file name.
Most importantly, clients of the package cannot make copies
of objects of type File_Name.

@NoPrefix@;This example is characteristic of any case where complete control
over the operations of a type is desired. Such packages serve a dual
purpose. They prevent a user from making use of the internal
structure of the type. They also implement the notion of an
encapsulated data type where the only operations on the type are
those given in the package specification.

@NoPrefix@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
The fact that the full view of File_Name is explicitly declared
@key[limited] means that parameter passing @Chg{Version=[2],New=[],
Old=[and function return ]}will always be by reference@Chg{Version=[2],New=[
and function results will always be built directly in the result object],Old=[]}
(see @RefSecNum{Formal Parameter Modes} and @RefSecNum{Return Statements}).

@end{Notes}

@begin{Extend83}
@Defn{extensions to Ada 83}
The restrictions in RM83-7.4.4(4),
which disallowed @key[out] parameters of limited types in certain
cases, are removed.
@end{Extend83}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Since limitedness and privateness are orthogonal in Ada 95 (and
to some extent in Ada 83), this is now its own @Chg{Version=[3],New=[subclause],Old=[clause]}
rather than being a subclause of
@RefSec{Private Types and Private Extensions}.
@end{DiffWord83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01],ARef=[AI95-00318-02]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  Limited types now have an assignment operation, but its use is restricted
  such that all uses are build-in-place. This is
  accomplished by restricting uses to @nt{aggregate}s and @nt{function_call}s.
  @nt{Aggregate}s were not allowed to have a limited type in Ada 95, which
  causes a compatibility issue discussed in @RefSec{Aggregates}.
  Compatibility issues with return statements for limited
  @nt{function_call}s are discussed
  in @RefSec{Return Statements}.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00411-01],ARef=[AI95-00419-01]}
  @ChgAdded{Version=[2],Text=[Rewrote the definition of limited to ensure that
  interfaces are covered, but that limitedness is not inherited from interfaces.
  Derived types that explicitly include @key{limited} are now also covered.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0052-1],ARef=[AI05-0217-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a definition for
  immutably limited types, so that the fairly complex definition does
  not need to be repeated in rules elsewhere in the Standard.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1],ARef=[AI05-0299-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> The built-in-place rules
  are consolidated in @RefSecNum{Assignment and Finalization}, and thus
  they are removed from this subclause.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0087-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Fixed an oversight: class-wide
  types were never defined to be limited, even if their associated specific
  type is. It is thought that this oversight was never implemented incorrectly
  by any compiler, thus we have not classified it as an incompatibility.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1]}
  @ChgAdded{Version=[3],Text=[Allowed @nt{conditional_expression}s in limited
  constructor contexts @em we want to treat these as closely to parentheses as
  possible.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0178-1]}
  @ChgAdded{Version=[3],Text=[Added wording so that expression functions can
  return limited entities.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0178-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added incomplete views
  to the list of reasons for a view of a type to be limited. This is not
  a change as the definition already was in
  @RefSecNum{Incomplete Type Declarations}. But it is much better to have
  all of the reasons for limitedness together.]}
@end{DiffWord2005}


@RMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledRevisedClause{Version=[3],New=[Assignment and Finalization],Old=[User-Defined Assignment and Finalization]}

@begin{Intro}
@redundant[@Defn{user-defined assignment}
@Defn2{Term=[assignment], Sec=(user-defined)}
Three kinds of actions are
fundamental to the manipulation of objects:
initialization, finalization, and assignment.
Every object is initialized, either explicitly
or by default, after being created
(for example, by an @nt{object_declaration} or @nt{allocator}).
Every object is finalized before being destroyed
(for example, by leaving a @nt{subprogram_body} containing an
@nt{object_declaration}, or by a call to an instance of
Unchecked_Deallocation).
An assignment operation is used as part of @nt{assignment_statement}s,
explicit initialization, parameter passing, and other operations.
@IndexSee{Term=[constructor],See=[initialization]}
@IndexSee{Term=[constructor],See=[Initialize]}
@IndexSee{Term=[destructor],See=[finalization]}

Default definitions for these three fundamental
operations are provided by the
language, but
@Defn{controlled type}
a @i{controlled} type gives the user additional
control over parts of these operations.
@Defn{Initialize}@Defn{Finalize}@Defn{Adjust}
In particular, the user can define, for a controlled type,
an Initialize procedure which is invoked immediately after
the normal default initialization of a controlled object, a Finalize procedure
which is invoked immediately before finalization of any of the
components of a controlled object, and an Adjust
procedure which is invoked as the last step of an assignment to
a (nonlimited) controlled object.]
@ToGlossary{Term=<Controlled type>,
  Text=<A controlled type supports user-defined assignment and
  finalization.
  Objects are always finalized before being destroyed.>}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00114-01],ARef=[AI95-00287-01]}
Here's the basic idea of initialization, value adjustment, and finalization,
whether or not user defined:
When an object is created,
if it is explicitly assigned an initial value,
@Chg{Version=[2],New=[the object is either built-in-place from an @nt{aggregate}
or function call (in which case neither Adjust nor Initialize is applied), or ],Old=[]}
the assignment copies and
adjusts the initial value. Otherwise, Initialize is applied to it
(except in the case of an @nt{aggregate} as a whole).
An @nt{assignment_statement} finalizes the target before
copying in and adjusting the new value.
Whenever an object goes away, it is finalized.
Calls on Initialize and Adjust happen bottom-up; that is,
components first, followed by the containing object.
Calls on Finalize @Chg{Version=[2],New=[happen],Old=[happens]} top-down; that is,
first the containing object, and then its components.
These ordering rules ensure that any components will be in a
well-defined state when Initialize, Adjust,
or Finalize is applied to the containing object.
@end{Ramification}
@end{Intro}

@begin{StaticSem}
@leading@keepnext@;The following language-defined library package exists:
@begin{Example}@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0020],ARef=[AI95-00126-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0212-1]}
@key[package] Ada.Finalization @key[is]@ChildUnit{Parent=[Ada],Child=[Finalization]}
    @key[pragma] @Chg{Version=[3],New=[Pure],Old=[Preelaborate]}(Finalization);@Chg{Version=[3],New=[],Old=[@Chg{New=[
    @key[pragma] Remote_Types(Finalization);],Old=[]}]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
    @key[type] @AdaTypeDefn{Controlled} @key[is abstract tagged private];@Chg{Version=[2],New=[
    @key{pragma} Preelaborable_Initialization(Controlled);],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00348-01]}
    @key(procedure) @AdaSubDefn{Initialize} (Object : @key(in out) Controlled)@Chg{Version=[2],New=[ @key{is null}],Old=[]};
    @key(procedure) @AdaSubDefn{Adjust}     (Object : @key(in out) Controlled)@Chg{Version=[2],New=[ @key{is null}],Old=[]};
    @key(procedure) @AdaSubDefn{Finalize}   (Object : @key(in out) Controlled)@Chg{Version=[2],New=[ @key{is null}],Old=[]};

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00161-01]}
    @key[type] @AdaTypeDefn{Limited_Controlled} @key[is abstract tagged limited private];@Chg{Version=[2],New=[
    @key{pragma} Preelaborable_Initialization(Limited_Controlled);],Old=[]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00348-01]}
    @key(procedure) @AdaSubDefn{Initialize} (Object : @key(in out) Limited_Controlled)@Chg{Version=[2],New=[ @key{is null}],Old=[]};
    @key(procedure) @AdaSubDefn{Finalize}   (Object : @key(in out) Limited_Controlled)@Chg{Version=[2],New=[ @key{is null}],Old=[]};
@key(private)
    ... -- @RI{not specified by the language}
@key[end] Ada.Finalization;
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00348-01]}
@Defn{controlled type}
A controlled type is a descendant of Controlled or Limited_Controlled.
@Chg{Version=[2],New=[],Old=[The (default) implementations of
Initialize, Adjust, and Finalize have no effect. ]}The predefined
"=" operator of type Controlled always returns True,
@Redundant[since this operator is incorporated into
the implementation of the predefined equality operator of types derived
from Controlled, as explained in
@RefSecNum(Relational Operators and Membership Tests).]
The type Limited_Controlled is like Controlled, except
that it is limited and it lacks the primitive subprogram Adjust.
@begin{Discussion}
We say @lquotes@;nonlimited controlled type@rquotes@ (rather than just
@lquotes@;controlled type@rquotes@;;) when we want to talk about descendants
of Controlled only.
@end{Discussion}
@begin{Reason}
  We considered making Adjust and Finalize abstract.
  However, a reasonable coding convention is e.g. for Finalize to
  always call the parent's Finalize after doing whatever work is needed
  for the extension part.
  (Unlike CLOS, we have no way to do that automatically in Ada 95.)
  For this to work, Finalize cannot be abstract.
  In a generic unit, for a generic formal abstract derived type whose
  ancestor is Controlled or Limited_Controlled, calling the ancestor's
  Finalize would be illegal if it were abstract, even though the actual
  type might have a concrete version.

  Types Controlled and Limited_Controlled are abstract, even though
  they have no abstract primitive subprograms.
  It is not clear that they need to be abstract, but there seems to be
  no harm in it, and it might make an implementation's life easier to
  know that there are no objects of these types @em in case the
  implementation wishes to make them @lquotes@;magic@rquotes@; in some way.

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[For Ada 2005, we considered making these types
  interfaces. That would have the advantage of allowing them to be added
  to existing trees. But that was rejected both because it would cause
  massive disruptions to existing implementations, and because it would be
  very incompatible due to the "no hidden interfaces" rule. The latter rule
  would prevent a tagged private type from being completed with a derivation
  from Controlled or Limited_Controlled @em a very common idiom.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00360-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[A type is said to
@i{need finalization} if:@Defn{needs finalization}@Defn2{Term=[type],Sec=[needs finalization]}]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[it is a controlled type, a task type or
a protected type; or]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0092-1]}
@ChgAdded{Version=[2],Text=[it has a component
@Chg{Version=[3],New=[whose type ],Old=[that ]} needs finalization; or]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0013-1]}
@Chg{Version=[3],New=[it is a class-wide type; or],
Old=[@Chg{Version=[2],New=[it is a limited type that
has an access discriminant whose designated type needs finalization; or],Old=[]}]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0026-1]}
@ChgAdded{Version=[3],Text=[it is a partial view whose full view needs finalization; or]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[it is one of a number of language-defined types
that are explicitly defined to need finalization.]}

@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The fact that a type needs finalization
  does not require it to be implemented with a controlled type. It just has to
  be recognized by the No_Nested_Finalization restriction.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This property is defined for the type, not
  for a particular view. That's necessary as restrictions look in private parts
  to enforce their restrictions; the point is to eliminate all controlled
  parts, not just ones that are visible.]}
@end{Ramification}

@end{Itemize}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
@Chg{Version=[2],New=[],Old=[@PDefn2{Term=[elaboration],
Sec=(object_declaration)}]}During
the elaboration @Chg{Version=[2],New=[or evaluation of a construct that
causes an object to be initialized by default],Old=[of
an @nt{object_declaration}]},
for every controlled subcomponent of
the object that is not assigned an initial value
(as defined in @RefSecNum{Object Declarations}),
Initialize is called on that subcomponent.
Similarly, if the object @Chg{Version=[2],New=[that is initialized by
default ],Old=[]}as a whole is controlled@Chg{Version=[2],New=[],Old=[ and
is not assigned an initial value]}, Initialize is called on the object.@Chg{Version=[2],
New=[],Old=[ The same applies to the evaluation of an @nt{allocator},
as explained in @RefSecNum{Allocators}.]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0021],ARef=[AI95-00182-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
For an @nt{extension_aggregate} whose @nt{ancestor_part} is a
@nt{subtype_mark}@Chg{Version=[2],New=[ denoting a],
Old=[, @Chg{New=[for each controlled subcomponent of the ancestor part, either
Initialize is called, or its initial value is assigned, as appropriate],
Old=[Initialize is called on all controlled subcomponents of the
ancestor part]}; if the type of the ancestor part is itself]}
controlled@Chg{Version=[2],New=[ subtype],Old=[]},
the Initialize procedure of the ancestor type is called,
unless that Initialize procedure is abstract.
@begin{Discussion}
@leading@keepnext@;Example:
@begin{Example}
@key[type] T1 @key[is] @key[new] Controlled @key[with]
    @key[record]
        ... --@RI{ some components might have defaults}
    @key[end] @key[record];

@key[type] T2 @key[is] @key[new] Controlled @key[with]
    @key[record]
        X : T1; --@RI{ no default}
        Y : T1 := ...; --@RI{ default}
    @key[end] @key[record];

A : T2;
B : T2 := ...;
@end{Example}

As part of the elaboration of A's declaration, A.Y is assigned a
value; therefore Initialize is not applied to A.Y.
Instead, Adjust is applied to A.Y as part of the assignment operation.
Initialize is applied to A.X and to A, since those objects are not
assigned an initial value.
The assignment to A.Y is not considered an assignment to A.

For the elaboration of B's declaration, Initialize is not called at
all. Instead the assignment adjusts B's value;
that is, it applies Adjust to B.X, B.Y, and B.

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0021],ARef=[AI95-00182-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00373-01]}
@ChgAdded{Version=[1],Text=[The @nt{ancestor_part} of an
@nt{extension_aggregate}@Chg{Version=[2],New=[, <> in aggregates, and the
return object of an @nt{extended_return_statement} are],Old=[ is]} handled
similarly.]}
@end{Discussion}

Initialize and other initialization
operations are done in an arbitrary order,
except as follows.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
Initialize is applied to an object after initialization
of its subcomponents, if any
@Redundant[(including both implicit initialization and Initialize calls)].
If an object has a component with an access discriminant
constrained by a per-object expression,
Initialize is applied to this component after any components that do not
have such discriminants.
For an object with several components with such a discriminant,
Initialize is applied to them in order
of their @nt{component_declaration}s.
For an @nt<allocator>,
any task activations follow all calls
on Initialize.
@begin{Reason}
  The fact that Initialize is done for subcomponents
  first allows Initialize for a composite
  object to refer to its subcomponents knowing
  they have been properly initialized.

  The fact that Initialize is done for components with access
  discriminants after other components
  allows the Initialize operation for a component
  with a self-referential access discriminant to assume
  that other components of the enclosing object have already been
  properly initialized.
  For multiple such components, it allows some predictability.
@end{Reason}

@leading@keepnext@Defn{assignment operation}
When a target object with any controlled parts is assigned a value,
@Redundant[either when created or in a subsequent
@nt{assignment_statement},]
the @i{assignment operation} proceeds as follows:
@begin(itemize)
  The value of the target becomes the assigned value.

  @Defn{adjusting the value of an object}
  @Defn{adjustment}
  The value of the target is @i{adjusted.}
@end(itemize)
@begin{Ramification}
    If any parts of the object are controlled,
    abort is deferred during the assignment operation.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0067-1]}@Comment{May need future changes.}
@Defn{adjusting the value of an object}
@Defn{adjustment}
To adjust the value of a
@Chg{Version=[3],New=[],Old=[@Redundant[(nonlimited)] ]}composite object,
the values of the components of the object are first
adjusted in an arbitrary order,
and then, if the object is
@Chg{Version=[3],New=[nonlimited ],Old=[]}controlled,
Adjust is called.@PDefn2{Term=[arbitrary order],Sec=[allowed]}
Adjusting the value of an elementary object has no effect@Redundant[,
nor does adjusting the value of a composite object with no
controlled parts.]
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0067-1]}
  Adjustment is never @Chg{Version=[3],New=[actually ],Old=[]}performed for
  values of @Chg{Version=[3],New=[an immutably],Old=[a by-reference]} limited type,
  since @Chg{Version=[3],New=[all
  assignment operations for such types are required to be built-in-place.
  Even so, we still define adjustment for all types in order that the
  canonical semantics is well-defined],Old=[these
  types do not support copying]}.
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
  The verbiage in the Initialize rule about access discriminants
  constrained by per-object expressions is not necessary here,
  since such types are
  @Chg{Version=[3],New=[either ],Old=[]}limited@Chg{Version=[3],New=[ or
  do not have defaults, so the discriminant can only be changed by
  an assignment to an outer object. Such an assignment could
  happen only before any adjustments or (if part of an outer Adjust)
  only after any inner (component) adjustments have completed.],
  Old=[, and therefore are never adjusted.]}
@end{Reason}

@PDefn2{Term=[execution], Sec=(assignment_statement)}
For an @nt{assignment_statement},
@Redundant[ after the @nt{name} and
@nt{expression} have been evaluated,
and any conversion (including constraint checking) has been done,]
an anonymous object is created, and
the value is assigned into it;
@Redundant[that is, the assignment operation is applied].
@Redundant[(Assignment includes value adjustment.)]
The target of the @nt{assignment_statement} is then finalized.
The value of the anonymous object is then assigned into the target of
the @nt{assignment_statement}.
Finally, the anonymous object is finalized.
@Redundant[As explained below,
the implementation may eliminate the intermediate anonymous object,
so this description subsumes the one given in
@RefSec{Assignment Statements}.]
@begin{Reason}
@leading@;An alternative design for user-defined assignment might involve an
Assign operation instead of Adjust:
@begin{Example}
@key[procedure] Assign(Target : @key[in] @key[out] Controlled; Source : @key[in] @key[out] Controlled);
@end{Example}

@leading@keepnext@;Or perhaps even a syntax like this:
@begin{Example}
@key[procedure] ":="(Target : @key[in] @key[out] Controlled; Source : @key[in] @key[out] Controlled);
@end{Example}

@leading@;Assign (or ":=") would have the responsibility of doing the copy,
as well as whatever else is necessary.
This would have the advantage that the Assign operation knows about both
the target and the source at the same time @em it would be possible to
do things like reuse storage belonging to the target, for example,
which Adjust cannot do.
However, this sort of design would not work in the case of unconstrained
discriminated variables, because there is no way to change the
discriminants individually.
For example:
@begin{Example}
@key[type] Mutable(D : Integer := 0) @key[is]
    @key[record]
        X : Array_Of_Controlled_Things(1..D);
        @key[case] D @key[is]
            @key[when] 17 => Y : Controlled_Thing;
            @key[when] @key[others] => @key[null];
        @key[end] D;
    @key[end] @key[record];
@end{Example}

An assignment to an unconstrained variable of type Mutable can cause
some of the components of X, and the component Y, to appear and/or
disappear.
There is no way to write the Assign operation to handle this sort of
case.

Forbidding such cases is not an option @em it would cause generic
contract model violations.
@end{Reason}


@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0067-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[When a function call
or @nt{aggregate} is used to initialize an object, the
result of the function call or @nt{aggregate} is an anonymous object, which
is assigned into the newly-created object. For such an assignment,
the anonymous object might be @i<built in place>,@Defn{built in place}@Seeother{Primary=[build-in-place],Other=[built in place]}
in which case the assignment does not involve any copying.
Under certain circumstances, the anonymous object is required to be built in
place. In particular:]}

@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1]}
  @ChgAdded{Version=[3],Text=[We say assignment to built-in-place objects does not
  involve copying, which matches the intended implementation (see below). Of
  course, the implementation can do any copying it likes, if it can make such
  copying semantically invisible (by patching up access values to point to the
  copy, and so forth).]}
@end{Discussion}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[If the full type of any part of the object is
  immutably limited, the anonymous object is built in place.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1]}
  @ChgAdded{Version=[3],Text=[We talk about the full types being immutably
  limited, as this is independent of the view of a type (in the same way that it
  is for determining the technique of parameter passing). That is, privacy is
  ignored for this purpose.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0067-1]}
  @ChgAdded{Version=[3],Text=[For function calls, we only require building in
  place for immutably limited types. These are the types that would have been
  return-by-reference types in Ada 95. We limited the requirement because
  we want to minimize disruption to Ada 95 implementations and users.]}
@end{Reason}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0232-1]}
  @ChgAdded{Version=[3],Text=[This is a dynamic property and is determined by
  the specific type of the parts of the actual object. In particular, if a part
  has a class-wide type, the tag of the object might need to be examined in
  order to determine if build-in-place is required. However, we expect that most
  Ada implementations will determine this property at compile-time using some
  assume-the-worst algorithm in order to chose the appropriate method to
  implement a given call or aggregate. In addition, there is no attribute or
  other method for a program to determine if a particular object has this
  property (or not), so there is no value to a more careful description of this
  rule.]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[In the case of an @nt{aggregate}, if the full type
  of any part of the newly-created object is controlled, the anonymous object is
  built in place.]}

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[This is necessary to prevent
elaboration problems with deferred constants of controlled types. Consider:]}
@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[package] P @key[is]
   @key[type] Dyn_String @key[is private];
   Null_String : @key[constant] Dyn_String;
   ...
@key[private]
   @key[type] Dyn_String @key[is new] Ada.Finalization.Controlled @key[with] ...
   @key[procedure] Finalize(X : @key[in out] Dyn_String);
   @key[procedure] Adjust(X : @key[in out] Dyn_String);
@comment{Blank Line}
   Null_String : @key[constant] Dyn_String :=
      (Ada.Finalization.Controlled @key[with] ...);
   ...
@key[end] P;]}
@end{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[When Null_String is elaborated, the bodies of Finalize and Adjust
clearly have not been elaborated. Without this rule, this declaration would
necessarily raise Program_Error (unless the permissions given below are
used by the implementation).]}
@end{Reason}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An @nt{aggregate} with a controlled part
  used in the return expression
  of a @nt{simple_@!return_@!statement} has to be built in place in the anonymous
  return object, as this is similar to an object declaration. (This is a change
  from Ada 95, but it is not an inconsistency as it only serves to restrict
  implementation choices.) But this only covers the @nt{aggregate}; a separate
  anonymous return object can still be used unless it too is required to be
  built in place.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Similarly, an @nt{aggregate} that has
  a controlled part but is not itself controlled and that is used to
  initialize an object also has to be built in place. This is also a change
  from Ada 95, but it is not an inconsistency as it only serves to restrict
  implementation choices. This avoids problems if a type like Dyn_String
  (in the example above) is used as a component in a type used as a
  deferred constant in package P.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added]}
  @ChgAdded{Version=[3],Text=[In other cases, it is unspecified whether the
  anonymous object is built in place.@PDefn{unspecified}]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This is left unspecified so the implementation
  can use any appropriate criteria for determining when to build in place.
  That includes making the decision on a call-by-call basis. Reasonable
  programs will not care what decision is made here anyway.]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0067-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Notwithstanding@Defn{notwithstanding}
what this International Standard says elsewhere, if an object is
built in place:]}

@begin{Itemize}
    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[Upon successful completion of the return
    statement or @nt{aggregate}, the anonymous object @i<mutates into> the
    newly-created object; that is, the anonymous object ceases to exist, and the
    newly-created object appears in its place.@Defn{mutates}]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[Finalization is not performed on the anonymous object.]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[Adjustment is not performed on the newly-created
    object.]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[All access values that designate parts of the
    anonymous object now designate the corresponding parts of the newly-created
    object.]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[All renamings of parts of the anonymous
    object now denote views of the corresponding parts of the newly-created
    object.]}

    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[Coextensions of the anonymous object become
    coextensions of the newly-created object.]}
@end{Itemize}

@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This @ldquote@;mutating@rdquote does not
  necessarily happen atomically with
  respect to abort and other tasks. For example, if a function call is used as
  the parent part of an @nt{extension_aggregate}, then the tag of the anonymous
  object (the function result) will be different from the tag of the
  newly-created object (the parent part of the @nt{extension_aggregate}). In
  implementation terms, this involves modifying the tag field. If the current
  task is aborted during this modification, the object might become abnormal.
  Likewise, if some other task accesses the tag field during this modification,
  it constitutes improper use of shared variables, and is erroneous.]}
@end{Honest}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The intended implementation is that the
  anonymous object is allocated at the
  same address as the newly-created object. Thus, no run-time action is
  required to cause all the access values and renamings to point to the right
  place. They just point to the newly-created object, which is what the return
  object has magically @ldquote@;mutated into@rdquote.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[There is no requirement that 'Address of the
  return object is equal to 'Address of the newly-created object, but that will
  be true in the intended implementation.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[For a function call, if the size of the
  newly-created object is known at the call site, the object is allocated there,
  and the address is implicitly passed to the function; the return object is
  created at that address. Otherwise, a storage pool is implicitly passed to the
  function; the size is determined at the point of the return statement, and
  passed to the Allocate procedure. The address returned by the storage pool is
  returned from the function, and the newly-created object uses that same
  address. If the return statement is left without returning (via an exception
  or a goto, for example), then Deallocate is called.  The storage pool might be
  a dummy pool that represents @ldquote@;allocate on the stack@rdquote.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[The Tag of the newly-created object may be
  different from that of the result object. Likewise, the master and
  accessibility level may be different.]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[An alternative implementation model might allow
  objects to move around to different addresses. In this case, access values and
  renamings would need to be modified at run time. It seems that this model
  requires the full power of tracing garbage collection.]}
@end{ImplNote}
@end{RunTime}

@begin{ImplReq}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0022],ARef=[AI95-00083-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0067-1]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[1],New=[For an @nt{aggregate} of a
controlled type whose value is assigned,
other than by an @nt{assignment_statement}@Chg{Version=[2],New=[],Old=[ or
a @nt{return_statement}]}, the
implementation shall not create a separate anonymous object for the
@nt{aggregate}. The @nt{aggregate} value shall be constructed directly in the target
of the assignment operation and Adjust is not called on the target object.],Old=[]}]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00318-02]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg],ARef=[AI05-0067-1]}
@ChgDeleted{Version=[3],Type=[Leading],Text=[@Chg{Version=[1],New=[@Chg{Version=[2],
New=[@PDefn{build-in-place}],Old=[]}This@Chg{Version=[2],
New=[ @i<build-in-place> requirement],Old=[]} is necessary to prevent
elaboration problems with deferred constants of controlled types. Consider:],Old=[]}]}
@begin{Example}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@Chg{New=[@key[package] P @key[is]
   @key[type] Dyn_String @key[is private];
   Null_String : @key[constant] Dyn_String;
   ...
@key[private]
   @key[type] Dyn_String @key[is new] Ada.Finalization.Controlled @key[with] ...
   @key[procedure] Finalize(X : @key[in out] Dyn_String);
   @key[procedure] Adjust(X : @key[in out] Dyn_String);
@comment{Blank Line}
   Null_String : @key[constant] Dyn_String :=
      (Ada.Finalization.Controlled @key[with] ...);
   ...
@key[end] P;],Old=[]}]}
@end{Example}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@Chg{New=[When Null_String is elaborated, the bodies of Finalize and Adjust
clearly have not been elaborated. Without this rule, this declaration would
necessarily raise Program_Error (unless the permissions given below are
used by the implementation).],Old=[]}]}
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgRef{Version=[3],Kind=[DeletedAddedNoDelMsg]}
@ChgDeleted{Version=[3],Text=[@Chg{Version=[2],New=[An @nt{aggregate} of a controlled type
used in the return expression
of a @nt{simple_@!return_@!statement} has to be built-in-place in the anonymous
return object, as this is similar to an object declaration. (This is a change
from Ada 95, but it is not an inconsistency as it only serves to restrict
implementation choices.) But this only covers the @nt{aggregate}; a separate
anonymous return object can still be used unless it too is required to be
built-in-place (see @RefSecNum{Limited Types}).],Old=[]}]}
@end{Ramification}
@end{ImplReq}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0067-1]}
An implementation is allowed to relax the above rules
@Chg{Version=[3],New=[for @nt{assignment_statement}s],
Old=[@Redundant[(for nonlimited controlled types)]]}
in the following ways:
@begin{TheProof}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0067-1]}
@ChgDeleted{Version=[3],Text=[The phrase @lquotes@;for nonlimited controlled
types@rquotes@; follows from the fact
that all of the following permissions apply to cases involving
assignment.
It is important because the programmer can count on a stricter semantics
for limited controlled types.]}
@end{TheProof}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1]}
@ChgAdded{Version=[3],Text=[The relaxations apply only to nonlimited types,
as @nt{assignment_statement}s
are not allowed for limited types. This is important so that the programmer
can count on a stricter semantics for limited controlled types.]}
@end{Ramification}
@begin{Itemize}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0067-1]}
@Chg{Version=[3],New=[If],Old=[For an @nt{assignment_statement} that assigns to]}
an object @Chg{Version=[3],New=[is assigned ],Old=[]}the value of that same
object, the implementation need not do anything.
@begin{Ramification}
  In other words, even if an object is controlled and a combination
  of Finalize and Adjust on the object might have a net
  side effect, they need not be performed.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0067-1]}
For @Chg{Version=[3],New=[assignment of],Old=[an @nt{assignment_statement} for]}
a noncontrolled type,
the implementation may finalize and assign each
component of the variable separately (rather than finalizing the entire
variable and assigning the entire new value)
unless a discriminant of the variable is changed by the assignment.
@begin{Reason}
For example, in a slice assignment, an anonymous object is not necessary
if the slice is copied component-by-component in the right direction,
since array types are not controlled (although their components may be).
Note that the direction, and even the fact that it's a slice assignment,
can in general be determined only at run time.
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
@ChgAdded{Version=[3],Text=[This potentially breaks a single assignment
operation into many, and thus abort deferral (see
@RefSecNum{Abort of a Task - Abort of a Sequence of Statements}) needs to
last only across an individual component assignment when the component
has a controlled part. It is only important that the copy step is
not separated (by an abort) from the adjust step, so aborts between
component assignments is not harmful.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00147-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0067-1]}
@Chg{Version=[3],New=[The],Old=[For an @nt{aggregate} or function call whose
value is assigned into a target object,
the implementation need not create a separate anonymous object if
it can safely create the value of
the @nt{aggregate} or function call
directly in the target object.
Similarly, for an @nt{assignment_@!statement}, the]}
implementation need not create an anonymous object if
the value being assigned is the result of evaluating a @nt{name}
denoting an object (the source object) whose storage cannot overlap
with the target. If the source object might overlap with the
target object, then the implementation can avoid the need for
an intermediary anonymous object by exercising one of the
above permissions and perform the assignment one component
at a time (for an overlapping array assignment), or not at all
(for an assignment where the target and the source of the assignment are
the same object).@Chg{Version=[2],New=[],Old=[ Even if an anonymous object is
created, the implementation may move its value to the target object as part of
the assignment without re-adjusting so long as the
anonymous object has no aliased subcomponents.]}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@Chg{Version=[3],New=[If the anonymous object is eliminated by this permission, ],
Old=[In the @nt{aggregate} case, only one
value adjustment is necessary, and]} there is no anonymous object to be
finalized@Chg{Version=[3],New=[ and thus the Finalize call on it is
eliminated],Old=[]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00147-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0005-1]}
@Chg{Version=[2],New=[@Chg{Version=[3],New=[Note that if the anonymous
object is eliminated but the new value is not built in place in the
target object],Old=[Similarly, in the function call case, the
anonymous object can be eliminated. Note, however]}, that Adjust
must be called],Old=[In the @nt{assignment_statement} case as well,
no finalization of the anonymous object is needed.
On the other hand, if the target has aliased subcomponents,
then an adjustment takes place]} directly on the target object
as the last step of the assignment, since some of the
subcomponents may be self-referential or otherwise
position-dependent.@Chg{Version=[2],New=[ This Adjust can be eliminated only
by using one of the following permissions.],Old=[]}
@end{Ramification}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00147-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[Furthermore, an implementation is
permitted to
omit implicit Initialize, Adjust, and Finalize calls and associated assignment
operations on an object of a nonlimited controlled type provided that:]}

@begin{Itemize}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[any omitted Initialize call is not a call on a
user-defined Initialize procedure, and]}
@begin{Honest}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This does not apply to any calls to a user-defined
Initialize routine that happen to occur in an Adjust or Finalize routine. It
is intended that it is never necessary to look inside of an Adjust or Finalize
routine to determine if the call can be omitted.]}
@end{Honest}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[We don't want to eliminate objects for which the
Initialize might have side effects (such as locking a resource).]}
@end{Reason}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[any usage of the value of the object after the implicit
Initialize or Adjust call and before any subsequent Finalize call on the object
does not change the external effect of the program, and]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[after the omission of such calls and operations, any
execution of the program that executes an Initialize or Adjust call on an
object or initializes an object by an @nt{aggregate} will also later execute a
Finalize call on the object and will always do so prior to assigning a new
value to the object, and]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[the assignment operations associated with omitted Adjust
calls are also omitted.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[This permission applies to Adjust and Finalize calls even
if the implicit calls have additional external effects.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[The goal of the above permissions is to allow
typical dead assignment and dead variable removal algorithms to work for
nonlimited controlled types. We require that @lquotes@;pairs@rquotes@; of
Initialize/Adjust/Finalize operations are removed. (These aren't always pairs,
which is why we talk about @lquotes@;any execution of the program@rquotes@;.)]}
@end{Reason}

@end{ImplPerm}

@begin{Extend83}
@Defn{extensions to Ada 83}
Controlled types and user-defined finalization are new to Ada 95.
(Ada 83 had finalization semantics only for masters of tasks.)
@end{Extend83}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00161-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}
  @b[Amendment Correction:] Types Controlled and Limited_Controlled now have
  Preelaborable_Initialization, so that objects of types derived from these
  types can be used in preelaborated packages.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0020],ARef=[AI95-00126-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that Ada.Finalization
  is a remote types package.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0021],ARef=[AI95-00182-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to clarify that
  the default initialization (whatever it is) of an ancestor part is used.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0022],ARef=[AI95-00083-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified that Adjust is never
  called on an @nt{aggregate} used for the initialization of an object or
  subaggregate, or passed as a parameter.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00147-01]}
  @ChgAdded{Version=[2],Text=[Additional optimizations are allowed for
  nonlimited controlled types. These allow traditional dead variable
  elimination to be applied to such types.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00318-02]}
  @ChgAdded{Version=[2],Text=[Corrected the build-in-place requirement
  for controlled @nt{aggregate}s to be consistent with the requirements
  for limited types.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00348-01]}
  @ChgAdded{Version=[2],Text=[The operations of types Controlled and
  Limited_Controlled are now declared as null procedures (see
  @RefSecNum{Null Procedures}) to make the semantics clear (and to provide
  a good example of what null procedures can be used for).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00360-01]}
  @ChgAdded{Version=[2],Text=[Types that need finalization are defined; this is
  used by the No_Nested_Finalization restriction
  (see @RefSec{Tasking Restrictions}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00373-01]}
  @ChgAdded{Version=[2],Text=[Generalized the description of objects that
  have Initialize called for them to say that it is done for all objects that
  are initialized by default. This is needed so that all of the new cases
  are covered.]}
@end{DiffWord95}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0212-1]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}
  Package Ada.Finalization now has Pure categorization, so it can be mentioned
  for any package. Note that this does not change the preelaborability of
  objects descended from Controlled and Limited_Controlled.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Eliminated coextensions
  from the @ldquote@;needs finalization@rdquote rules, as this cannot be
  determined in general in the compilation unit that declares the type.
  (The designated type of the coextension may have been imported as a
  limited view.) Uses of @ldquote@;needs finalization@rdquote need to
  ensure that coextensions are handled by other means (such as
  in No_Nested_Finalization @en see @RefSecNum{Tasking Restrictions})
  or that coextensions cannot happen.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0013-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the
  @ldquote@;needs finalization@rdquote rules to include class-wide types,
  as a future extension can include a part that needs finalization.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0026-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the
  @ldquote@;needs finalization@rdquote rules to clearly say that they
  ignore privacy.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0067-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Changed @ldquote@;built in place@rdquote
  to @RuntimeTitle and centralized the rules here. This eliminates the
  fiction that built in place is just a combination of a permission and
  a requirement; it clearly has noticeable semantic effects. This wording
  change is not intended to change the semantics of any correct Ada program.]}
@end{DiffWord2005}


@LabeledSubClause{Completion and Finalization}

@begin{Intro}
@redundant[This subclause defines @i{completion} and @i{leaving} of the execution
of constructs and entities.
A @i{master} is the execution of a construct that
includes finalization of local objects after it is complete
(and after waiting for any local tasks
@em see @RefSecNum(Task Dependence - Termination of Tasks)),
but before leaving.
Other constructs and entities are left immediately upon completion.
@IndexSee{Term=[cleanup],See=(finalization)}
@IndexSee{Term=[destructor],See=(finalization)}]
@end{Intro}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@Defn{completion and leaving (completed and left)}
@Defn2{Term=[completion], Sec=(run-time concept)}
The execution of a construct or entity
is @i{complete} when the end of that execution has been reached,
or when a transfer of control
(see @RefSecNum{Simple and Compound Statements - Sequences of Statements})
causes it to be abandoned.
@Defn{normal completion}
@Defn2{Term=[completion], Sec=(normal)}
@Defn{abnormal completion}
@Defn2{Term=[completion], Sec=(abnormal)}
Completion due to reaching the end of execution,
or due to the transfer of control of an @Chg{Version=[2],
New=[@nt{exit_statement}, return statement, @nt{goto_statement}],
Old=[@ntf{exit_}, @ntf{return_}, @ntf{goto_}]},
or @nt{requeue_statement} or of the
selection of a @nt{terminate_alternative}
is @i{normal completion}.
Completion is @i{abnormal} otherwise @Redundant[@em
when control is transferred out of a construct
due to abort or the raising of an exception].
@begin{Discussion}
Don't confuse the run-time concept of completion with
the compile-time concept of completion
defined in @RefSecNum{Completions of Declarations}.
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00162-01],ARef=[AI95-00416-01]}
@Defn{leaving}
@Defn{left}
After execution of a construct or entity is complete,
it is @i{left},
meaning that execution continues with the next action,
as defined for the execution that is taking place.
@Defn{master}
Leaving an execution happens immediately after its completion,
except in the case of a @i{master}:
the execution of
a @Chg{Version=[2],New=[body other than a @nt{package_body};
the execution of a @nt{statement};
or the evaluation of an @nt{expression}, @nt{function_call}, or @nt{range} that
is not part of an enclosing @nt{expression}, @nt{function_call}, @nt{range}, or
@nt{simple_@!statement} other than a @nt{simple_@!return_@!statement}],
Old=[@nt{task_body}, a @nt{block_@!statement},
a @nt{subprogram_body}, an @nt{entry_body}, or an @nt{accept_@!statement}]}.
A master is finalized after it is
complete, and before it is left.

@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00162-01],ARef=[AI95-00416-01]}
  @Chg{Version=[2],New=[@nt{Expression}s and @nt{statement}s
  are masters so that objects created by subprogram calls (in @nt{aggregate}s,
  @nt{allocator}s for anonymous access-to-object types, and so on) are
  finalized and have their tasks awaited before the @nt{expression}s or
  @nt{statement}s are left. Note that @nt{expression}s like the @nt{condition}
  of an @nt{if_statement} are masters, because they are not enclosed by a
  @nt{simple_statement}. Similarly, a @nt{function_call} which is renamed
  is a master, as it is not in a @nt{simple_@!statement}.],
  Old=[Note that although an @nt{accept_statement} has no
  @nt{declarative_part}, it can call functions and evaluate @nt{aggregate}s,
  possibly causing anonymous controlled objects to be created,
  and we don't want those objects to escape outside the rendezvous.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[We have to include @nt{function_call}s in the
  contexts that do not cause masters to occur so that @nt{expression}s
  contained in a @nt{function_call} (that is not part of an @nt{expression} or
  @nt{simple_statement}) do not individually become masters. We certainly do
  not want the parameter @nt{expression}s of a @nt{function_call} to be
  separate masters, as they would then be finalized before the function is
  called.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[The fact that a @nt{function_call} is a master
  does not change the accessibility of the return object denoted by the
  @nt{function_call}; that depends on the use of the @nt{function_call}.
  The @nt{function_call} is the master
  of any short-lived entities (such as @nt{aggregate}s used as parameters of
  types with task or controlled parts).]}
@end{Ramification}

@Defn2{Term=[finalization], Sec=(of a master)}
For the @i{finalization} of a master,
dependent tasks are first awaited,
as explained in @RefSecNum{Task Dependence - Termination of Tasks}.
Then each object whose accessibility level is
the same as that of the master is finalized
if the object was successfully initialized and still exists.
@Redundant[These actions are performed whether the master is left
  by reaching the last statement or via a transfer of control.]
When a transfer of control causes completion of an execution, each
included master is finalized in order, from innermost outward.
@begin{Ramification}
  As explained in @RefSecNum{Operations of Access Types},
  the set of objects with the same accessibility level
  as that of the master
  includes objects declared immediately within the master,
  objects declared in nested packages,
  objects created by @nt{allocator}s
  (if the ultimate ancestor access type is declared in one of those places)
  and subcomponents of all of these things.
  If an object was already finalized by Unchecked_Deallocation,
  then it is not finalized again when the master is left.

  Note that any object whose accessibility level is deeper than that of
  the master would no longer exist;
  those objects would have been finalized by some inner master.
  Thus, after leaving a master, the only objects yet to be
  finalized are those whose accessibility level is less deep than that
  of the master.

@end{Ramification}
@begin{Honest}
Subcomponents of objects due to be finalized are not finalized
by the finalization of the master;
they are finalized by the finalization of the containing object.
@end{Honest}
@begin{Reason}
We need to finalize subcomponents of objects even if the containing
object is not going to get finalized because it was not fully
initialized.
But if the containing object is finalized, we don't want to require
repeated finalization of the subcomponents,
as might normally be implied by the recursion in finalization of a
master and the recursion in finalization of an object.
@end{Reason}
@begin{Honest}
Formally, completion and leaving refer to executions of constructs or
entities.
However, the standard sometimes (informally) refers
to the constructs or entities whose executions are being completed.
Thus, for example,
@lquotes@;the subprogram call or task is complete@rquotes@;
really means
@lquotes@;@i{the execution of} the subprogram call or task is complete.@rquotes@;
@end{Honest}

@leading@keepnext@RootDefn2{Term=[finalization], Sec=(of an object)}
For the @i{finalization} of an object:
@begin{Itemize}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0099-1]}
  If @Chg{Version=[3],New=[the full type of ],Old=[]}the object is
  @Chg{Version=[3],New=[],Old=[of ]}an elementary type, finalization
  has no effect;
  @begin{Reason}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0099-1]}
    @ChgAdded{Version=[3],Text=[We say @ldquote@;full type@rdquote in this
    and the following bullets as privacy is ignored for the purpose of
    determining the finalization actions of an object; that is as expected
    for @RunTimeTitle rules.]}
  @end{Reason}

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0099-1]}
  If @Chg{Version=[3],New=[the full type of ],Old=[]}the object is
  @Chg{Version=[3],New=[a tagged type, and the tag of the object
  identifies],Old=[of]} a controlled type,
  the Finalize procedure @Chg{Version=[3],New=[of that controlled
  type ],Old=[]}is called;

  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0099-1]}
  If @Chg{Version=[3],New=[the full type of ],Old=[]}the object is
  @Chg{Version=[3],New=[],Old=[of ]}a protected type,
  @Chg{Version=[3],New=[or if the full type of the object is a tagged type
  and the tag of the object identifies a protected type, ],Old=[]}the
  actions defined in @RefSecNum{Protected Units and Protected Objects} are performed;

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00416-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0099-1]}
  If @Chg{Version=[3],New=[the full type of ],Old=[]}the object is
  @Chg{Version=[3],New=[],Old=[of ]}a composite type, then after performing
  the above actions, if any, every component of the object
  is finalized in an arbitrary order, except as follows:@PDefn2{Term=[arbitrary order],Sec=[allowed]}
  if the object has
  a component with an access discriminant constrained by a per-object
  expression, this component is finalized before any components that
  do not have such discriminants; for an object with several components
  with such a discriminant, they are finalized in the
  reverse of the order of their
  @nt<component_declaration>s@Chg{Version=[2],New=[;],Old=[.]}
  @begin{Reason}
    This allows the finalization of a component with an access discriminant
    to refer to other components of the enclosing object prior to
    their being finalized.
  @end{Reason}
  @begin{Honest}
    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0099-1]}
    @ChgAdded{Version=[3],Text=[The components discussed here are all of the
    components that the object actually has, not just those components that are
    statically identified by the type of the object. These can be different if
    the object has a classwide type.]}
  @end{Honest}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[If the object has coextensions (see
  @RefSecNum{Operations of Access Types}), each coextension is
  finalized after the object whose access discriminant designates it.]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0066-1]}
  @ChgAdded{Version=[3],Text=[In the case of an @nt{aggregate} or function call that is used
  (in its entirety) to directly initialize a part of an object, the
  coextensions of the result of evaluating the @nt{aggregate} or function
  call are transfered to become coextensions of the object being
  initialized and are not finalized until the object being initialized
  is ultimately finalized, even if an anonymous object is created
  as part of the operation.]}
@end{Ramification}

@end{Itemize}

@PDefn2{Term=[execution], Sec=(instance of Unchecked_Deallocation)}
Immediately before an instance of Unchecked_Deallocation
reclaims the storage of an object,
the object is finalized.
@Redundant[If an instance of Unchecked_Deallocation is never applied to
an object created by an @nt{allocator},
the object will still exist when the corresponding master completes,
and it will be finalized then.]

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00280-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0051-1],ARef=[AI05-0190-1]}
The @Chg{Version=[3],New=[],Old=[order in which the ]}finalization of a master
performs finalization of objects @Chg{Version=[3],New=[],Old=[is as follows:
Objects ]}created by declarations in the master
@Chg{Version=[3],New=[],Old=[are finalized ]}in the reverse order of their
creation.@Chg{Version=[3],New=[],Old=[ For objects that were created by
@nt{allocator}s for an access type whose
ultimate ancestor is declared in the master,
this rule is applied as though each
such object that still exists had been created
in an arbitrary order
at the first freezing point
(see @RefSecNum{Freezing Rules})@PDefn2{Term=[arbitrary order],Sec=[allowed]}
of the ultimate ancestor type@Chg{Version=[2],New=[; the finalization of
these objects is called the @i<finalization of the
collection>@Defn{finalization of the collection}@Defn2{Term=[collection],
Sec=[finalization of]}],Old=[]}.@Chg{Version=[3],New=[ Objects created by
allocators for an anonymous access type that are not coextensions of some other
object, are finalized in an arbitrary order during the finalization of their
associated master.@PDefn2{Term=[arbitrary order],Sec=[allowed]}],Old=[]}]}@Chg{Version=[2],
New=[ After the finalization of a master is complete, the objects finalized
as part of its finalization cease to @i<exist>, as do any types and subtypes
defined and created within the master.@PDefn2{Term=[exist],Sec=[cease to]}
@PDefn2{Term=[cease to exist],Sec=[object]}
@Defn2{Term=[cease to exist],Sec=[type]}],Old=[]}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0190-1]}@ChgNote{Moved down}
  @ChgDeleted{Version=[3],Text=[Note that we talk about the type of the
  @nt{allocator} here. There may be access values of a (general) access type
  pointing at objects created by @nt{allocator}s for some other type; these are
  not finalized at this point.]}

  @ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0190-1]}@ChgNote{Moved down}
  @ChgDeleted{Version=[3],Text=[The freezing point of the ultimate ancestor access type is chosen
  because before that point, pool elements cannot be created,
  and after that point, access values designating (parts of)
  the pool elements can be created.
  This is also the point after which the pool object cannot have been
  declared.
  We don't want to finalize the pool elements until after anything
  finalizing objects that contain access values designating them.
  Nor do we want to finalize pool elements after finalizing the pool
  object itself.]}
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0190-1]}@ChgNote{Moved down}
@ChgDeleted{Version=[3],Text=[Finalization of allocated objects is done according
  to the (ultimate ancestor) @nt{allocator} type, not according to the storage pool
  in which they are allocated.
  Pool finalization might reclaim storage (see @RefSec{Storage Management}),
  but has nothing (directly) to do with finalization of the
  pool elements.]}

@ChgRef{Version=[3],Kind=[Deleted],ARef=[AI05-0190-1]}@ChgNote{Moved down}
@ChgDeleted{Version=[3],Text=[Note that finalization is done only for objects
that still exist; if an instance of Unchecked_Deallocation has already gotten
rid of a given pool element, that pool element will not be finalized when
the master is left.]}

Note that a deferred constant declaration does not create the
constant; the full constant declaration creates it.
Therefore, the order of finalization depends on where the full
constant declaration occurs,
not the deferred constant declaration.

An imported object is not created by its declaration.
It is neither initialized nor finalized.
@end{Ramification}
@begin{ImplNote}
An implementation has to ensure that the storage for an object is not
reclaimed when references to the object are still possible
(unless, of course, the user explicitly requests reclamation via an
instance of Unchecked_Deallocation).
This implies, in general, that objects cannot be deallocated one by one
as they are finalized;
a subsequent finalization might reference an object that has been
finalized, and that object had better be in its (well-defined)
finalized state.
@end{ImplNote}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0190-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[Each nonderived access type @i<T> has
an associated @i<collection>,@Defn2{Term=[collection],Sec=[of an access type]}
which is the set of objects created by @nt{allocator}s of @i<T>,
or of types derived from @i<T>. Unchecked_Deallocation removes an object from
its collection. Finalization of a collection consists of finalization of each
object in the collection, in an arbitrary order. The collection of an access
type is an object implicitly declared at the following place:@PDefn2{Term=[arbitrary order],Sec=[allowed]}]}

@begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[The place of the implicit declaration determines
  when allocated objects are finalized. For multiple collections declared at the
  same place, we do not define the order of their implicit declarations.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}@ChgNote{Moved down}
  @ChgAdded{Version=[3],Text=[Finalization of allocated objects is done according
  to the (ultimate ancestor) @nt{allocator} type, not according to the storage
  pool in which they are allocated. Pool finalization might reclaim storage (see
  @RefSec{Storage Management}), but has nothing (directly) to do with
  finalization of the pool elements.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}@ChgNote{Moved down}
  @ChgAdded{Version=[3],Text=[Note that finalization is done only for objects that
  still exist; if an instance of Unchecked_Deallocation has already gotten rid
  of a given pool element, that pool element will not be finalized when the
  master is left.]}
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}@ChgNote{Moved down}
  @ChgAdded{Version=[3],Text=[Note that we talk about the type of the
  @nt{allocator} here. There may be access values of a (general) access type
  pointing at objects created by @nt{allocator}s for some other type; these are
  not (necessarily) finalized at this point.]}
@end{Reason}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[For a named access type, the first freezing point (see
@RefSecNum{Freezing Rules}) of the type.]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0190-1]}@ChgNote{Moved down}
  @ChgAdded{Version=[3],Text=[The freezing point of the ultimate ancestor access
  type is chosen because before that point, pool elements cannot be created,
  and after that point, access values designating (parts of)
  the pool elements can be created.
  This is also the point after which the pool object cannot have been declared.
  We don't want to finalize the pool elements until after anything
  finalizing objects that contain access values designating them.
  Nor do we want to finalize pool elements after finalizing the pool
  object itself.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[For the type of an access parameter, the call that
contains the @nt{allocator}.]}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[For the type of an access result, within the master of
the call (see @RefSecNum{Operations of Access Types}).]}
@begin{Honest}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[We mean at a place within the master consistent
  with the execution of the call within the master. We don't say that
  normatively, as it is difficult to explain that when the master of the call
  need not be the master that immediately includes the call (such as when an
  anonymous result is converted to a named access type).]}
@end{Honest}

@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[For any other anonymous access type, the first
freezing point of the innermost enclosing declaration.]}
@end{Itemize}


@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
@PDefn2{Term=[execution], Sec=(assignment_statement)}
The target of an @Chg{Version=[2],New=[@nt{assignment_statement}],
Old=[assignment statement]} is finalized before copying in the
new value, as explained
in @RefSecNum{Assignment and Finalization}.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0021],ARef=[AI95-00182-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00162-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0066-1],ARef=[AI05-0142-4],ARef=[AI05-0269-1]}
@Chg{Version=[2],New=[The master of an object is the master enclosing its
creation whose accessibility level (see @RefSecNum{Operations of Access Types})
is equal to that of the object@Chg{Version=[3],New=[, except
in the case of an anonymous object representing the result of
an @nt{aggregate} or function call. If such an anonymous object
is part of the result of evaluating the actual parameter expression for
an explicitly aliased
parameter of a function call, the master of the object is the innermost
master enclosing the evaluation of the @nt{aggregate} or function call, excluding
the @nt{aggregate} or function call itself. Otherwise, the master of such
an anonymous object is the innermost master enclosing the evaluation of the
@nt{aggregate} or function call, which may be the @nt{aggregate} or function
call itself],Old=[]}.],
Old=[@Chg{New=[If the @i{object_}@nt{name} in an @nt{object_renaming_declaration}, or
the actual parameter for a generic formal @key[in out] parameter in a
@nt{generic_instantiation}, denotes any part of an anonymous object created by
a function call, the anonymous object is not finalized until after it is no
longer accessible via any name. Otherwise, an],
Old=[The]} anonymous object@Chg{New=[],Old=[s]} created by
@Chg{New=[a ],Old=[]}function @Chg{New=[call or],Old=[calls and]} by
@Chg{New=[an ],Old=[]}@nt{aggregate}@Chg{New=[ is],Old=[s are]}
finalized no later than the end of the innermost enclosing
@nt{declarative_item} or @nt{statement};
if that is a @nt{compound_statement},
@Chg{New=[the object is],Old=[they are]} finalized before starting the
execution of any @nt{statement} within the @nt{compound_statement}.]}
@begin{Honest}
@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00162-01]}@ChgNote{Should be ChgDeleted}
@ChgDeleted{Version=[2],Text=[@leading@;This is not to be construed as permission to call Finalize
asynchronously with respect to normal user code.
For example,]}
@begin{Example}
@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[@key[declare]
    X : Some_Controlled_Type := F(G(...));
    --@RI{ The anonymous objects created for F and G are finalized}
    --@RI{ no later than this point.}
    Y : ...
@key[begin]
    ...
@key[end];]}
@end{Example}

@ChgRef{Version=[2],Kind=[Deleted]}
@ChgDeleted{Version=[2],Text=[
The anonymous object for G should not be finalized at some random
point in the middle of the body of F,
because F might manipulate the same data structures as
the Finalize operation, resulting in erroneous access
to shared variables.]}
@end{Honest}
@begin{Reason}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00162-01]}
@Chg{Version=[2],New=[This effectively imports all of the special rules for
the accessibility level of renames, @nt{allocator}s, and so on, and applies
them to determine where objects created in them are finalized. For instance,
the master of a rename of a subprogram is that of the renamed subprogram.],
Old=[It might be quite inconvenient for the implementation to defer
finalization of the anonymous object for G until after copying the
value of F into X, especially if the size of the result
is not known at the call site.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0066-1]}
@ChgAdded{Version=[3],Text=[In @RefSecNum{Operations of Access Types}
we assign an accessibility level to the result of an
@nt{aggregate} or function call that is used to directly initialize a
part of an object based on the object being initialized. This is
important to ensure that any access discriminants denote objects
that live at least as long as the object being initialized.
However, if the result of the @nt{aggregate} or function call is not
built directly in the target object, but instead is built in an
anonymous object that is then assigned to the target, the anonymous
object needs to be finalized after the assignment rather than
persisting until the target object is finalized (but not its
coextensions). (Note than an implementation is never required to
create such an anonymous object, and in some cases is required to
@i{not} have such a separate object, but rather to build the result
directly in the target.)]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4]}
@ChgAdded{Version=[3],Text=[The special case for explicitly aliased parameters
of functions is needed for the same reason, as access discriminants of the
returned object may designate one of these parameters. In that case, we want to
lengthen the lifetime of the anonymous objects as long as the possible lifetime
of the result.]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4]}
@ChgAdded{Version=[3],Text=[We don't do a similar change for other kinds of
calls, because the extended lifetime of the parameters adds no value, but could
constitute a storage leak. For instance, such an anonymous object created by a
procedure call in the elaboration part of a package body would have to live
until the end of the program, even though it could not be used after the
procedure returns (other than via Unchecked_Access).]}
@end{Reason}

@begin{Ramification}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4]}
@ChgAdded{Version=[3],Text=[Note that the lifetime of the master given to
anonymous objects in explicitly aliased parameters of functions is not
necessarily as long as the lifetime of the master of the object being
initialized (if the function call is used to initialize an @nt{allocator}, for
instance). In that case, the accessibility check on explicitly aliased
parameters will necessarily fail if any such anonymous objects exist. This is
necessary to avoid requiring the objects to live as long as the access type or
having the implementation complexity of an implicit coextension.]}
@end{Ramification}


@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0023],ARef=[AI95-00169-01]}
@ChgRef{Version=[2],Kind=[RevisedAdded],ARef=[AI95-00162-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0066-1],ARef=[AI05-0262-1]}
@Chg{Version=[2],New=[In the case of an @nt{expression} that is a master,
finalization of any (anonymous) objects occurs @Chg{Version=[3],New=[after
completing],Old=[as the final part of]}
evaluation of the @nt{expression}@Chg{Version=[3],New=[ and all use of the
objects, prior to starting the execution of any subsequent construct],Old=[]}.],
Old=[@Chg{New=[If a transfer of control or raising of an exception occurs prior to
performing a finalization of an anonymous object, the anonymous object is
finalized as part of the finalizations due to be performed for the object's
innermost enclosing master.],Old=[]}]}

@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0023],ARef=[AI95-00169-01]}
@leading@PDefn2{Term=(bounded error),Sec=(cause)}
It is a bounded error for a call on Finalize or Adjust @Chg{New=[that occurs as
part of object finalization or assignment ], Old=[]}to propagate an exception.
The possible consequences depend on what action invoked the Finalize or
Adjust operation:
@begin{Ramification}
  It is not a bounded error for Initialize to propagate an
  exception. If Initialize propagates an exception,
  then no further calls on Initialize are performed,
  and those components that have already been initialized
  (either explicitly or by default)
  are finalized in the usual way.

  @ChgRef{Version=[1],Kind=[Added],Ref=[8652/0023],ARef=[AI95-00169-01]}
  @Chg{New=[It also is not a bounded error for an explicit call to Finalize or
  Adjust to propagate an exception. We do not want implementations to have to
  treat explicit calls to these routines specially.],Old=[]}
@end{Ramification}
@begin{Itemize}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
For a Finalize invoked as part of an @nt<assignment_statement>,
Program_Error is raised at that point.

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0024],ARef=[AI95-00193-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
@Chg{New=[For an Adjust invoked as part of @Chg{Version=[2],New=[assignment
operations other than those invoked as part of an @nt{assignment_statement}],
Old=[the initialization of a controlled object]}, other adjustments due to be
performed might or might not be performed, and then Program_Error is raised.
During its propagation, finalization might or
might not be applied to objects whose Adjust failed.],Old=[]}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
For an Adjust invoked as part of an @Chg{Version=[2],New=[@nt{assignment_statement}],
Old=[assignment @Chg{New=[statement],Old=[operation]}]}, any other adjustments
due to be performed are performed, and then Program_Error is raised.
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0024],ARef=[AI95-00193-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
@ChgAdded{Version=[1],Text=[In the case of assignments that are part of
initialization, there is
no need to complete all adjustments if one propagates an exception, as the
object will immediately be finalized. So long as a subcomponent is not going
to be finalized, it need not be adjusted, even if it is initialized as part of
an enclosing composite assignment operation for which some adjustments are
performed. However, there is no harm in an implementation making additional
Adjust calls (as long as any additional components that are adjusted are also
finalized), so we allow the implementation flexibility here.
On the other hand, for an @Chg{Version=[2],New=[@nt{assignment_statement}],
Old=[assignment statement]}, it is important that all
adjustments be performed, even if one fails, because all controlled
subcomponents are going to be finalized.@Chg{Version=[2],New=[ Other kinds of
assignment are more like initialization than @nt{assignment_statement}s, so we
include them as well in the permission.],Old=[]}]}
@end{Reason}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0024],ARef=[AI95-00193-01]}
@ChgAdded{Version=[1],Text=[Even if an Adjust invoked as part of the
initialization of a
controlled object propagates an exception, objects whose initialization
(including any Adjust or Initialize calls) successfully completed will be
finalized. The permission above only applies to objects whose Adjust failed.
Objects for which Adjust was never even invoked must not be finalized.]}
@end{Ramification}

@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
For a Finalize invoked as part of a call on an instance of
Unchecked_Deallocation, any other finalizations due to
be performed are performed, and then Program_Error is raised.
@begin{Discussion}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0104],ARef=[AI95-00179-01]}
@ChgAdded{Version=[1],Text=[The standard does not specify if storage is
recovered in this case.
If storage is not recovered (and the object continues to exist), Finalize
may be called on the object again (when the @nt<allocator>'s master is
finalized).]}
@end{Discussion}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0023],ARef=[AI95-00169-01]}
@ChgRef{Version=[3],Kind=[DeletedAdded],ARef=[AI05-0064-1]}
@ChgDeleted{Version=[3],Text=[@Chg{New=[@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
For a Finalize invoked as part of the finalization of the anonymous
object created by a function call or @nt{aggregate}, any other finalizations
due to be performed are performed, and then Program_Error is raised.],Old=[]}]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0023],ARef=[AI95-00169-01]}
@Chg{New=[@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
For a Finalize invoked due to reaching the end of the execution of a
master, any other finalizations associated with the master are performed, and
Program_Error is raised immediately after leaving the master.],Old=[]}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0064-1]}
@begin{Discussion}
@ChgAdded{Version=[3],Text=[This rule covers both ordinary objects created
by a declaration, and anonymous objects created as part of evaluating an
@nt{expression}. All contexts that create objects that need finalization
are defined to be masters.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
For a Finalize invoked by the transfer of control of
an @Chg{Version=[2],
New=[@nt{exit_statement}, return statement, @nt{goto_statement}],
Old=[@ntf{exit_}, @ntf{return_}, @ntf{goto_}]},
or @nt{requeue_@!statement},
Program_Error is raised no earlier than after the finalization of the
master being finalized when the exception occurred,
and no later than the point where normal execution would have continued.
Any other finalizations due to be performed up to that point are
performed before raising Program_Error.
@begin{Ramification}
For example, upon leaving a @nt{block_statement} due to a
@nt{goto_statement}, the Program_Error would be raised at the point of the
target statement denoted by the label,
or else in some more dynamically nested place,
but not so nested as to allow an @nt{exception_handler} that has
visibility upon the finalized object to handle it.
For example,
@begin{Example}
@key[procedure] Main @key[is]
@key[begin]
    <<The_Label>>
    Outer_Block_Statement : @key[declare]
        X : Some_Controlled_Type;
    @key[begin]
        Inner_Block_Statement : @key[declare]
            Y : Some_Controlled_Type;
            Z : Some_Controlled_Type;
        @key[begin]
            @key[goto] The_Label;
        @key[exception]
            @key[when] Program_Error => ... --@RI{ Handler number 1.}
        @key[end];
    @key[exception]
        @key[when] Program_Error => ... --@RI{ Handler number 2.}
    @key[end];
@key[exception]
    @key[when] Program_Error => ... --@RI{ Handler number 3.}
@key[end] Main;
@end{Example}

The @nt{goto_statement} will first cause
Finalize(Y) to be called.
Suppose that Finalize(Y) propagates an exception.
Program_Error will be raised after leaving Inner_Block_Statement,
but before leaving Main.
Thus, handler number 1 cannot handle this Program_Error;
it will be handled either by handler number 2 or handler number 3.
If it is handled by handler number 2,
then Finalize(Z) will be done before executing the handler.
If it is handled by handler number 3,
then Finalize(Z) and Finalize(X) will both
be done before executing the handler.
@end{Ramification}

For a Finalize invoked by a transfer of control
that is due to raising an exception,
any other finalizations due to be performed for the same master are
performed; Program_Error is raised immediately after leaving the master.
@begin{Ramification}
If, in the above example, the @nt{goto_statement} were replaced by a
@nt{raise_statement}, then the Program_Error would be handled by
handler number 2, and
Finalize(Z) would be done before executing the handler.
@end{Ramification}
@begin{Reason}
We considered treating this case in the same way as the others,
but that would render certain @nt{exception_handler}s useless.
For example, suppose the only @nt{exception_handler} is
one for @key{others} in the main subprogram.
If some deeply nested call raises an exception,
causing some Finalize operation to be called,
which then raises an exception,
then normal execution @lquotes@;would have continued@rquotes@;
at the beginning of the @nt{exception_handler}.
Raising Program_Error at that point would cause that
handler's code to be skipped.
One would need two nested @nt{exception_handler}s
to be sure of catching such cases!

On the other hand, the @nt{exception_handler} for a given master
should not be allowed to handle exceptions raised during finalization
of that master.
@end{Reason}

For a Finalize invoked by a transfer of control due to
an abort or selection of a terminate alternative,
the exception is ignored;
any other finalizations due to be performed are performed.
@begin{Ramification}
This case includes an asynchronous transfer of control.
@end{Ramification}
@begin{Honest}
@Defn2{Term=[Program_Error],Sec=(raised by failure of run-time check)}
This violates the general principle that it is always possible for
a bounded error to raise Program_Error
(see @RefSec{Classification of Errors}).
@end{Honest}
@end{Itemize}
@end{Bounded}

@begin{ImplPerm}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0107-1]}
@ChgAdded{Version=[3],Text=[If the execution of an @nt{allocator} propagates an
exception, any parts of the allocated object that were successfully initialized
may be finalized as part of the finalization of the innermost master enclosing
the @nt{allocator}.]}

  @begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This allows deallocating the memory for the
  allocated object at the innermost master, preventing a storage leak.
  Otherwise, the object would have to stay around until the finalization of the
  collection that it belongs to, which could be the entire life of the program
  if the associated access type is library level.]}
  @end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0111-3],ARef=[AI05-0262-1]}
@ChgAdded{Version=[3],Text=[The implementation may finalize objects created by
@nt{allocator}s for an access type whose storage pool supports subpools (see
@RefSecNum{Storage Subpools}) as if the objects were created (in an arbitrary
order) at the point where the storage pool was elaborated instead of at the first
freezing point of the access type.@PDefn2{Term=[arbitrary order],Sec=[allowed]}]}

  @begin{Ramification}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This allows the finalization of such objects to
  occur later than they otherwise would, but still as part of the finalization
  of the same master. Accessibility rules in @RefSecNum{Storage Subpools} ensure
  that it is the same master (usually that of the environment task).]}
  @end{Ramification}

  @begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This permission is intended to allow the allocated
  objects to "belong" to the subpool objects and to allow those objects to be
  finalized at the time that the storage pool is finalized (if they are not
  finalized earlier). This is expected to ease implementation, as the objects
  will only need to belong to the subpool and not also to the collection.]}
  @end{ImplNote}

@end{ImplPerm}


@begin{Notes}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The rules of @Chg{Version=[3],New=[Clause],Old=[Section]} 10 imply that
immediately prior to partition termination, Finalize operations
are applied to library-level controlled objects (including those
created by @nt{allocator}s of library-level access types, except those
already finalized).
This occurs after waiting for library-level
tasks to terminate.
@begin{Discussion}
We considered defining a pragma that would apply to a controlled
type that would suppress Finalize operations for library-level objects
of the type upon partition termination.
This would be useful for types whose finalization actions
consist of simply reclaiming global heap storage, when this is already
provided automatically by the environment upon program termination.
@end{Discussion}

A constant is only constant between its initialization and
finalization.
Both initialization and finalization are allowed to
change the value of a constant.

Abort is deferred during certain operations related to controlled types,
as explained in
@RefSecNum{Abort of a Task - Abort of a Sequence of Statements}.
Those rules prevent an abort from causing a controlled object
to be left in an ill-defined state.

The Finalize procedure is called upon finalization of
a controlled object, even if Finalize was called earlier,
either explicitly or as part of an assignment; hence,
if a controlled type is visibly controlled (implying that its Finalize
primitive is directly callable), or is nonlimited (implying that
assignment is allowed), its Finalize procedure should be
designed to have no ill effect if it is applied a second time
to the same object.
@begin{Discussion}
  Or equivalently, a Finalize procedure
  should be @lquotes@;idempotent@rquotes@;;
  applying it twice to the same object should be equivalent to
  applying it once.
@end{Discussion}
@begin{Reason}
  A user-written Finalize procedure should be idempotent since it
  can be called explicitly
  by a client (at least if the type is "visibly" controlled).
  Also, Finalize is used implicitly as part of the
  @nt<assignment_statement> if the
  type is nonlimited, and an abort is permitted to disrupt an
  @nt<assignment_statement> between finalizing the left-hand side
  and assigning the new value to it (an abort is not permitted to
  disrupt an assignment operation between copying in the new value
  and adjusting it).
@end{Reason}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
Either Initialize or Adjust,
but not both, is applied to (almost) every controlled object
when it is created:
Initialize is done when no initial value is assigned to the object,
whereas Adjust is done as part of assigning the initial value.
The one exception is the @Chg{Version=[2],New=[],Old=[anonymous ]}object
@Chg{Version=[2],New=[initialized],Old=[created]} by an
@nt{aggregate}@Chg{Version=[2],New=[ (both the anonymous object created for
an aggregate, or an object initialized by an @nt{aggregate} that is
built-in-place)],Old=[]}; Initialize is not applied to the @nt{aggregate}
as a whole, nor is the value of the @nt<aggregate> @Chg{Version=[2],
New=[or object ],Old=[]}adjusted.

@leading@Defn2{Term=[assignment operation], Sec=(list of uses)}
All of the following use the assignment operation,
and thus perform value adjustment:
@begin{Itemize}
the @nt{assignment_statement} (see @RefSecNum{Assignment Statements});

explicit initialization of a stand-alone object
(see @RefSecNum{Object Declarations})
or of a pool element (see @RefSecNum{Allocators});

default initialization of a component of a stand-alone object
or pool element
(in this case, the value of each component is assigned, and therefore adjusted,
but the value of the object as a whole is not adjusted);

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00318-02]}
function return, when the result @Chg{Version=[2],New=[is not built-in-place],
Old=[type is not a return-by-reference
type (see @RefSecNum{Return Statements});]}
(adjustment of the result happens before finalization of
the function@Chg{Version=[2],New=[],Old=[;
values of return-by-reference types are not
adjusted]});

predefined operators (although the only one that matters is
concatenation; see @RefSecNum{Binary Adding Operators});

generic formal objects of mode @key{in}
(see @RefSecNum{Formal Objects});
these are defined in terms of constant declarations; and

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
@nt{aggregate}s (see @RefSecNum{Aggregates})@Chg{Version=[2],New=[, when
the result is not built-in-place],Old=[]}
(in this case, the value of each component, and the parent part, for an
@nt{extension_aggregate}, is assigned, and therefore adjusted,
but the value of the @nt{aggregate} as a whole is not adjusted;
neither is Initialize called);
@end{Itemize}

@leading@;The following also use the assignment operation,
but adjustment never does anything interesting in these cases:
@begin{Itemize}
By-copy parameter passing uses the assignment operation
(see @RefSecNum{Parameter Associations}),
but controlled objects are always passed by reference,
so the assignment operation never does anything interesting in
this case.
If we were to allow by-copy parameter passing for controlled objects,
we would need to make sure that the actual is finalized before doing
the copy back for [@key{in}] @key{out} parameters.
The finalization of the parameter itself needs to happen
after the copy back (if any),
similar to the finalization of an anonymous function return
object or @nt{aggregate} object.

@key{For} loops use the assignment operation
(see @RefSecNum{Loop Statements}), but since the type
of the loop parameter is never controlled,
nothing interesting happens there, either.

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00318-02]}
@ChgAdded{Version=[2],Text=[Objects initialized by function results and
@nt{aggregate}s that are built-in-place. In this case, the assignment
operation is never executed, and no adjustment takes place. While
built-in-place
is always allowed, it is required for some types @em see
@RefSecNum{Limited Types} and
@RefSecNum{Assignment and Finalization} @em and that's
important since limited types have no Adjust to call.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00287-01]}
@ChgDeleted{Version=[2],Text=[Because Controlled and Limited_Controlled
are library-level tagged types,
all controlled types will be library-level types,
because of the accessibility rules
(see @RefSecNum{Operations of Access Types} and @RefSecNum{Type Extensions}).
This ensures that the Finalize operations may be applied without
providing any @lquotes@;display@rquotes@; or @lquotes@;static-link.@rquotes@;
This simplifies finalization as a result of garbage collection,
abort, and asynchronous transfer of control.]}

Finalization of the parts of a protected object are not done as
protected actions.
It is possible (in pathological cases)
to create tasks during finalization that
access these parts in parallel with the finalization itself.
This is an erroneous use of shared variables.
@end{Discussion}
@begin{ImplNote}
One implementation technique for finalization is to chain the
controlled objects together on a per-task list.
When leaving a master, the list can be walked up to a marked place.
The links needed to implement the list can be declared (privately)
in types Controlled and Limited_Controlled,
so they will be inherited by all controlled types.

Another implementation technique, which we refer to as the @lquotes@;PC-map@rquotes@;
approach essentially implies inserting exception handlers at various
places, and finalizing objects based on where the exception was
raised.

@Defn{PC-map approach to finalization}
@Defn{program-counter-map approach to finalization}
The PC-map approach is for the compiler/linker to create a map of
code addresses; when an exception is raised, or abort occurs,
the map can be consulted to see where the task was executing,
and what finalization needs to be performed.
This approach was given in the Ada 83 Rationale as a
possible implementation strategy for exception handling @em the
map is consulted to determine which exception handler applies.

If the PC-map approach is used, the implementation must take care in the
case of arrays. The generated code will generally contain a loop to
initialize an array. If an exception is raised part way through the
array, the components that have been initialized must be finalized,
and the others must not be finalized.

It is our intention that both of these implementation methods should
be possible.
@end{ImplNote}
@end{Notes}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
Finalization depends on the concepts of completion and leaving,
and on the concept of a master.
Therefore, we have moved the definitions of these concepts here,
from where they used to be in
@Chg{Version=[3],New=[Clause],Old=[Section]} @RefSecNum{Tasks and Synchronization}.
These concepts also needed to be generalized somewhat.
Task waiting is closely related to user-defined finalization;
the rules here refer to the task-waiting rules of
@Chg{Version=[3],New=[Clause],Old=[Section]} @RefSecNum{Tasks and Synchronization}.
@end{DiffWord83}

@begin{Inconsistent95}
  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0066-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 95}@b<Ada 2012 Correction:>
  Changed the definition
  of the master of an anonymous object used to directly initialize an
  object, so it can be finalized immediately rather than having to hang
  around as long as the object. In this case, the Ada 2005 definition was
  inconsistent with Ada 95, and Ada 2012 changes it back. It is unlikely
  that many compilers implemented the rule as written in Amendment 1,
  so an inconsistency is unlikely to arise in practice.]}
@end{Inconsistent95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0021],ARef=[AI95-00182-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Fixed the wording to say that
  anonymous objects aren't finalized until the object can't be used anymore.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0023],ARef=[AI95-00169-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Added wording to clarify what
  happens when Adjust or Finalize raises an exception; some cases had been
  omitted.]}

  @ChgRef{Version=[1],Kind=[AddedNormal],Ref=[8652/0024],ARef=[AI95-00193-01]}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00256-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Stated that if Adjust raises an
  exception during initialization, nothing further is required. This is
  corrected in Ada 2005 to include all kinds of assignment other than
  @nt{assignment_statement}s.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00162-01],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Revised the definition of master to include
  @nt{expression}s and @nt{statement}s, in order to cleanly define what
  happens for tasks and controlled objects created as part of a subprogram call.
  Having done that, all of the special wording to cover those cases is
  eliminated (at least until the Ada comments start rolling in).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00280-01]}
  @ChgAdded{Version=[2],Text=[We define @i{finalization of the collection}
  here, so as to be able to conveniently refer to it in other rules (especially
  in @RefSec{Allocators}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Clarified that a coextension is finalized at
  the same time as the outer object. (This was intended for Ada 95, but since
  the concept did not have a name, it was overlooked.)]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0051-1],ARef=[AI05-0190-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}@b<Correction:>
  Better defined when objects allocated from anonymous access types are
  finalized. This could be inconsistent if objects are finalized in
  a different order than in an Ada 2005 implementation and that order caused
  different program behavior; however programs that depend on the order of
  finalization within a single master are already fragile and hopefully
  are rare.]}
@end{Inconsistent2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0064-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Removed a redundant rule,
  which is now covered by the additional places where masters are defined.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0099-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified the finalization
  rules so that there is no doubt that privacy is ignored, and to ensure
  that objects of classwide interface types are finalized based on their
  specific concrete type.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0107-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Allowed premature finalization
  of parts of failed @nt{allocator}s. This could be an inconsistency, but the
  previous behavior is still allowed and there is no requirement that
  implementations take advantage of the permission.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
  @ChgAdded{Version=[3],Text=[Added a permission to finalize object allocated
  from a subpool later than usual.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0142-4]}
  @ChgAdded{Version=[3],Text=[Added text to specially define the master of
  anonymous objects which are passed as explicitly aliased parameters (see
  @RefSecNum{Subprogram Declarations}) of functions. The model for these
  parameters is explained in detail in @RefSecNum{Parameter Associations}.]}
@end{DiffWord2005}
