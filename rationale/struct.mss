@Part(xxx, Root="rat.msm")

@comment($Source: e:\\cvsroot/ARM/Rationale/struct.mss,v $)
@comment($Revision: 1.2 $ $Date: 2006/01/27 06:40:05 $)

@LabeledSection{Structure and visibility}

@Subheading{Abstract}

@i{This paper
describes various improvements in the areas of structure and visibility
for Ada 2005.}

@i{The most important improvement is perhaps the introduction of limited
with clauses which permit types in two packages to refer to each other.
A related addition to context clauses is the private with clause which
just provides access from a private part.}

@i{There are also important improvements to limited types which make
them much more useful; these include initialization with aggregates
and composition using a new form of return statement.}

@i{This is one of a number of papers concerning Ada 2005 which are being
published in the Ada User Journal. An earlier version of this paper
appeared in the Ada User Journal, Vol. 26, Number 2, June 2005. Other
papers in this series will be found in later issues of the Journal
or elsewhere on this website.}

@LabeledClause{Ada Issues: Structure and visibility}

The WG9 guidance document @LocalLink{Target=[R1],Sec=[References],Text={[1]}}
identifies the solution of the problem of mutually dependent types as one of
the two specific issues that
need to be addressed in devising Ada 2005.

@leading@keepnext@;Moreover the guidance document also emphasizes
@begin[SyntaxText]
Improvements that will remedy shortcomings in Ada. It cites in
particular improvements in OO features, specifically, adding a Java-like
interface feature and improved interfacing to other OO languages.
@end[SyntaxText]

OO is largely about structure and visibility and so further improvements
and in particular those that remedy shortcomings are desirable.

The following Ada Issues cover the relevant changes and are described
in detail in this paper:

@begin[Description]
@AILink{AI=[AI95-00217-06],Text=[217]}@\Mutually recursive types @en limited
with

@AILink{AI=[AI95-00262-01],Text=[262]}@\Access to private units in the private
part

@AILink{AI=[AI95-00287-01],Text=[287]}@\Limited aggregates allowed

@AILink{AI=[AI95-00318-02],Text=[318]}@\Limited and anonymous access return
types

@AILink{AI=[AI95-00326-01],Text=[326]}@\Tagged incomplete types

@AILink{AI=[AI95-00412-01],Text=[412]}@\Subtypes and renamings of incomplete
entities

These changes can be grouped as follows.


@end[Description]
First there is the important solution to the problem of mutually dependent
types across packages provided by the introduction of limited with clauses
(@AILink{AI=[AI95-00217-06],Text=[217]}). Related changes are the introduction
of tagged incomplete types (@AILink{AI=[AI95-00326-01],Text=[326]}) and the
ability to have subtypes and renamings of incomplete views
(@AILink{AI=[AI95-00412-01],Text=[412]}).

Another improvement to the visibility rules is the introduction of
private with clauses (@AILink{AI=[AI95-00262-01],Text=[262]}).

There are some changes to aggregates. These were triggered by problems
with limited types but apply to aggregates in general
(part of @AILink{AI=[AI95-00287-01],Text=[287]}).

An important area is that of limited types which are somewhat confused
in Ada 95. There are two changes which permit limited values to be
built @i[in situ]. One is the use of aggregates for initialization
and the other is a more elaborate return statement which enables the
construction of limited values when returning from a
function (@AILink{AI=[AI95-00287-01],Text=[287]},
@AILink{AI=[AI95-00318-02],Text=[318]}).


@LabeledClause{Mutually dependent types}

For many programmers the solution of the problem of mutually dependent
types will be the single most important improvement introduced in
Ada 2005.@Defn{mutually dependent types}

@leading@;This topic was discussed in the Introduction using an example of two
mutually dependent types, @exam[Point] and @exam[Line]. Each type
needed to refer to the other in its declaration and of course the
solution to this problem is to use incomplete types. In Ada 95 there
are three stages. We first declare the incomplete types
@begin[Example]
@tabset[P35]
@key[type] Point;@\-- @examcom[incomplete types]
@key[type] Line;
@end[Example]

Suppose for simplicity that we wish to study patterns of points and
lines such that each point has exactly three lines through it and
that each line has exactly three points on it. (This is not so stupid.
The two most fundamental theorems of projective geometry, those of
Pappus and Desargues, concern such structures and so does the simplest
of finite geometries, the Fano plane.)

@leading@keepnext@;Using the incomplete types we can then declare
@begin[Example]
@tabset[P35]
@key[type] Point_Ptr @key[is access] Point;@\-- @examcom[use incomplete types]
@key[type] Line_Ptr @key[is access] Line;
@end[Example]

@leading@keepnext@;and finally we can complete the type declarations thus
@begin[Example]
@tabset[P35]
@key[type] Point @key[is]@\-- @examcom[complete the types]
   @key[record]
      L, M, N: Line_Ptr;
   @key[end record];

@key[type] Line @key[is]
   @key[record]
      P, Q, R: Point_Ptr;
   @key[end record];
@end[Example]

@leading@;Of course, in Ada 2005, as discussed in the previous paper
(see @RefSecNum{Anonymous access types}), we can
use anonymous access types more freely so that the second stage can
be omitted in this example. As a consequence the complete declarations
are simply
@begin[Example]
@tabset[P35]
@key[type] Point @key[is]@\-- @examcom[complete the types]
   @key[record]
      L, M, N: @key[access] Line;
   @key[end record];

@key[type] Line @key[is]
   @key[record]
      P, Q, R: @key[access] Point;
   @key[end record];
@end[Example]

This has the important advantage that we do not have to invent irritating
identifiers such as @exam[Point_Ptr].

@leading@;But we will stick to Ada 95 for the moment. In Ada 95 there are two
rules
@begin[Itemize]
the incomplete type can only be used in the definition
of access types;

the complete type declaration must be in the same declarative
region as the incomplete type.
@end[Itemize]

@leading@keepnext@;The first rule does actually permit
@begin[Example]
@key[type] T;
@key[type] A @key[is access procedure ](X: @key[in out] T);
@end[Example]

Note that we are here using the incomplete type @exam[T] for a parameter.
This is not normally allowed, but in this case the procedure itself
is being used in an access type. The additional level of indirection
means that the fact that the parameter mechanism for @exam[T] is not
known yet does not matter.

@leading@;Apart from this, it is not possible to use an incomplete type for
a parameter in a subprogram in Ada 95 except in the case of an access
parameter. Thus we cannot have
@begin[Example]
@key[function] Is_Point_On_Line(P: Point; L: Line) @key[return] Boolean;
@end[Example]

before the complete type declarations.

@leading@;It is also worth pointing out that the problem of mutually dependent
types (within a single unit) can often be solved by using private
types thus
@begin[Example]
   @key[type] Point @key[is private];
   @key[type] Point_Ptr @key[is access] Point;
   @key[type] Line@key[ is private];
   @key[type] Line_Ptr@key[ is access] Line;
@key[private]

   @key[type] Point @key[is]
      @key[record]
         L, M, N: Line_Ptr;
      @key[end record];

   @key[type] Line @key[is]
      @key[record]
         P, Q, R: Point_Ptr;
      @key[end record];
@end[Example]

But we need to use incomplete types if we want the user to see the
full view of a type so the situation is somewhat different.

As an aside, remember that if an incomplete type is declared in a
private part then the complete type can be deferred to the body (this
is the so-called Taft Amendment in Ada 83). In this case neither the
user nor indeed the compiler can see the complete type and this is
the main reason why we cannot have parameters of incomplete types
whereas we can for private types.

@leading@;We will now introduce what has become a canonical example for
discussing this topic. This concerns employees and the departments of the
organization in which they work. The information about employees needs to refer
to the departments and the departments need to refer to the employees. We
assume that the material regarding employees and departments is quite large so
that we naturally wish to declare the two types in distinct packages
@exam[Employees] and @exam[Departments]. So we would like to say
@begin[Example]
@key[with] Departments; @key[use] Departments;
@key[package] Employees @key[is]
   @key[type] Employee @key[is private];
   @key[procedure] Assign_Employee(E: @key[in out] Employee; D: @key[in out] Department);
   @key[type] Dept_Ptr @key[is access all] Department;
   @key[function] Current_Department(E: Employee) @key[return] Dept_Ptr;
   ...
@key[end] Employees;

@key[with] Employees; @key[use] Employees;
@key[package] Departments @key[is]
   @key[type] Department @key[is private];
   @key[procedure] Choose_Manager(D:@key[ in out] Department; M: @key[in out] Employee);
   ...
@key[end] Departments;
@end[Example]

We cannot write this because each package has a with clause for the
other and they cannot both be declared (or entered into the library)
first.

We assume of course that the type @exam[Employee] includes information
about the @exam[Department] for whom the @exam[Employee] works and
the type @exam[Department] contains information regarding the manager
of the department and presumably a list of the other employees as
well @en note that the manager is naturally also an @exam[Employee].

@leading@;So in Ada 95 we are forced to put everything into one package thus
@begin[Example]
@key[package] Workplace @key[is]
   @key[type] Employee @key[is private];
   @key[type] Department @key[is private];
   @key[procedure] Assign_Employee(E: @key[in out] Employee; D: @key[in out] Department);
   @key[type] Dept_Ptr @key[is access all] Department;
   @key[function] Current_Department(E: Employee) @key[return] Dept_Ptr;
   @key[procedure] Choose_Manager(D:@key[ in out] Department; M: @key[in out] Employee);
@key[private]
   ...
@key[end] Workplace;
@end[Example]

Not only does this give rise to huge cumbersome packages but it also
prevents us from using the proper abstractions. Thus the types @exam[Employee]
and @exam[Department] have to be declared in the same private part
and so are not protected from each other's operations.

Ada 2005 solves this by introducing a variation of the with clause
@en the limited with clause. A limited with clause enables a library
unit to have an incomplete view of all the visible types in another
package. We can now write@Defn{limited with clause}@Defn2{Term=[with clause],Sec=[limited]}
@begin[Example]
@key[limited with] Departments;
@key[package] Employees @key[is]
   @key[type] Employee @key[is private];
   @key[procedure] Assign_Employee(E: @key[in out] Employee; D: @key[access] Departments.Department);
   @key[type] Dept_Ptr @key[is access all] Departments.Department;
   @key[function] Current_Department(E: Employee) @key[return] Dept_Ptr;
   ...
@key[end] Employees;

@key[limited with] Employees;
@key[package] Departments @key[is]
   @key[type] Department @key[is private];
   @key[procedure] Choose_Manager(D:@key[ in out] Department; M: @key[access] Employees.Employee);
   ...
@key[end] Departments;
@end[Example]

It is important to understand that a limited with clause does not
impose a dependence. Thus if a package @exam[A] has a limited with
clause for @exam[B], then @exam[A] does not depend on @exam[B] as
it would with a normal with clause, and so @exam[B] does not have
to be compiled before @exam[A] or placed into the library before @exam[A].

If we have a cycle of packages we only have to put @key[limited with]
on one package since that is sufficient to break the cycle of dependences.
However, for symmetry, in this example we have made them both have
a limited view of each other.

Note the terminology: we say that we have a limited view of a package
if the view is provided through a limited with clause. So a limited
view of a package provides an incomplete view of its visible types.
And by an incomplete view we mean as if they were incomplete types.@Defn{limited view}

In the example, because an incomplete view of a type cannot generally
be used as a parameter, we have had to change one parameter of each
of @exam[Assign_Employee] and @exam[Choose_Manager] to be an access
parameter.

Having broken the circularity we can then put normal with clauses
for each other on the two package bodies.

There are a number of rules necessary to avoid problems. A natural
one is that we cannot have both a limited with clause and a normal
with clause for the same package in the same context clause (a normal
with clause is now officially referred to as a nonlimited with clause).
An important and perhaps unexpected rule is that we cannot have a
use package clause with a limited view because severe surprises might
happen.@Defn{nonlimited with clause}

@leading@;To understand how this could be possible it is important to realise
that a limited with clause provides a very restricted view of a package.
It just makes visible
@begin[Itemize]
the name of the package and packages nested within,

an incomplete view of the types declared in the visible
parts of the packages.
@end{Itemize}

@leading@keepnext@;Nothing else is visible at all. Now consider
@begin[Example]
@tabset[P35]
@key[package] A @key[is]
   X: Integer := 99;
@key[end] A;

@key[package] B @key[is]
   X: Integer := 111;
@key[end] B;

@key[limited with] A, B;
@key[package] P @key[is]
   ...@\-- @examcom[neither X visible here]
@key[end] P;
@end[Example]

@leading@;Within package @exam[P] we cannot access @exam[A.X] or @exam[B.X]
because they are not types but objects. But we could declare a child
package with its own with clause thus
@begin[Example]
@key[with] A;
@key[package] P.C @key[is]
   Y: Integer := A.X;
@key[end] P.C;
@end[Example]

The nonlimited with clause on the child "overrides" the limited with
clause on the parent so that @exam[A.X ]is visible.

@leading@;Now suppose we were allowed to add a use package clause to the parent
package; since a use clause on a parent applies to a child this means
that we could refer to @exam[A.X] as just @exam[X] within the child
so we would have
@begin[Example]
@tabset[P35]
@key[limited with] A, B;
@key[use] A, B;@\-- @examcom[illegal]
@key[package] P @key[is]
   ...@\-- @examcom[neither X visible here]
@key[end] P;

@key[with] A;
@key[package] P.C @key[is]
   Y: Integer := X;@\-- @examcom[A.X now visible as just X]
@key[end] P.C;
@end[Example]

If we were now to change the with clause on the child to refer to
@exam[B] instead of @exam[A], then @exam[X] would refer to @exam[B.X]
rather than @exam[A.X]. This would not be at all obvious because the
use clause that permits this is on the parent and we are not changing
the context clause of the parent at all. This would clearly be unacceptable
and so use package clauses are forbidden if we only have a limited
view of the package.

@leading@;Here is a reasonably complete list of the rules designed to prevent
misadventure when using limited with clauses
@begin[Itemize]
@leading@;a use package clause cannot refer to a package with a
limited view as illustrated above,
@begin[Example]
@tabset[P35]
@key[limited with] P; @key[use] P;@\-- @examcom[illegal]
@key[package] Q @key[is] ...
@end[Example]

@leading@noprefix@;the rule also prevents
@begin[Example]
@tabset[P35]
@key[limited with] P;
@key[package] Q @key[is]
   @key[use] P;@\-- @examcom[illegal]
@end[Example]

@leading@;a limited with clause can only appear on a specification
@en it cannot appear on a body or a subunit,
@begin[Example]
@tabset[P35]
@key[limited with] P;@\-- @examcom[illegal]
@key[package body] Q @key[is ]...
@end[Example]

@leading@;a limited with clause and a nonlimited with clause for
the same package may not appear in the same context clause,
@begin[Example]
@tabset[P35]
@key[limited with] P; @key[with] P;@\-- @examcom[illegal]
@end[Example]

@leading@;a limited with clause and a use clause for the same package
or one of its children may not appear in the same context clause,
@begin[Example]
@tabset[P35]
@key[limited with] P; @key[use] P.C;@\-- @examcom[illegal]
@end[Example]

@leading@;a limited with clause may not appear in the context clause
applying to itself,
@begin[Example]
@tabset[P35]
@key[limited with] P;@\-- @examcom[illegal]
@key[package] P @key[is] ...
@end[Example]

@leading@;a limited with clause may not appear on a child unit
if a nonlimited with clause for the same package applies to its parent
or grandparent etc,
@begin[Example]
@tabset[P35]
@key[with] Q;
@key[package] P @key[is] ...

@key[limited with] Q;@\-- @examcom[illegal]
@key[package] P.C @key[is] ...
@end[Example]

@leading@noprefix@;but note that the reverse is allowed as mentioned above

@begin[Example]
@tabset[P35]
@key[limited with] Q;
@key[package] P @key[is] ...

@key[with] Q;@\-- @examcom[OK]
@key[package] P.C @key[is] ...
@end[Example]

@leading@;a limited with clause may not appear in the scope of
a use clause which names the unit or one of its children,

@begin[Example]
@tabset[P35]
@key[with] A;
@key[package] P @key[is]
   @key[package] R @key[renames] A;
@key[end] P;

@key[with] P;
@key[package] Q @key[is]
   @key[use] P.R;@\-- @examcom[applies to A]
@key[end] Q;

@key[limited with] A;@\-- @examcom[illegal]
@key[package] Q.C @key[is] ...
@end[Example]
@noprefix@;without this specific rule, the use clause in @exam[Q] which
actually refers to @exam[A] would clash with the limited with clause for
@exam[A].
@end[Itemize]

Finally note that a limited with clause can only refer to a package
declaration and not to a subprogram, generic declaration or instantiation,
or to a package renaming.

We will now return to the rules for incomplete types. As noted above
the rules for incomplete types are quite strict in Ada 95 and apart
from the curious case of an access to subprogram type it is not possible
to use an incomplete type for a parameter other than in an access
parameter.

@leading@;Ada 2005 enables some relaxation of these rules by introducing tagged
incomplete types. We can write@Defn{tagged incomplete type}
@begin[Example]
@key[type] T @key[is tagged];
@end[Example]

@leading@;and then the complete type must be a tagged type. Of course the
reverse does not hold. If we have just
@begin[Example]
@key[type] T;
@end[Example]

then the complete type @exam[T] might be tagged or not.

@leading@;A curious feature of Ada 95 was mentioned in the Introduction. In
Ada 95 we can write
@begin[Example]
@key[type] T;
...
@key[type] T_Ptr @key[is access all] T'Class;
@end[Example]

@leading@;By using the attribute @exam[Class], this promises in a rather sly
way that the complete type @exam[T] will be tagged. This is strictly
obsolescent in Ada 2005 and moved
to @URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-11.html],Text=[Annex J]}.
In Ada 2005 we should write
@begin[Example]
@key[type] T @key[is tagged];
...
@key[type] T_Ptr @key[is access all] T'Class;
@end[Example]

The big advantage of introducing tagged incomplete types is that we
know that tagged types are always passed by reference and so we are
allowed to use tagged incomplete types for parameters.

This advantage extends to the incomplete view obtained from a limited
with clause. If a type in a package is visibly tagged then the incomplete
view obtained is tagged incomplete and so the type can then be used
for parameters.

@leading@;Returning to the packages @exam[Employees] and @exam[Departments]
it probably makes sense to make both types tagged since it is likely
that the types @exam[Employee] and @exam[Department] form a hierarchy.
So we can write
@begin[Example]
@key[limited with] Departments;
@key[package] Employees @key[is]
   @key[type] Employee @key[is tagged private];
   @key[procedure] Assign_Employee(E: @key[in out] Employee; D: @key[in out] Departments.Department'Class);
   @key[type] Dept_Ptr @key[is] @key[access all] Departments.Department'Class;
   @key[function] Current_Department(E: Employee) @key[return] Dept_Ptr;
   ...
@key[end] Employees;

@key[limited with] Employees;
@key[package] Departments @key[is]
   @key[type] Department @key[is tagged private];
   @key[procedure] Choose_Manager(D:@key[ in out] Department; M: @key[in out] Employees.Employee'Class);
   ...
@key[end] Departments;
@end[Example]

@leading@;The text is a bit cumbersome now with @exam[Class] sprinkled liberally
around but we can introduce some subtypes in order to shorten the
names. We can also avoid the introduction of the type @exam[Dept_Ptr]
since we can use an anonymous access type for the function result
as mentioned in the previous paper (see @RefSecNum{Anonymous access types}).
So we get
@begin[Example]
@key[limited with] Departments;
@key[package] Employees @key[is]
   @key[type] Employee @key[is tagged private];
   @key[subtype] Dept @key[is] Departments.Department;
   @key[procedure] Assign_Employee(E: @key[in out] Employee; D: @key[in out] Dept'Class);
   @key[function] Current_Department(E: Employee) @key[return] @key[access] Dept'Class;
   ...
@key[end] Employees;

@key[limited with] Employees;
@key[package] Departments @key[is]
   @key[type] Department @key[is tagged private];
   @key[subtype] Empl @key[is] Employees.Employee;
   @key[procedure] Choose_Manager(D:@key[ in out] Department; M: @key[in out] Empl'Class);
   ...
@key[end] Departments;
@end[Example]

@leading@;Observe that in Ada 2005 we can use a simple subtype as an
abbreviation for an incomplete type thus
@begin[Example]
@key[subtype] Dept @key[is] Departments.Department;
@end[Example]

but such a subtype cannot have a constraint or a null exclusion. In
essence it is just a renaming. Remember that we cannot have a use
clause with a limited view. Moreover, many projects forbid use clauses
anyway but permit renamings and subtypes for local abbreviations.
It would be a pain if such abbreviations were not also available when
using a limited with clause.

@leading@;It's a pity we cannot also write
@begin[Example]
@key[subtype] A_Dept @key[is] Departments.Department'Class;
@end[Example]

but then you cannot have everything in life.

A similar situation arises with the names of nested packages. They
can be renamed in order to provide an abbreviation.

The mechanism for breaking cycles of dependences by introducing limited
with clauses does not mean that the implementation does not check
everything thoroughly in a rigorous Ada way. It is just that some
checks might have to be deferred. The details depend upon the implementation.

For the human reader it is very helpful that use clauses are not allowed
in conjunction with limited with clauses since it eliminates any doubt
about the location of types involved. It probably helps the poor compilers
as well.

Readers might be interested to know that this topic was one of the
most difficult to solve satisfactorily in the design of Ada 2005.
Altogether seven different versions of @AILink{AI=[AI95-00217-06],Text=[AI-217]}
were developed. This chosen solution is on reflection by far the best and was
in fact number 6.

A number of loopholes in Ada 95 regarding incomplete types are also
closed in Ada 2005.

@leading@;One such loophole is illustrated by the following (this is Ada 95)
@begin[Example]
@tabset[P35]
@key[package] P @key[is]
   ...
@key[private]
   @key[type] T;@\-- @examcom[an incomplete type]
   @key[type] ATC @key[is access all] T'Class;@\-- @examcom[it must be tagged]
   X: ATC;
   @key[procedure] Op(X: @key[access] T);@\-- @examcom[primitive operation]
   ...
@key[end] P;
@end[Example]

The incomplete type @exam[T] is declared in the private part of the
package @exam[P]. The access type @exam[ACT] is then declared and
since it is class wide this implies that the type @exam[T] must be
tagged (the reader will recall from the discussion above that this
odd feature is banished to
@URLLink{URL=[http://www.adaic.org/standards/05rm/html/RM-J-11.html],Text=[Annex J]} in Ada 2005).
The full type @exam[T]
is then declared in the body. We also declare a primitive operation
@exam[Op] of the type @exam[T] in the private part.

@leading@;However, before the body of @exam[P] is declared, nothing in Ada 95
prevents us from writing a private child thus
@begin[Example]
@tabset[P35]
@key[private package] P.C @key[is]
   @key[procedure] Naughty;
@key[end] P.C;

@key[package body] P.C @key[is]
   @key[procedure] Naughty @key[is]
   @key[begin]
      Op(X);@\-- @examcom[a dispatching call]
   @key[end] Naughty;
@key[end] P.C;
@end[Example]

and the procedure @exam[Naughty] can call the dispatching operation
@exam[Op]. The problem is that we are required to compile this call
before the type @exam[T] is completed and thus before the location
of its tag is known.

This problem is prevented in Ada 2005 by a rule that if an incomplete
type declared in a private part has primitive operations then the
completion cannot be deferred to the body.

@leading@;Similar problems arise with access to subprogram types. Thus, as
mentioned above, Ada 95 permits
@begin[Example]
@key[type] T;
@key[type] A @key[is access procedure ](X: @key[in out] T);
@end[Example]

In Ada 2005, the completion of @exam[T] cannot be deferred to a body.
Nor can we declare such an access to subprogram type if we only have
an incomplete view of @exam[T] arising from a limited with clause.

@leading@;Another change in Ada 2005 can be illustrated by the
@exam[Departments] and @exam[Employees] example. We can write
@begin[Example]
@key[limited with] Departments;
@key[package] Employees @key[is]
   @key[type] Employee @key[is tagged private];
   @key[procedure] Assign_Employee(E: @key[in out] Employee; D: @key[in out] Departments.Department'Class);
   @key[type] Dept_Ptr @key[is] @key[access all] Departments.Department'Class;
   ...
@key[end] Employees;

@key[with] Employees; @key[use] Employees;
@key[procedure] Recruit(D: Dept_Ptr; E: @key[in out] Employee) @key[is]
@key[begin]
   Assign_Employee(E, D.@key[all]);
@key[end] Recruit;
@end[Example]

Ada 95 has a rule that says "thou shalt not dereference an incomplete
type". This would prevent the call of @exam[Assign_Employee] which
is clearly harmless. It would be odd to require @exam[Recruit] to
have a nonlimited with clause for @exam[Departments] to allow the
call of @exam[Assign_Employee]. Accordingly the rule is changed in
Ada 2005 so that dereferencing an incomplete view is only forbidden
when used as a prefix as, for example, in @exam[D'Size].


@LabeledClause{Visibility from private parts}

@leading@;Ada 95 introduced public and private child packages in order to enable
subsystems to be decomposed in a structured manner. The general idea
is that
@begin{Itemize}
public children enable the decomposition of the view
of a subsystem to the user of the subsystem,

private children enable the decomposition of the implementation
of a subsystem.
@end[Itemize]

In turn both public and private children can themselves have children
of both kinds. This has proved to work well in most cases but a difficulty
has arisen regarding private parts.

@leading@;Recall that the private part of a package really concerns the
implementation of the package rather than specifying the facilities to the
external user. Although it does not concern algorithmic aspects of the
implementation it does concern the implementation of data abstraction. During
the original design of Ada some thought was given to the idea that a package
should truly be written and compiled as three distinct parts. Perhaps like this
@begin[Example]
@tabset[P35]
@key[with] ...
@key[package] P @key[is]
   ...@\-- @examcom[visible specification]
@key[end];

@key[with] ...
@key[package private] P @key[is]@\-- @examcom[just dreaming]
   ...@\-- @examcom[private part]
@key[end];

@key[with] ...
@key[package body] P is
   ...@\-- @examcom[body]
@key[end];
@end[Example]

Each part could even have had its own context clause as shown.

However, it was clear that this would be an administrative nightmare
in many situations and so the two-part specification and body emerged
with the private part lurking at the end of the visible part of the
specification (and sharing its context clause).

This was undoubtedly the right decision in general. The division into
just two parts supports separate compilation well and although the
private part is not part of the logical interface to the user it does
provide information about the physical interface and that is needed
by the compiler.

@leading@;The problem that has emerged is that the private part of a public
package cannot access the information in private child packages.@Defn{private child packages}
Private children are of course not visible to the user but there is no reason
why they should not be visible to the private part of a public package
provided that somehow the information does not leak out. Thus consider
a hierarchy
@begin[Example]
@key[package] App @key[is]
   ...
@key[private]
   ...
@key[end] App;

@key[package] App.Pub @key[is]
   ...
@key[private]
   ...
@key[end] App.Pub;

@key[private package] App.Priv @key[is]
   ...
@key[private]
   ...
@key[end] App.Priv;
@end[Example]

There is no reason why the private parts of @exam[App] and @exam[App.Pub]
and the visible part of the specification of @exam[App.Priv] should
not share visibility (the private part of @exam[App.Priv] logically
belongs to the next layer of secrecy downwards). But this sharing
is not possible in Ada 95.

The public package @exam[App.Pub] is not permitted to have a with
clause for the child package @exam[App.Priv] since this would mean
that the visible part of @exam[App.Pub] would also have visibility
of this information and by mechanisms such as renaming could pass
it on to the external user.

The specification of the parent package @exam[App] is also not permitted
to have a with clause for @exam[App.Priv] since this would break the
dependence rules anyway. Any child has a dependence on its parent
and so the parent specification has to be compiled or entered into
the program library first.

Note that the private part of the public child @exam[App.Pub] does
automatically have visibility of the private part of the parent @exam[App].
But the reverse cannot be true again because of the dependence rules.

Finally note that the private child @exam[App.Priv] can have a with
clause for its public sibling @exam[App.Pub] (it creates a dependence
of course) but that only gives the private child visibility of the
visible part of the public child.

So the only visibility sharing among the three regions in Ada 95 is
that the private part of the public child and the visible part of
the private child can see the private part of the parent.

The practical consequence of this is that in large systems, information
which should really be lower down the hierarchy has to be placed in
the private part of the ultimate parent. This tends to mean that the
parent package becomes very large thereby making maintenance more
difficult and forcing frequent recompilations of the parent and thus
the whole hierarchy of packages.

The situation is much alleviated in Ada 2005 by the introduction of
private with clauses.@Defn{private with clause}@Defn2{Term=[with clause],Sec=[private]}

@leading@;If a package @exam[P] has a private with clause for a package
@exam[Q] thus
@begin[Example]
@key[private with] Q;
@key[package] P @key[is] ...
@end[Example]

@leading@;then the private part of @exam[P] has visibility of the visible part
of the package @exam[Q], whereas the visible part of @exam[P] does
not have visibility of @exam[Q] and so visibility cannot be transmitted
to a user of @exam[P]. It is rather as if the with clause were attached
to just the private part of @exam[P] thus
@begin[Example]
@tabset[P35]
@key[package] P @key[is]
   ...
@key[with] Q;@\-- @examcom[we cannot write this]
@key[private]
   ...
@key[end] P;
@end[Example]

This echoes the three-part decomposition of a package discussed above.

A private with clause can be placed wherever a normal with clause
for the units mentioned can be placed and in addition a private with
clause which mentions a private unit can be placed on any of its parent's
descendants.

@leading@;So we can put a private with clause for @exam[App.Priv] on
@exam[App.Pub] thereby permitting visibility of the private child from the
private part of its public sibling. Thus
@begin[Example]
@tabset[P35]
@key[private with] App.Priv;
@key[package] App.Pub @key[is]
   ...@\-- @examcom[App.Priv not visible here]
@key[private]
   ...@\-- @examcom[App.Priv visible here]
@key[end] App.Pub;
@end[Example]

This works provided we don't run afoul of the dependence rules. The
private with clause means that the public child has a dependence on
the private child and therefore the private child must be compiled
or entered into the program library first.

@leading@;We might get a situation where there exists a mutual dependence
between the public and private sibling in that each has a type that the other
wants to access. In such a case we can use a limited private with
clause thus@Defn{limited private with clause}@Defn2{Term=[with clause],Sec=[limited private]}

@begin[Example]
@tabset[P35]
@key[limited private with] App.Priv;
@key[package] App.Pub @key[is]
   ...@\-- @examcom[App.Priv not visible here]
@key[private]
   ...@\-- @examcom[limited view of App.Priv here]
@key[end] App.Pub;
@end[Example]

@leading@;The child packages are both dependent on the parent package and so
the parent cannot have with clauses for them. But a parent can have
a limited with clause for a public child and a limited private with
clause for a private child thus
@begin[Example]
@tabset[P35]
@key[limited with] App.Pub; @key[limited private with] App.Priv;
@key[package] App @key[is]
   ...@\-- @examcom[limited view of App.Pub here]
@key[private]
   ...@\-- @examcom[limited view of App.Priv here]
@key[end] App;
@end[Example]

@leading@;A simple example of the use of private with clauses was given in the
Introduction. Here it is somewhat extended
@begin[Example]
@tabset[P35]
@key[limited with] App.User_View; @key[limited private with] App.Secret_Details;
@key[package] App @key[is]
   ...@\-- @examcom[limited view of type Outer visible here]
@key[private]
   ...@\-- @examcom[limited view of type Inner visible here]
@key[end] App;

@key[private package ]App.Secret_Details @key[is]
   @key[type] Inner @key[is] ...
   ... @\-- @examcom[various operations on Inner etc]
@key[end] App.Secret_Details;

@key[private with] App.Secret_Details;
@key[package] App.User_View @key[is]

   @key[type] Outer @key[is private];
   ...@\-- @examcom[various operations on Outer visible to the user]

@\-- @examcom[type Inner is not visible here]
@key[private]@\-- @examcom[type Inner is visible here]

    @key[type] Outer @key[is]
      @key[record]
         X: Secret_Details.Inner;
         ...
      @key[end record];
   ...
@key[end] App.User_View;
@end[Example]

In the previous section we observed that there were problems with
interactions between use clauses, nonlimited with clauses, and limited
with clauses. Those rules also apply to private with clauses where
a private with clause is treated as a nonlimited with clause and a
limited private with clause is treated as a limited with clause. In
other words private is ignored for the purpose of those rules.

@leading@;Moreover, we cannot place a package use clause in the same context
clause as a private with clause (limited or not). This is because
we would then expect it to apply to the visible part as well which
would be wrong. However, we can always put a use clause in the private
part thus
@begin[Example]
@tabset[P35]
@key[private with] Q;
@key[package] P @key[is]
   ...@\-- @examcom[Q not visible here]
@key[private]
   @key[use] Q;
   ...@\-- @examcom[use visibility of Q here]
@key[end] P;
@end[Example]

@leading@;At the risk of confusing the reader it might be worth pointing out
that strictly speaking the rules regarding private with are treated
as legality rules rather than visibility rules. Here is an example
which illustrates this subtlety and the dangers it avoids
@begin[Example]
@tabset[P35]
@key[package] P @key[is]
   @key[function] F @key[return] Integer;
@key[end] P;

@key[function] F @key[return] Integer;

@key[with] P;
@key[private with] F;
@key[package] Q @key[is]
   @key[use] P;
   X: Integer := F;@\-- @examcom[illegal]
   Y: Integer := P.F;@\-- @examcom[legal]
@key[private]
   Z: Integer := F;@\-- @examcom[legal, calls the library F]
@key[end] Q;
@end[Example]

If we treated the rules regarding private with as pure visibility
rules then the call of @exam[F] in the declaration of @exam[X] in
the visible part would be a call of @exam[P.F]. So moving the declaration
of @exam[X] to the private part would silently change the @exam[F]
being called @en this would be nasty. We can always write the call
of @exam[F] as @exam[P.F] as shown in the declaration of @exam[Y].

So the rules regarding private with are written to make entities visible
but unmentionable in the visible part. In practice programmers can
just treat them as visibility rules so that the entities are not visible
at all which is how we have described them above.

@leading@;A useful consequence of the unmentionable rather than invisible
approach is that we can use the name of a package mentioned in a private with
clause in a pragma in the context clause thus
@begin[Example]
@key[private with] P;  @key[pragma] Elaborate(P);
@key[package] Q @key[is] ...
@end[Example]

@leading@;Private with clauses are in fact allowed on bodies as well, in which
case they just behave as a normal with clause. Another minor point
is that Ada has always permitted several with clauses for the same
unit in one context clause thus
@begin[Example]
@key[with] P;  @key[with] P;  @key[with] P, P;
@key[package] Q is ...
@end[Example]

@leading@keepnext@;To avoid complexity we similarly allow
@begin[Example]
@key[with] P;  @key[private with] P;
@key[package] Q @key[is]
@end[Example]

and then the private with is ignored.

@leading@;We have introduced private with clauses in this section as the
solution to the problem of access to private children from the private part of
the parent or public sibling. But they have other important uses. If we have
@begin[Example]
@key[private with] P;
@key[package] Q @key[is] ...
@end[Example]

then we are assured that the package @exam[Q] cannot inadvertently
access @exam[P] in the visible part and, in particular, pass on access
to entities in @exam[P] by renamings and so on. Thus writing @key[private
with] provides additional documentation information which can be useful
to both human reviewers and program analysis tools. So if we have
a situation where a private with clause is all that is needed then
we should use it rather than a normal with clause.

@leading@;In summary, whereas in Ada 95 there is just one form of with clause,
Ada 2005 provides four forms
@begin[Example]
@tabset[P35]
@key[with] P;@\-- @examcom[full view]

@key[limited with] P;@\-- @examcom[limited view]

@key[private with] P;@\-- @examcom[full view from private part]

@key[limited private with] P;@\-- @examcom[limited view from private part]
@end[Example]

Finally, note that if a private with clause is given on a specification
then it applies to the body as well as to the private part.


@LabeledClause{Aggregates}

There are important changes to aggregates in Ada 2005 which are very
useful in a number of contexts. These were triggered by the changes
to the rules for limited types which are described in the next section,
but it is convenient to first consider aggregates separately.

The main change is that the box notation @exam[<>] is now permitted
as the value in a named aggregate. The meaning is that the component
of the aggregate takes the default value if there is one.@Defn2{Term=[default value],Sec=[in aggregate]}@Defn2{Term=[box],Sec=[in aggregate]}

@leading@keepnext@;So if we have a record type such as
@begin[Example]
@key[type] RT @key[is]
   @key[record]
      A: Integer := 7;
      B: @key[access] Integer;
      C: Float;
   @key[end record];
@end[Example]

@leading@keepnext@;then if we write
@begin[Example]
X: RT := (A => <>, B => <>, C => <>);
@end[Example]

then @exam[X.A] has the value @exam[7], @exam[X.B] has the value @key[null]
and @exam[X.C] is undefined. So the default value is that given in
the record type declaration or, in the absence of such an explicit
default value, it is the default value for the type. If there is no
explicit default value and the type does not have one either then
the value is simply undefined as usual.

@leading@keepnext@;The above example could be abbreviated to
@begin[Example]
X: RT := (@key[others] => <>);
@end[Example]

@leading@keepnext@;The obvious combinations are allowed
@begin[Example]
(A => <>, B => An_Integer'Access, C => 2.5)
(A => 3, @key[others] => <>)
(A => 3, B | C => <>)
@end[Example]

The last two are the same. There is a rule in Ada 95 that if several
record components in an aggregate are given the same expression using
a @exam[|] then they have to be of the same type. This does not apply
in the case of @exam[<>] because no typed expression is involved.

@leading@;The @exam[<>] notation is not permitted with positional notation.
So we cannot write
@begin[Example]
@tabset[P35]
(3, <>, 2.5)@\-- @examcom[illegal]
@end[Example]

@leading@;But we can mix named and positional notations in a record aggregate
as usual provided the named components follow the positional ones,
so the following are permitted
@begin[Example]
(3, B => <>, C => 2.5)
(3, @key[others] => <>)
@end[Example]

A minor but important rule is that we cannot use @exam[<>] for a component
of an aggregate that is a discriminant if it does not have a default.
Otherwise we could end up with an undefined discriminant.

@leading@;The @exam[<>] notation is also allowed with array aggregates. But
in this case the situation is much simpler because it is not possible
to give a default value for array components. Thus we might have
@begin[Example]
P: @key[array] (1.. 1000) @key[of] Integer := (1 => 2, @key[others] => <>);
@end[Example]

@leading@;The array @exam[P] has its first component set to @exam[2] and the
rest undefined. (Maybe @exam[P] is going to be used to hold the first
1000 prime numbers and we have a simple algorithm to generate them
which requires the first prime to be provided.) The aggregate could
also be written as
@begin[Example]
(2, @key[others] => <>)
@end[Example]

Remember that @key[others] is permitted with a positional array aggregate
provided it is at the end. But otherwise @exam[<>] is not allowed
with a positional array aggregate.

We can add @key[others] @exam[=> <>] even when there are no components
left. This applies to both arrays and records.

@leading@;The box notation is also useful with tasks and protected objects used
as components. Consider
@begin[Example]
@key[protected type] Semaphore @key[is] ... ;

@key[type] PT @key[is]
   @key[record]
      Guard: Semaphore;
      Count: Integer;
      Finished: Boolean := False;
   @key[end record];
@end[Example]

@leading@;As explained in the next section, we can now use an aggregate to
initialize an object of a limited type. Although we cannot give an explicit
initial value for a @exam[Semaphore] we would still like to use an aggregate to
get a coverage check. So we can write
@begin[Example]
X: PT := (Guard => <>, Count => 0, Finished => <>);
@end[Example]

@leading@;Note that although we can use @exam[<>] to stand for the value of
a component of a protected type in a record we cannot use it for a
protected object standing alone.
@begin[Example]
@tabset[P35]
Sema: Semaphore := <>;@\-- @examcom[illegal]
@end[Example]

The reason is that there is no need since we have no coverage check
to concern us and there could be no other reason for doing it anyway.

@leading@;Similarly we can use @exam[<>] with a component of a private type
as in
@begin[Example]
@key[type] Secret@key[ is private];

@key[type] Visible @key[is]
   @key[record]
      A: Integer;
      S: Secret;
   @key[end record];

X: Visible := (A => 77; S => <>);
@end[Example]

@leading@keepnext@;but not when standing alone
@begin[Example]
@tabset[P35]
S: Secret := <>;@\-- @examcom[illegal]
@end[Example]

It would not have any purpose because such a variable will take any
default value anyway.

@leading@;We conclude by mentioning a small point for the language lawyer.
Consider
@begin[Example]
@key[function] F @key[return] Integer;

@key[type] T @key[is]
   @key[record]
      A: Integer := F;
      B: Integer := 3;
   @key[end record];
@end[Example]

@leading@keepnext@;Writing
@begin[Example]
@tabset[P35]
X: T := (A => 5, @key[others] => <>);@\-- @examcom[does not call F]
@end[Example]

@leading@keepnext@;is not quite the same as
@begin[Example]
@tabset[P35]
X: T;@\-- @examcom[calls F]
...
X.A := 5;  X.B := 3;
@end[Example]

In the first case the function @exam[F] is not called whereas in the
second case it is called when @exam[X] is declared in order to default
initialize @exam[X.A]. If it had a nasty side effect then this could
matter. But then programmers should not use nasty side effects anyway.


@LabeledClause{Limited types and return statements}

The general idea of a limited type is to restrict the operations that
a user can do on the type to just those provided by the author of
the type and in particular to prevent the user from doing assignment
and thus making copies of objects of the type.

However, limited types have always been a problem. In Ada 83 the concept
of limitedness was confused with that of private types. Thus in Ada
83 we only had limited private types (although task types were inherently
limited).

Ada 95 brought significant improvement by two changes. It allowed
limitedness to be separated from privateness. It also allowed the
redefinition of equality for all types whereas Ada 83 forbade this
for limited types. In Ada 95, the key property of a limited type is
that assignment is not predefined and cannot be defined (equality
is not predefined either but it can be defined). The general idea
of course is that there are some types for which it would be wrong
for the user to be able to make copies of objects. This particularly
applies to types involved in resource control and types implemented
using access types.

However, although Ada 95 greatly improved the situation regarding
limited types, nevertheless two major difficulties have remained.
One concerns the initialization of objects and the other concerns
the results of functions.

The first problem is that Ada 95 treats initialization as a process
of assigning the initial value to the object concerned (hence the
use of @exam[:=] unlike some Algol based languages which use @exam[=]
for initialization and @exam[:=] for assignment). And since initialization
is treated as assignment it is forbidden for limited types. This means
that we cannot initialize objects of a limited type nor can we declare
constants  of a limited type. We cannot declare constants because
they have to be initialized and yet initialization is forbidden. This
is more annoying in Ada 95 since we can make a type limited but not
private.@Defn2{Term=[initialization],Sec=[of limited objects]}@Defn{limited object initialization}

@leading@;The following example was discussed in the Introduction
@begin[Example]
@key[type] T is @key[limited]
   @key[record]
      A: Integer;
      B: Boolean;
      C: Float;
   @key[end record];
@end[Example]

@leading@;Note that this type is explicitly limited (but not private) but its
components are not limited. If we declare an object of type @exam[T]
in Ada 95 then we have to initialize the components (by assigning
to them) individually thus
@begin[Example]
   X: T;
@key[begin]
   X.A := 10;  X.B := True;  X.C := 45.7;
@end[Example]

Not only is this annoying but it is prone to errors as well. If we
add a further component @exam[D] to the type @exam[T] then we might
forget to initialize it. One of the advantages of aggregates is that
we have to supply all the components which automatically provides
full coverage analysis.

This problem did not arise in Ada 83 because we could not make a type
limited without making it also private and so the individual components
were not visible anyway.

@leading@;Ada 2005 overcomes the difficulty by stating that initialization by
an aggregate is not actually assignment even though depicted by the
same symbol. This permits
@begin[Example]
   X: T := (A => 10,  B => True,  C => 45.7);
@end[Example]

We should think of the individual components as being initialized
individually @i[in situ] @en an actual aggregated value is not created
and then assigned.

The reader might recall that the same thing happens when an aggregate
is used to initialize a controlled type; this was not as Ada 95 was
originally defined but it was corrected in
@AILink{AI=[AI95-00083-01],Text=[AI-83]} and consolidated
in the 2001 Corrigendum @LocalLink{Target=[R2],Sec=[References],Text={[2]}}.

@leading@;We can now declare a constant of a limited type as expected@Defn2{Term=[constant],Sec=[of a limited type]}
@begin[Example]
   X: @key[constant] T := (A => 10,  B => True,  C => 45.7);
@end[Example]

@leading@;Limited aggregates can be used in a number of other contexts as well
@begin[Itemize]
as the default expression in a component declaration,

@leading@noprefix@;so if we nest the type @exam[T] inside some other type
(which itself then is always limited @en it could be explicitly limited but
there is a general rule that a type is implicitly limited if it has a limited
component) we might have
@begin[Example]
@key[type] Twrapper @key[is]
   @key[record]
      Tcomp: T := (0, False, 0.0);
   @key[end record];
@end[Example]

as an expression in a record aggregate,

@leading@keepnext@noprefix@;so again using the type @exam[Twrapper] as in

@begin[Example]
XT: Twrapper := (Tcomp => (1, True, 1.0));
@end[Example]

as an expression in an array aggregate similarly,

@leading@keepnext@noprefix@;so we might have
@begin[Example]
@key[type] Tarr @key[is array] (1 .. 5) @key[of] T;

Xarr: Tarr := (1 .. 5 => (2, True, 2.0));
@end[Example]

as the expression for the ancestor part of an extension aggregate,

@leading@keepnext@noprefix@;so if @exam[TT] were tagged as in
@begin[Example]
@key[type] TT@key[ is tagged limited]
   @key[record]
      A: Integer;
      B: Boolean;
      C: Float;
   @key[end record];

@key[type] TTplus@key[ is new] TT @key[with]
   @key[record]
      D: Integer;
   @key[end record];
...
XTT: TTplus := ((1, True, 1.0) @key[with] 2);
@end[Example]

as the expression in an initialized allocator,

@leading@keepnext@noprefix@;so we might have
@begin[Example]
@key[type] T_Ptr @key[is access] T;
XT_Ptr: T_Ptr;
...
XT_Ptr := @key[new] T'(3, False, 3.0);
@end[Example]

@leading@;as the actual parameter for a subprogram parameter of
a limited type of mode in
@begin[Example]
@key[procedure] P(X: @key[in] T);
...
P((4, True, 4.0));
@end[Example]

@leading@;similarly as the default expression for a parameter
@begin[Example]
@key[procedure] P(X: @key[in] T := (4, True, 4.0));
@end[Example]

@leading@;as the result in a return statement
@begin[Example]
@key[function] F( ... ) @key[return] T @key[is]
@key[begin]
   ...
   @key[return] (5, False, 5.0);
@key[end] F;
@end[Example]

@noprefix@;this really concerns the other major change to limited types which
we shall return to in a moment.

@leading@;as the actual parameter for a generic formal limited
object parameter of mode in,
@begin[Example]
@key[generic]
   FT: @key[in] T;
@key[package] P @key[is] ...
...
@key[package] Q @key[is new] P(FT => (7, True, 7.0));
@end[Example]
@end[Itemize]

@leading@;The last example is interesting. Limited generic parameters were not
allowed in Ada 95 at all because there was no way of passing an actual
parameter because the generic parameter mechanism for an in parameter
is considered to be assignment. But now the actual parameter can be
passed as an aggregate. An aggregate can also be used as a default
value for the parameter thus
@begin[Example]
@key[generic]
   FT: @key[in] T := (0, False, 0.0);
@key[package] P @key[is] ...
@end[Example]

Remember that there is a difference between subprogram and generic
parameters. Subprogram parameters were always allowed to be of limited
types since they are mostly implemented by reference and no copying
happens anyway. The only exception to this is with limited private
types where the full type is an elementary type.

The change in Ada 2005 is that an aggregate can be used as the actual
parameter in the case of a subprogram parameter of mode @key[in] whereas
that was not possible in Ada 95.

@leading@;Sometimes a limited type has components where an initial value cannot
be given as in
@begin[Example]
@key[protected type] Semaphore @key[is] ... ;

@key[type] PT @key[is]
   @key[record]
      Guard: Semaphore;
      Count: Integer;
      Finished: Boolean := False;
   @key[end record];
@end[Example]

@leading@;Since a protected type is inherently limited the type @exam[PT] is
also limited because a type with a limited component is itself limited.
Although we cannot give an explicit initial value for a @exam[Semaphore],
we would still like to use an aggregate to get the coverage check.
In such cases we can use the box symbol @exam[<>] as described in
the previous section to mean use the default value for the type (if
any). So we can write
@begin[Example]
X: PT := (Guard => <>, Count => 0, Finished => <>);
@end[Example]

@leading@;The major rule that must always be obeyed is that values of limited
types can never be copied. Consider nested limited types
@begin[Example]
@key[type] Inner @key[is limited]
   @key[record]
      L: Integer;
      M: Float;
   @key[end record];

@key[type] Outer @key[is limited]
   @key[record]
      X: Inner;
      Y: Integer;
@key[end record];
@end[Example]

@leading@keepnext@;If we declare an object of type @exam[Inner]
@begin[Example]
An_Inner: Inner := (L => 2, M => 2.0);
@end[Example]

@leading@keepnext@;then we could not use @exam[An_Inner] in an aggregate of
type @exam[Outer]
@begin[Example]
@tabset[P49]
An_Outer: Outer := (X => An_Inner, Y => 3); @\-- @examcom[illegal]
@end[Example]

@leading@;This is illegal because we would be copying the value. But we can
use a nested aggregate as mentioned earlier
@begin[Example]
An_Outer: Outer := (X => (2, 2.0), Y => 3);
@end[Example]

The other major change to limited types concerns returning values
from functions.

We have seen that the ability to initialize an object of a limited
type with an aggregate solves the problem of giving an initial value
to a limited type provided that the type is not private.

Ada 2005 introduces a new approach to returning the results from functions
which can be used to solve this and other problems.

@leading@keepnext@;We will first consider the case of a type that is limited
such as
@begin[Example]
@key[type] T @key[is] @key[limited]
   @key[record]
      A: Integer;
      B: Boolean;
      C: Float;
   @key[end record];
@end[Example]

@leading@;We can declare a function that returns a value of type @exam[T]
provided that the return does not involve any copying. For example we could
have
@begin[Example]
@key[function] Init(X: Integer; Y: Boolean; Z: Float) @key[return] T @key[is]
@key[begin]
   @key[return] (X, Y, Z);
@key[end] Init;
@end[Example]

@leading@;This function builds the aggregate in place in the return expression
and delivers it to the location specified where the function is called.
Such a function can be called from precisely those places listed above
where an aggregate can be used to build a limited value in place.
For example
@begin[Example]
V: T := Init(2, True, 3.0);
@end[Example]

So the function itself builds the value in the variable @exam[V] when
constructing the returned value. Hence the address of @exam[V] is
passed to the function as a sort of hidden parameter.

@leading@;Of course if @exam[T] is not private then this achieves no more than
simply writing
@begin[Example]
V: T := (2, True, 3.0);
@end[Example]

But the function @exam[Init] can be used even if the type is private.
It is in effect a constructor function for the type. Moreover, the
function @exam[Init] could be used to do some general calculation
with the parameters before delivering the final value and this brings
considerable flexibility.@Defn{constructor function}

@leading@;We noted that such a function can be called in all the places where
an aggregate can be used and this includes in a return expression
of a similar function or even itself
@begin[Example]
@key[function] Init_True(X: Integer; Z: Float) @key[return] T @key[is]
@key[begin]
   @key[return] Init(X, True, Z);
@key[end] Init_True;
@end[Example]

@leading@;It could also be used within an aggregate. Suppose we have a function
to return a value of the limited type @exam[Inner] thus
@begin[Example]
@key[function] Make_Inner(X: Integer; Y: Float) @key[return] Inner @key[is]
@key[begin]
   @key[return] (X, Y);
@key[end] Make_Inner;
@end[Example]

@leading@;then not only could we use it to initialize an object of type
@exam[Inner] but we could use it in a declaration of an object of type
@exam[Outer] thus
@begin[Example]
An_Inner: Inner := Make_Inner(2, 2.0);
An_Outer: Outer := (X => Make_Inner(2, 2.0), Y => 3);
@end[Example]

In the latter case the address of the component of @exam[An_Outer]
is passed as the hidden parameter to the function @exam[Make_Inner].

Being able to use a function in this way provides much flexibility
but sometimes even more flexibility is required. New syntax permits
the final returned object to be declared and then manipulated in a
general way before finally returning from the function. @Defn{extended return statement}

@leading@keepnext@;The basic structure is
@begin[Example]
@tabset[P35]
@key[function] Make( ... ) @key[return] T @key[is]
@key[begin]
   ...
   @key[return] R: T @key[do]@\-- @examcom[declare R to be returned]
      ...@\-- @examcom[here we can manipulate R in the usual way]
      ...@\-- @examcom[in a sequence of statements]
   @key[end return];
@key[end] Make;
@end[Example]

The general idea is that the object @exam[R] is declared and can then
be manipulated in an arbitrary way before being finally returned.
Note the use of the reserved word @key[do] to introduce the statements
in much the same way as in an accept statement. The sequence ends
with @key[end return] and at this point the function passes control
back to where it was called. Note that if the function had been called
in a construction such as the initialization of an object @exam[X]
of a limited type @exam[T] thus
@begin[Example]
X: T := Make( ... );
@end[Example]

then the variable @exam[R] inside the function is actually the variable
@exam[X] being initialized. In other words the address of @exam[X]
is passed as a hidden parameter to the function @exam[Make] in order
to create the space for @exam[R]. No copying is therefore ever performed.

@leading@keepnext@;The sequence of statements could have an exception handler
@begin[Example]
@tabset[P35]
   @key[return] R: T @key[do]
      ...  @\-- @examcom[statements]
   @key[exception]
      ...  @\-- @examcom[handlers]
   @key[end return];
@end[Example]

@leading@;If we need local variables within an extended return statement then
we can declare an inner block in the usual way
@begin[Example]
@tabset[P35]
   @key[return] R: T @key[do]
      @key[declare]
         ...@\-- @examcom[local declarations]
      @key[begin]
         ...@\-- @examcom[statements]
      @key[end];
   @key[end return];
@end[Example]

@leading@keepnext@;The declaration of @exam[R] could have an initial value
@begin[Example]
   @key[return] R: T := Init( ... ) @key[do]
      ...
   @key[end return];
@end[Example]

@leading@;Also, much as in an accept statement, the @key[do] ... @key[end
return] part can be omitted, so we simply get
@begin[Example]
   @key[return] R: T;
@end[Example]

@leading@keepnext@;or

@begin[Example]
   @key[return] R: T := Init( ... );
@end[Example]
which is handy if we just want to return the object with its default
or explicit initial value.

@leading@;Observe that extended return statements cannot be nested but could
have simple return statements inside
@begin[Example]
@tabset[P35]
   @key[return] R: T := Init( ... ) @key[do]
      @key[if] ... @key[then]
          ...
          @key[return];@\-- @examcom[result is R]
      @key[end if];
      ...
   @key[end return];
@end[Example]

Note that simple return statements inside an extended return statement
do not have an expression since the result returned is the object
@exam[R] declared in the extended return statement itself.

@leading@;Although extended return statements cannot be nested there
could nevertheless
be several in a function, perhaps in branches of an if statement or
case statement. This would be quite likely in the case of a type with
discriminants
@begin[Example]
@key[type] Person(Sex: Gender) @key[is] ... ;

@key[function] F( ... ) @key[return] Person @key[is]
@key[begin]
   @key[if] ... @key[then]
      @key[return] R: Person(Sex => Male) @key[do]
         ...
      @key[end return];
   @key[else]
      @key[return] R: Person(Sex => Female) @key[do]
         ...
      @key[end return];
   @key[end if];
@key[end] F;
@end[Example]

This also illustrates the important point that although we introduced
these extended return statements in the context of greater flexibility
for limited types they can be used with any types at all such as the
nonlimited type @exam[Person]. The mechanism of passing a hidden parameter
which is the address for the returned object of course only applies
to limited types. In the case of nonlimited types, the result is simply
delivered in the usual way.

We can also rename the result of a function call @en even if it is
limited.

@leading@;The result type of a function can be constrained or unconstrained
as in the case of the type @exam[Person] but the actual object delivered
must be of a definite subtype. For example suppose we have
@begin[Example]
@key[type] UA @key[is array] (Integer @key[range] <>) @key[of] Float;
@key[subtype] CA @key[is] UA(1 .. 10);
@end[Example]

Then the type @exam[UA] is unconstrained but the subtype @exam[CA]
is constrained. We can use both with extended return statements.

@leading@;In the constrained case the subtype in the extended return statement
has to statically match (typically it will be the same textually but
need not) thus
@begin[Example]
@tabset[P35]
@key[function] Make( ... ) @key[return] CA @key[is]
@key[begin]
   ...
   @key[return] R: UA(1 .. 10) @key[do@\]@\-- @examcom[statically matches]
      ...
   @key[end return];
@key[end] Make;
@end[Example]

@leading@;In the unconstrained case the result @exam[R] has to be constrained
either by its subtype or by its initial value. Thus
@begin[Example]
@key[function] Make( ... ) @key[return] UA @key[is]
@key[begin]
   ...
   @key[return] R: UA(1 .. N) @key[do]
      ...
   @key[end return];
@key[end] Make;
@end[Example]

@leading@keepnext@;or

@begin[Example]
@key[function] Make( ... ) @key[return] UA @key[is]
@key[begin]
   ...
   @key[return] R: UA := (1 .. N => 0.0) @key[do]
      ...
   @key[end return];
@key[end] Make;
@end[Example]

@leading@;The other important change to the result of functions which was
discussed in the previous paper (see @RefSecNum{Anonymous access types}) is
that the result type can be of an anonymous
access type. So we can write a function such as
@begin[Example]
@key[function] Mate_Of(A: @key[access] Animal'Class) @key[return access]  Animal'Class;
@end[Example]

The introduction of explicit access types for the result means that
Ada 2005 is able to dispense with the notion of returning by reference.@Defn{return by reference}

@leading@;This does, however, introduce a noticeable incompatibility between
Ada 95 and Ada 2005. We might for example have a pool of slave tasks
acting as servers. Individual slave tasks might be busy or idle. We
might have a manager task which allocates slave tasks to different
jobs. The manager might declare the tasks as an array
@begin[Example]
@tabset[P35]
Slaves: @key[array ](1 .. 10) @key[of] TT;@\-- @examcom[TT is some task type]
@end[Example]

@leading@keepnext@;and then have another array of properties of the tasks such
as
@begin[Example]
@key[type] Task_Data @key[is]
   @key[record]
      Active: Boolean := False;
      Job_Code: ... ;
   @key[end record];
Slave_Data: @key[array ](1 .. 10) @key[of] Task_Data;
@end[Example]

@leading@keepnext@;We now need a function to find an available slave. In Ada 95
we write
@begin[Example]
@tabset[P35]
@key[function] Get_Slave @key[return] TT @key[is]
@key[begin]
   ...@\-- @examcom[find index K of first idle slave]
   @key[return] Slaves(K);@\-- @examcom[in Ada 95, not in Ada 2005]
@key[end] Get_Slave;
@end[Example]

This is not permitted in Ada 2005. If the result type is limited (as
in this case) then the expression in the return statement has to be
an aggregate or function call and not an object such as @exam[Slaves(K)].

In Ada 2005 the function has to be rewritten to honestly return an
access value referring to the task type rather than invoking the mysterious
concept of returning by reference.

@leading@keepnext@;So we have to write
@begin[Example]
@tabset[P35]
@key[function] Get_Slave @key[return] @key[access] TT @key[is]
@key[begin]
   ...@\-- @examcom[find index K of first idle slave]
   @key[return] Slaves(K)'Access;@\-- @examcom[in Ada 2005]
@key[end ]Get_Slave;
@end[Example]

and all the calls of @exam[Get_Slave] have to be changed to correspond
as well.

This is perhaps the most serious incompatibility between Ada 95 and
Ada 2005. But then, at the end of the day, honesty is the best policy.


