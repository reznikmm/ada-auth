@Part(comp-rel, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/comp-rel.mss,v $}
@comment{$Revision: 1.3 $ $Date: 2006/10/19 22:29:17 $}


@LabeledSection{package Asis.Compilation_Units.Relations}

@Chg{Version=[1],New=[The library package @ChildUnit{Parent=[Asis.Compilation_Units],Child=[Relations]}Asis.Compilation_Units.Relations
shall exist. The package
shall provide interfaces equivalent to those described in the
following subclauses.],
Old=[@f{@key[package] @ChildUnit{Parent=[Asis.Compilation_Units],Child=[Relations]}Asis.Compilation_Units.Relations @key[is]}]}


Asis.Compilation_Units.Relations encapsulates semantic relationship
concepts used in ASIS.


@LabeledClause{type Relationship}

@i{Relationship}@Defn2{Term=[Relationship],Sec=[between Compilation Units]}
queries provide references to compilation units that are
related, in some specific fashion, to one or more given compilation units.
Compilation units located by these queries are returned as a set of
ordered lists.

@begin{DescribeCode}
@begin{Example}
@key[type] @AdaTypeDefn{Relationship} (Consistent_Length   : Asis.ASIS_Natural;
                   Inconsistent_Length : Asis.ASIS_Natural;
                   Missing_Length      : Asis.ASIS_Natural;
                   Circular_Length     : Asis.ASIS_Natural) @key[is]
   @key[record]
      Consistent   : Asis.Compilation_Unit_List (1 .. Consistent_Length);
      Inconsistent : Asis.Compilation_Unit_List (1 .. Inconsistent_Length);
      Missing      : Asis.Compilation_Unit_List (1 .. Missing_Length);
      Circular     : Asis.Compilation_Unit_List (1 .. Circular_Length);
   @key[end record];
@end{Example}
@end{DescribeCode}

The following describes the semantics of the unit lists returned by the
queries Semantic_Dependence_Order and Elaboration_Order:

@leading@;Each query returns a set of four lists. Every unit returned will have the
same Enclosing_Context. The lists are:
@begin{Itemize}
@b{Consistent}: A list of consistent ordered units.

@b{Inconsistent}: A list of units that are inconsistent with one or more units on
which they semantically depend.

@b{Missing}: A list of units that have missing (nonexistent) related units.

@b{Circular}: A list of circular semantic dependencies between units.
@end{Itemize}

@leading@keepnext@;These lists are further described as:

@begin{Itemize}
@leading@keepnext@;Consistent units list:
@begin{InnerItemize}
   @noprefix@;The semantics for the ordering of units in the first list are
   defined by the individual queries.

   @noprefix@;Every unit in this list is unique. No duplicates are returned; no
   two units in the list are Is_Equal or Is_Identical.
@end{InnerItemize}

@leading@keepnext@;Inconsistent units list:
@begin{InnerItemize}
   @noprefix@;The second list is made up of unit pairs.

   @noprefix@;Each pairing defines an inconsistent semantic dependence relationship.
   The right unit of each pair semantically depends on the preceding left
   unit. All rightmost units of each pair are always inconsistent, and will
   not appear in the consistent units list. The leftmost unit can be either
   consistent or inconsistent. If a leftmost units is consistent, then it
   also appears in the consistent units list; otherwise the unit is part of
   an inconsistent transitive relationship.

   @noprefix@;The unit pairs are ordered such that there are no forward semantic
   dependencies between the inconsistent units. Each inconsistent unit's
   supporters always precede it in the list.

   @noprefix@;As an example, given four units, A withs B, B withs C, and C withs D;
   if D is replaced, the inconsistent list contains six units with the
   three pairs:
@begin{Example}
             DC  CB  BA
@end{Example}

   @noprefix@;The list indicates that units C, B, and A are inconsistent (the rightmost
   units of each pair). Semantic dependencies such as B depends on C
   also are indicated. The units C, B, and A are in an order that could be
   submitted to the compiler (a possible recompilation order).

   @noprefix@;If a unit is inconsistent because the source for the unit has been
   edited (or otherwise been made inconsistent by some action of the user
   or implementation) then the unit references Nil_Compilation_Unit as the
   cause of the inconsistency (e.g., (Nil A Nil B) is a list of two
   inconsistent units, neither of which can point to a third unit as the
   cause for their being inconsistent).
@end{InnerItemize}

@begin{ImplPerm}
@begin{InnerItemize}
@noprefix@;An implementation is allowed to use Nil_Compilation_Unit value for
the first unit of each pair if it cannot determine the supporting unit
causing the inconsistent semantic dependence.

@leading@keepnext@noprefix@;For the above example, the list returned is:

@begin{Example}
            DC DB DA CB CA BA
@end{Example}

@leading@keepnext@noprefix@;This list reports all dependencies:

@begin{Example}
            D withed by C withed by B withed by A => DC DB DA
                        C withed by B withed by A => CB CA
                                    B withed by A => BA
@end{Example}
@end{InnerItemize}
@end{ImplPerm}

@leading@keepnext@;Missing dependence list:
@begin{InnerItemize}
   @noprefix@;The third list is made up of unit pairs.  Each pairing consists of a
   unit followed by a missing related unit needed by the first unit.
   A missing unit is a required Compilation_Unit, with a known name, with a
   Unit_Kind that is either A_Nonexistent_Declaration or A_Nonexistent_Body.

   @leading@keepnext@noprefix@;For example:

   @noprefix@;Given a list containing the units:  AB AC

@begin{Example}
            If Unit_Kind(B) = A_Nonexistent_Declaration and
               Unit_Kind(C) = A_Nonexistent_Body then

            It can be deduced that:
               A is missing a needed supporter B (A depends semantically on B).
               A is missing a needed related unit body C (depending on
               the kind for A, C can be A's required body or some subunit of A).
@end{Example}

   @noprefix@;A unit is reported as missing only if the Post-Compilation Rules of Ada
   determine it to be needed. Reference Manual 10.2.
@end{InnerItemize}

@leading@keepnext@;Circular dependence list:
@begin{InnerItemize}

   @noprefix@;Circular dependencies between compilation units are provided in the
   fourth list. There may be more than one set of circular dependencies.
   The ordering of distinct sets in the list is implementation-defined.
   This list will never contain nonexistent units.

   @noprefix@;The list is made up of unit pairs. The second unit of each pair depends
   semantically on the first unit. A circularity is established when the
   first unit of a pair also appears as the second unit of a later pair.
   (See the unit A in the example below; it is the first unit of the first
   pair and is the second unit of the third pair). The next set of circular
   dependent units, if any, starts with the next unit in the list (the unit
   D in the example below).

   @noprefix@;For example:

   @noprefix@;Given a list containing the units:  AC CB BA DG GF FE ED

@begin{Example}
           It can be determined that there are two sets of circularly
           dependent units:
               {A, B, C} and {D, E, F, G}

           The dependencies are:  A depends on B, B depends on C, C depends on A.
                  D depends on E, E depends on F, F depends on G, G depends on D.
@end{Example}

   @noprefix@;Each circle of dependence is reported exactly once. It is not reported
   once for each unit in the circle.
@end{InnerItemize}
@end{Itemize}


@LabeledClause{constant Nil_Relationship}


@begin{DescribeCode}
@begin{Example}
@AdaObjDefn{Nil_Relationship} : @key[constant] Relationship :=
        (Consistent_Length   => 0,
         Inconsistent_Length => 0,
         Missing_Length      => 0,
         Circular_Length     => 0,
         Consistent          => Asis.Nil_Compilation_Unit_List,
         Inconsistent        => Asis.Nil_Compilation_Unit_List,
         Missing             => Asis.Nil_Compilation_Unit_List,
         Circular            => Asis.Nil_Compilation_Unit_List);
@end{Example}
@end{DescribeCode}


@LabeledClause{function Semantic_Dependence_Order}

Semantic Dependence Relationships @em Reference Manual 10.1.1(24).
laboration Dependence Relationships @em Reference Manual 10.1.1(25).

@begin{Notes}
To properly determine unit consistency, use one of the two semantic
dependence queries: Elaboration_Order or Semantic_Dependence_Order.
These queries return a value of the type Relationship, which contains
lists of consistent, inconsistent, missing and circular units.

@noprefix@;For these two queries, the existence of units in one or more of the
inconsistent, missing, or circular units list means that the consistent
unit list may not be complete.

@noprefix@;Applications that do not check for inconsistent, missing, or circular
units before using the consistent list might not operate as expected.
@end{Notes}

@begin{DescribeCode}
@begin{Example}
  @key[function] @AdaSubDefn{Semantic_Dependence_Order}
                (Compilation_Units : @key[in] Asis.Compilation_Unit_List;
                 Dependent_Units   : @key[in] Asis.Compilation_Unit_List;
                 The_Context       : @key[in] Asis.Context;
                 Relation          : @key[in] Asis.Relation_Kinds)
                @key[return] Relationship;
@end{Example}

Compilation_Units @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} a list of pertinent units.
Dependent_Units @Chg{Version=[1],New=[specifies],Old=[    @en Specifies]} dependents used to limit the query.
The_Context @Chg{Version=[1],New=[specifies],Old=[        @en Specifies]} a program Context for context.
Relation @Chg{Version=[1],New=[specifies],Old=[           @en Specifies]} the relationship to query.

Produces a Relationship value containing compilation_unit elements related to the
given Compilation_Units by the specified relation.

The compilation_unit elements in the consistent units list are ordered such that
there are no forward semantic dependencies.

Dependent_Units are ignored unless the Relation is Descendants or
Dependents. The union of units in the needed units of the Dependent_Units
list provide a limiting context for the query. Only members of these
needed units are present in the result.

If the Dependent_Units list is Is_Nil, the context for the search is the
entire Context. The result of such a query is the full (unlimited)
list of Dependents for the Compilation_Units.


-- All units in the result will have an Enclosing_Context value that
Is_Identical to The_Context.

@leading@keepnext@;Appropriate Unit_Kinds:
@begin{Display}
A_Procedure
A_Function
A_Package
A_Generic_Procedure
A_Generic_Function
A_Generic_Package
A_Procedure_Instance
A_Function_Instance
A_Package_Instance
A_Procedure_Renaming
A_Function_Renaming
A_Package_Renaming
A_Generic_Procedure_Renaming
A_Generic_Function_Renaming
A_Generic_Package_Renaming
A_Procedure_Body
A_Function_Body
A_Package_Body
A_Procedure_Body_Subunit
A_Function_Body_Subunit
A_Package_Body_Subunit
A_Task_Body_Subunit
A_Protected_Body_Subunit
An_Unknown_Unit            -- See @ImplPermTitle
@end{Display}

The Semantic_Dependence_Order query should never raise an exception
when processing inconsistent unit (sub)sets. This query is the only
means for an application to know if a given unit is consistent with
(some of) its supporters (dependents), and therefore the related
semantic processing can give valuable results for this unit.
@end{DescribeCode}

@begin{ImplPerm}
The handling of An_Unknown_Unit is implementation specific. It can be
possible to obtain Semantic Dependence Relationships when starting
with a list containing one or more units that are An_Unknown_Unit.
However, results may vary across ASIS implementations.
@end{ImplPerm}

@begin{Notes}
Semantic_Dependence_Order defines consistent units to be ordered such
that there are no forward semantic dependencies.
@end{Notes}


@LabeledClause{function Elaboration_Order}


@begin{DescribeCode}
@begin{Example}
@key[function] @AdaSubDefn{Elaboration_Order}
            (Compilation_Units : @key[in] Asis.Compilation_Unit_List;
             The_Context       : @key[in] Asis.Context)
            @key[return] Relationship;
@end{Example}

Compilation_Units @Chg{Version=[1],New=[specifies],Old=[  @en Specifies]} a list of units to elaborate.
The_Context @Chg{Version=[1],New=[specifies],Old=[        @en Specifies]} a program Context for context.

Produces, in elaboration order, a Relationship value containing compilation
units required to elaborate the given compilation units.

The return value contains the set of ordered lists described above for the
queries on Semantic Dependence Relationships. If the inconsistent,
missing, and circular lists are empty, the consistent list will contain
all units required to elaborate the arguments.

@begin{ImplPerm}
@begin{InnerItemize}@Comment{Only way to get indenting and justification}
@noprefix@;The Relationship value may include any number of implementation-specific
runtime support packages.
@end{InnerItemize}
@end{ImplPerm}

The first unit in the Consistent units list will always be the
specification for package Standard. The list will contain all units
required to elaborate the arguments.

Use the Context_Clause_Elements query to get pragma Elaborate elements
for a compilation unit.

@leading@keepnext@;Appropriate Unit_Kinds:
@begin{Display}
A_Procedure
A_Function
A_Package
A_Generic_Procedure
A_Generic_Function
A_Generic_Package
A_Procedure_Instance
A_Function_Instance
A_Package_Instance
A_Procedure_Renaming
A_Function_Renaming
A_Package_Renaming
A_Generic_Procedure_Renaming
A_Generic_Function_Renaming
A_Generic_Package_Renaming
A_Procedure_Body
A_Function_Body
A_Package_Body
A_Procedure_Body_Subunit
A_Function_Body_Subunit
A_Package_Body_Subunit
A_Task_Body_Subunit
A_Protected_Body_Subunit
An_Unknown_Unit            -- See @ImplPermTitle
@end{Display}
@end{DescribeCode}

@begin{ImplPerm}
The handling of An_Unknown_Unit is implementation specific. It can
be possible to obtain Semantic Dependence Relationships when starting
with a list containing one or more units that are An_Unknown_Unit.
However, results may vary across ASIS implementations.
@end{ImplPerm}

@begin{Example}
@ChgDeleted{Version=[1],Text=[@key[end] Asis.Compilation_Units.Relations;]}
@end{Example}

