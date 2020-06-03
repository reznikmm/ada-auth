@Part(04, Root="ada.mss")

@Comment{$Date: 2020/06/03 00:09:00 $}

@Comment{$Source: e:\\cvsroot/ARM/Source/04b.mss,v $}
@Comment{$Revision: 1.82 $}

@LabeledClause{Type Conversions}

@begin{Intro}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Redundant[Explicit type conversions, both
value conversions and view conversions, are
allowed between closely related
types as defined below. This @Chg{Version=[3],New=[subclause],Old=[clause]}
also defines rules for value and view
conversions to a particular subtype of a type,
both explicit ones and those implicit in other constructs.
@IndexSee{Term=[subtype conversion],See=(type conversion)}
@Defn{type conversion}
@Defn{conversion}
@IndexSee{Term=[cast],See=(type conversion)}]
@IndexSeeAlso{Term=[subtype conversion],See=(implicit subtype conversion)}
@IndexSee{Term=[type conversion, implicit],See=(implicit subtype conversion)}
@end{Intro}

@begin{Syntax}
@Syn{lhs=<type_conversion>,rhs="
    @Syn2{subtype_mark}(@Syn2{expression})
  | @Syn2{subtype_mark}(@Syn2{name})"}

@end{Syntax}

@begin{Intro}
@Defn2{Term=[target subtype], Sec=(of a @nt<type_conversion>)}
The @i(target subtype) of a @nt<type_conversion> is the subtype
denoted by the @nt{subtype_mark}.
@Defn2{Term=[operand], Sec=(of a @nt<type_conversion>)}
The @i(operand) of a @nt<type_conversion> is the
@nt{expression} or @nt{name} within the parentheses;
@Defn2{Term=[operand type], Sec=(of a @nt<type_conversion>)}
its type is the @i(operand type).

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
@Defn{convertible}
One type is @i(convertible) to a second type if a @nt<type_conversion>
with the first type as operand type and the second type as target type
is legal according to the rules of this @Chg{Version=[3],New=[subclause],Old=[clause]}.
Two types are convertible if each is convertible to the other.
@begin{Ramification}
  Note that @lquotes@;convertible@rquotes@; is defined in terms of legality
  of the conversion. Whether the conversion would raise an exception
  at run time is irrelevant to this definition.
@end{Ramification}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0017],ARef=[AI95-00184-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00330-01]}
@Defn{view conversion}
@Defn2{Term=[conversion],Sec=(view)}
A @nt{type_conversion} whose operand is the
@nt<name> of an object is called a @i(view conversion) if
@Chg{New=[both ],Old=[]}its target type
@Chg{New=[and operand type are],Old=[is]} tagged, or if it
appears@Chg{Version=[2],New=[ in a call],Old=[]} as an actual parameter of mode
@key[out] or @key[in out];
@Defn{value conversion}
@Defn2{Term=[conversion],Sec=(value)}
other @nt<type_conversion>s are called @i(value conversions).
@IndexSee{Term=[super],See=(view conversion)}
@begin{Ramification}
  A view conversion to a tagged type can appear in
  any context that requires an object @nt<name>, including in
  an object renaming, the @nt<prefix> of a @nt<selected_component>,
  and if the operand is a variable, on the left side of an
  @nt<assignment_statement>. View conversions to other types only
  occur as actual parameters. Allowing view conversions of untagged
  types in all contexts seemed to incur an undue implementation burden.

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00330-01]}
  @ChgAdded{Version=[2],Text=[A type conversion appearing as an @key{in out}
  parameter in a generic instantiation is not a view conversion; the second
  part of the rule only applies to subprogram calls, not instantiations.]}
@end{Ramification}
@end{Intro}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(type_conversion operand)}
The operand of a @nt<type_conversion> is expected to be of any type.
@begin{Discussion}
  This replaces the "must be determinable" wording of Ada 83.
  This is equivalent to (but hopefully more intuitive than) saying
  that the operand of a @nt<type_conversion>
  is a @lquotes@;complete context.@rquotes@;
@end{Discussion}

The operand of a view conversion
is interpreted only as a @nt<name>;
the operand of a value conversion
is interpreted as an @nt<expression>.
@begin{Reason}
  @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0005-1]}
  @ChgRef{Version=[5],Kind=[Revised]}@ChgNote{This is also in AI12-0005-1, don't want a duplicate reference}
  This formally resolves the syntactic ambiguity between
  the two forms of @nt<type_conversion>@Chg{Version=[4],New=[.
  This matters @Chg{Version=[5],New=[because],Old=[as]} an @nt{expression} that
  is a @nt{name} is evaluated and represents a value while a @nt{name} by
  itself can be an object; we want
  a view conversion to be an object],Old=[, not that it really matters]}.
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0005-1]}
  @ChgAdded{Version=[4],Text=[This wording uses "interpreted as" rather than
  "shall be" so that this rule is not used to resolve overloading; it is
  solely about evaluation as described above.]}
@end{Ramification}

@end{Resolution}

@begin{Legality}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
@Chg{Version=[2],New=[In a view conversion for an untagged type,
the target type shall be convertible (back) to the operand type.],
Old=[@Defn2{Term=[type conversion],sec=(numeric)}
@Defn2{Term=[conversion],sec=(numeric)}
If the target type is a numeric type, then the operand type
shall be a numeric type.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=<Untagged view conversions appear only as [@key{in}]
  @key{out}
  parameters. Hence, the reverse conversion must be legal as well.
  The forward conversion must be legal even for an @key{out} parameter,
  because (for example) actual parameters of an access type are always
  copied in anyway.>}
@end{Reason}
@begin{NotIso}
@ChgAdded{Version=[2],Noprefix=[T],Noparanum=[T],Text=[@Shrink{@i<Paragraphs 9
through 20 were reorganized and moved below.>}]}@Comment{This message should be
deleted if the paragraphs are ever renumbered.}
@end{NotIso}
@begin{Discussion}
@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[The entire @LegalityTitle section has been
reorganized to eliminate an
unintentional incompatibility with Ada 83. In rare cases, a type conversion
between two types related by derivation is not allowed by Ada 95, while it is
allowed in Ada 83. The reorganization fixes this.
Much of the wording of the legality section is unchanged, but it is reordered
and reformatted. Because of the limitations of our tools, we had to delete and
replace nearly the entire section. The text of Ada 95 paragraphs 8 through 12,
14, 15, 17, 19, 20, and 24 are unchanged (just moved); these are now
24.1 through 24.5, 24.12, 24.13, 24.17, 24.19, 24.20, and 8.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[@Defn2{Term=[type conversion],sec=(array)}
@Defn2{Term=[conversion],sec=(array)}
If the target type is an array type, then the operand type shall
be an array type. Further:]}
@begin(itemize)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[The types shall have the same dimensionality;]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0008],ARef=[AI95-00168-01]}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[Corresponding index types shall be convertible;@Chg{New=[],Old=[ and]}
  @PDefn2{Term=[convertible],Sec=(required)}]}

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0008],ARef=[AI95-00168-01]}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[The component subtypes shall statically match@Chg{New=[; and],Old=[.]}
  @PDefn2{Term=[statically matching],Sec=(required)}]}

@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0008],ARef=[AI95-00168-01]}
@ChgRef{Version=[2],Kind=[DeletedAddedNoDelMsg],ARef=[AI95-00251-01]}
@ChgDeleted{Version=[2],Text=[@Chg{New=[In a view conversion, the target type and the operand type shall
both or neither have aliased components.],Old=[]}]}
@begin{Reason}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[DeletedAddedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[@Chg{New=[Without this rule, it is possible to violate the constrained status
of aliased array components. Consider:],Old=[]}]}
@begin{Example}
@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[DeletedAddedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[@Chg{New=[@key[package] P @key[is]
   @key[type] T @key[is private];
   A : @key[constant] T;
   @key[type] A1 @key[is array] (1 .. 10) @key[of aliased] T;
   @key[type] A2 @key[is array] (1 .. 10) @key[of] T;
@key[private]
   @key[type] T (D : Integer := 0) @key[is null record];
   A : @key[constant] T := (D => 1);
@key[end] P;],Old=[]}]}

@ChgRef{Version=[1],Kind=[Added]}
@ChgRef{Version=[2],Kind=[DeletedAddedNoDelMsg]}
@ChgDeleted{Version=[2],Text=[@Chg{New=[@key[with] P;
@key[procedure] Exam @key[is]
   X : P.A1;
   @key[procedure] S (Y : @key[in out] P.A2) @key[is]
   @key[begin]
      Y (1) := P.A;
   @key[end];
@key[begin]
   S (P.A2 (X)); -- This call will change the discriminant of X (1),
                 -- so we cannot allow the conversion.
@key[end];],Old=[]}]}
@end{Example}
@end{Reason}
@end(itemize)

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[@Defn2{Term=[type conversion],sec=(access)}
@Defn2{Term=[conversion],sec=(access)}
If the target type is a general access type, then the operand type
shall be an access-to-object type. Further:]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[The @LegalityTitle and @RunTimeTitle are worded
  so that a @nt{type_conversion} T(X) (where T is an access type) is (almost)
  equivalent to the @nt{attribute_reference} X.@key[all]'Access, where the
  result is of type T. The @nt{type_conversion} accepts a null value, whereas
  the @nt{attribute_reference} would raise Constraint_Error.]}
@end{Discussion}
@begin(itemize)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[If the target type is an access-to-variable
  type, then the operand type shall be an access-to-variable type;]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[If the target type is an access-to-constant
  type, then the operand type
  can be access-to-constant or access-to-variable.]}
@end{Ramification}

  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[If the target designated type is tagged,
  then the operand designated type
  shall be convertible to the target designated type;
  @PDefn2{Term=[convertible],Sec=(required)}]}

  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[If the target designated type is not tagged,
  then the designated types shall be the same,
  and either the designated subtypes shall statically match or
  the target designated subtype shall be discriminated and unconstrained; and
  @PDefn2{Term=[statically matching],Sec=(required)}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[These rules are designed to ensure that
  aliased array objects only @i(need) "dope" if their nominal subtype is unconstrained,
  but they can always @i(have) dope if required by the run-time model
  (since no sliding is permitted as part of access type conversion).
  By contrast, aliased discriminated objects will always @i(need) their
  discriminants stored with them, even if nominally constrained.
  (Here, we are assuming an implementation that represents
  an access value as a single pointer.)]}
@end{Reason}

  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[@PDefn2{Term=[accessibility rule],Sec=(type conversion)}
  The accessibility level of the operand type shall not be statically
  deeper than that of the target type.
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.]}
  @begin{Ramification}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[The access parameter case is handled by a runtime check.
  Runtime checks are also done in instance bodies.]}
  @end{Ramification}
@end(itemize)

@ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
@ChgDeleted{Version=[2],Type=[Leading],Text=[@Defn2{Term=[type conversion],sec=(access)}
@Defn2{Term=[conversion],sec=(access)}
If the target type is an access-to-subprogram type, then the operand
type shall be an access-to-subprogram type. Further:]}
@begin(itemize)
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[The designated profiles shall be
  subtype-conformant.@Defn2{Term=[subtype conformance],Sec=(required)}]}

  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00251-01]}
  @ChgDeleted{Version=[2],Text=[@PDefn2{Term=[accessibility rule],Sec=(type conversion)}
  The accessibility level of the operand type shall not be statically
  deeper than that of the target type.
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.
  If the operand type is declared within a generic body,
  the target type shall be declared within the generic body.]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
  @ChgDeleted{Version=[2],Text=[The reason it is illegal to convert from
  an access-to-subprogram type
  declared in a generic body to one declared outside that body
  is that in an implementation that shares generic bodies,
  procedures declared inside the generic need to have a different
  calling convention @em they need an extra parameter pointing to the
  data declared in the current instance.
  For procedures declared in the spec,
  that's OK, because the compiler can know about them at compile time of
  the instantiation.]}
@end{Reason}
@end(itemize)

@Comment{We start the new text here, so we can modify the handful of rules
that are not reformatted. (Except the first rule is at the top.)}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0115-1]}
@Leading@Defn2{Term=[type conversion],sec=[composite (non-array)]}
@Defn2{Term=[conversion],sec=[composite (non-array)]}
@Chg{Version=[2],New=[If there is a type@Chg{Version=[3],New=[ (other than a
root numeric type)],Old=[]} that is an ancestor of both the target
type and the operand type, or both types are class-wide types, then at least
one of the following rules shall apply:],Old=[@Defn2{Term=[type conversion],sec=(enumeration)}
@Defn2{Term=[conversion],sec=(enumeration)}
If the target type is not included in any of the above four
cases, there shall be a type that is an ancestor of both
the target type and the operand type.
Further, if the target type is tagged, then either:]}
@begin(itemize)
  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[@Defn2{Term=[type conversion],sec=(enumeration)}
  @Defn2{Term=[conversion],sec=(enumeration)}The target type shall be
  untagged; or]}

  The operand type shall be covered by or
  descended from the target type; or
  @begin{Ramification}
    This is a conversion
    toward the root, which is always safe.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
  The operand type shall be a class-wide type that covers the target
  type@Chg{Version=[2],New=[; or],Old=[.]}
  @begin{Ramification}
    This is a conversion of a class-wide type toward the leaves,
    which requires a tag check. See @RunTimeTitle.

    @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
    These two rules imply that a conversion from @Chg{Version=[2],New=[an
    ancestor],Old=[a parent]} type
    to a type extension is not permitted, as this would require
    specifying the values for additional components, in general,
    and changing the tag. An @nt<extension_aggregate> has to be used
    instead, constructing a new value, rather than converting an
    existing value. However, a conversion
    from the class-wide type rooted at @Chg{Version=[2],New=[an
    ancestor],Old=[the parent]} type is permitted;
    such a conversion just verifies that the operand's tag is
    a descendant of the target.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[The operand and target types shall both be class-wide
  types and the specific type associated with at least one of them shall be an
  interface type.]}
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[We allow converting any class-wide type T'Class to
    or from a class-wide interface type even if the specific type T does not
    have an appropriate
    interface ancestor, because some extension of T might have the needed
    ancestor. This is similar to a conversion of a class-wide type toward the
    leaves of the tree, and we need to be consistent. Of course, there is
    a runtime check that the actual object has the needed interface.]}
  @end{Ramification}
@end(itemize)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00251-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0115-1]}
@Chg{Version=[2],New=[If there is no type@Chg{Version=[3],New=[ (other than a
root numeric type)],Old=[]} that is the ancestor of both the
target type and the operand type, and they are not both class-wide types, one
of the following rules shall apply:], Old=[In a view conversion for
an untagged type, the target type shall be convertible (back) to the operand type.]}
@begin{Reason}
@ChgRef{Version=[2],Kind=[DeletedNoDelMsg]}
@ChgDeleted{Version=[2],Text=<Untagged view conversions appear only as [in] out
  parameters. Hence, the reverse conversion must be legal as well.
  The forward conversion must be legal even if an out parameter,
  because actual parameters of an access type are always
  copied in anyway.>}
@end{Reason}

@begin(itemize)
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[type conversion],sec=(numeric)}
@Defn2{Term=[conversion],sec=(numeric)}
If the target type is a numeric type, then the operand type
shall be a numeric type.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[type conversion],sec=(array)}
@Defn2{Term=[conversion],sec=(array)}
If the target type is an array type, then the operand type shall
be an array type. Further:]}

@begin(inneritemize)

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @Chg{Version=[2],New=[The types shall have the same dimensionality;],Old=[]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @Chg{Version=[2],New=[Corresponding index types shall be convertible;
  @PDefn2{Term=[convertible],Sec=(required)}],Old=[]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @Chg{Version=[2],New=[The component subtypes shall statically match;
  @PDefn2{Term=[statically matching],Sec=(required)}],Old=[]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00392-01]}
  @Chg{Version=[2],New=[If the component types are anonymous access types, then the
  accessibility level of the operand type shall not be statically deeper
  than that of the target type;
  @PDefn2{Term=[accessibility rule],Sec=(type conversion, array components)}],Old=[]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[For unrelated array types, the component types
    could have different accessibility, and we had better not allow a
    conversion of a local type into a global type, in case the local type
    points at local objects. We don't need a check for other types of
    components; such components necessarily are for related types, and
    either have the same accessibility or (for access discriminants) cannot
    be changed so the discriminant check will prevent problems.]}
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00246-01]}
  @ChgAdded{Version=[2],Text=[Neither the target type nor the operand type shall be
  limited;]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[We cannot allow conversions between unrelated limited
    types, as they may have different representations, and (since the types
    are limited), a copy cannot be made to reconcile the representations.]}
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[If the target type of a view conversion has
  aliased components, then so shall the operand type; and]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
    @ChgAdded{Version=[2],Text=[We cannot allow a view conversion from an object
    with unaliased components to an object with aliased components, because
    that would effectively allow pointers to unaliased components. This rule
    was missing from Ada 95.]}
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00246-01],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[The operand type of a view conversion shall not
  have a tagged, private, or volatile subcomponent.]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00246-01]}
    @ChgAdded{Version=[2],Text=[We cannot allow view conversions between unrelated
    might-be-by-reference types, as they may have different representations,
    and a copy cannot be made to reconcile the representations.]}
  @end{Reason}
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[These rules only apply to unrelated array
    conversions; different (weaker) rules apply to conversions between related
    types.]}
  @end{Ramification}
@end(inneritemize)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@Chg{Version=[2],New=[If the target type is @i<universal_access>, then the
operand type shall be an access type.],Old=[]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Such a conversion cannot be written explicitly,
  of course, but it can be implicit (see below).]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[type conversion],sec=(access)}
@Defn2{Term=[conversion],sec=(access)}If the target type is a general access-to-object type, then
the operand type shall be @i<universal_@!access> or an access-to-object type.
Further, if the operand type is not @i<universal_@!access>:]}

@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[The @LegalityTitle and @RunTimeTitle are worded
  so that a @nt{type_conversion} T(X) (where T is an access type) is
  (almost) equivalent to the @nt{attribute_reference}
  X.@key[all]'Access, where the result is of type T.
  The only difference is that the @nt{type_conversion} accepts a null value,
  whereas the @nt{attribute_reference} would raise Constraint_Error.]}
@end{Discussion}

@begin(inneritemize)

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @Chg{Version=[2],New=[If the target type is an access-to-variable
  type, then the operand type shall be an access-to-variable type;],Old=[]}
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[If the target type is an access-to-constant type,
    then the operand type can be access-to-constant or access-to-variable.]}
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[If the target designated type is tagged, then the
  operand designated type shall be convertible to the target designated type;
  @PDefn2{Term=[convertible],Sec=(required)}]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Type=[Leading],Text=[If the target designated type is
  not tagged, then the designated types shall be the same, and either:]}

  @begin(innerinneritemize)
    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00363-01]}
    @Chg{Version=[2],New=[the designated subtypes shall statically match;
    or@PDefn2{Term=[statically matching],Sec=(required)}],Old=[]}

    @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00363-01],ARef=[AI95-00384-01]}
    @Chg{Version=[2],New=[the designated type shall be discriminated in its
    full view and unconstrained in any partial view, and one of the designated
    subtypes shall be unconstrained;],Old=[]}

    @begin{Ramification}
      @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
      @ChgAdded{Version=[2],Text=[This does not require that types have a partial view
      in order to allow the conversion, simply that any partial view that does
      exist is unconstrained.]}

      @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00384-01]}
      @ChgAdded{Version=[2],Text=[This allows conversions both ways (either subtype
      can be unconstrained); while Ada 95 only allowed the conversion if the
      target subtype is unconstrained. We generally want type conversions to be
      symmetric; which type is the target shouldn't matter for legality.]}
    @end{Ramification}

    @begin{Reason}
      @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
      @ChgAdded{Version=[2],Text=[If the visible partial view is constrained, we
      do not allow conversion between unconstrained and constrained subtypes.
      This means that whether the full type had discriminants is not visible
      to clients of the partial view.]}
    @end{Reason}

    @begin{Discussion}
      @ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0095-1]}
      @ChgAdded{Version=[4],Text=[We assume the worst in a generic body whether
      or not a formal subtype has a constrained partial view; specifically, in a
      generic body a discriminated subtype is considered to have a constrained
      partial view if it is a descendant of an untagged generic formal private
      or derived type (see @RefSecNum{Formal Private and Derived Types} for the
      formal definition of this rule).]}
    @end{Discussion}

  @end(innerinneritemize)
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[These rules are designed to ensure that aliased array objects
    only @i(need) "dope" if their nominal subtype is unconstrained,
    but they can always @i(have) dope if required by the run-time model
    (since no sliding is permitted as part of access type conversion).
    By contrast, aliased discriminated objects will always @i(need) their
    discriminants stored with them, even if nominally constrained.
    (Here, we are assuming an implementation that represents
    an access value as a single pointer.)]}
  @end{Reason}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0148-1],ARef=[AI05-0248-1]}
  @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0027-1]}
  @Chg{Version=[2],New=[@PDefn2{Term=[accessibility rule],Sec=(type conversion)}
  The accessibility level of the operand type shall not be statically
  deeper than that of the target type@Chg{Version=[3],New=[, unless the target
  type is an anonymous access type of a stand-alone object. If the target type
  is that of such a stand-alone object, the accessibility level of the operand
  type shall not be statically deeper than that of the declaration of the
  stand-alone object],Old=[]}.@Chg{Version=[4],New=[],Old=[
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.]}],Old=[]}
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0148-1]}
    @ChgAdded{Version=[2],Text=[The access parameter case is handled by a run-time
    check. runtime checks are also done in instance bodies@Chg{Version=[3],
    New=[, and for stand-alone objects of anonymous access types],Old=[]}.]}
  @end{Ramification}
  @begin{Reason}
    @ChgRef{Version=[3],Kind=[Added]}
    @ChgAdded{Version=[3],Text=[We prohibit storing accesses to objects deeper
    than a stand-alone object of an anonymous access-to-object (even while we
    allow storing all other accesses) in order to prevent dangling accesses.]}
  @end{Reason}

@end(inneritemize)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01]}
@ChgAdded{Version=[2],Text=[@Defn2{Term=[type conversion],sec=(access)}
@Defn2{Term=[conversion],sec=(access)}If the target type is a pool-specific
access-to-object type, then
the operand type shall be @i<universal_access>.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This allows @b<null> to be converted to pool-specific
  types. Without it, @b<null> could be converted to general access types but
  not pool-specific ones, which would be too inconsistent. Remember that these
  rules only apply to unrelated types, so we don't have to talk about
  conversions to derived or other related types.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00230-01],ARef=[AI95-00251-01]}
@ChgAdded{Version=[2],Type=[Leading],Text=[@Defn2{Term=[type conversion],sec=(access)}
@Defn2{Term=[conversion],sec=(access)}
If the target type is an access-to-subprogram type, then the operand type
shall be @i<universal_@!access> or an access-to-subprogram type. Further, if
the operand type is not @i<universal_@!access>:]}

@begin(inneritemize)

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0239-1]}
  @Chg{Version=[2],New=[The designated profiles shall be
  @Chg{Version=[3],New=[subtype conformant],Old=[subtype-conformant]}.
  @Defn2{Term=[subtype conformance],Sec=(required)}],Old=[]}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00251-01]}
  @ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0027-1]}
  @Chg{Version=[2],New=[@PDefn2{Term=[accessibility rule],Sec=(type conversion)}
  The accessibility level of the operand type shall not be statically
  deeper than that of the target type.@Chg{Version=[4],New=[],Old=[
  @PDefn{generic contract issue}
  In addition to the places where @LegalityTitle normally apply
  (see @RefSecNum{Generic Instantiation}),
  this rule applies also in the private part of an
  instance of a generic unit.]}
  If the operand type is declared within a generic body,
  the target type shall be declared within the generic body.],Old=[]}

  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgAdded{Version=[2],Text=[The reason it is illegal to convert from an
    access-to-subprogram type declared in a generic body to one declared outside
    that body is that in an implementation that shares generic bodies,
    subprograms declared inside the generic need to have a different
    calling convention @em they need an extra parameter pointing to the
    data declared in the current instance. For subprograms declared in the spec,
    that's OK, because the compiler can know about them at compile time of
    the instantiation.]}
  @end{Reason}

  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0064-2]}
  @ChgAdded{Version=[5],Text=[If the target type is nonblocking, the operand
  type shall be nonblocking.]}
@end(inneritemize)

@end(itemize)

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0027-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}@Comment{Just for a paragraph number change}
@ChgAdded{Version=[4],Text=[@PDefn{generic contract issue}
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
these rules apply also in the private part of an
instance of a generic unit.]}
@begin{Discussion}
  @ChgRef{Version=[4],Kind=[AddedNormal]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0005-1]}
  @ChgAdded{Version=[4],Text=[This applies to @i<all> of the @LegalityTitle
  in this section. It won't matter for the majority of these rules, but
  in any case that it does, we want to apply the same
  @Chg{Version=[5],New=[rules],Old=[recheck]} in the private
  part. (Ada got the default wrong for these, as there is only one known case
  where we don't want to recheck in the private part,
  see derivations without record extensions in
  @RefSecNum{Derived Types and Classes}.)]}
@end{Discussion}

@end{Legality}

@begin{StaticSem}
A @nt{type_conversion} that is a value conversion denotes the value
that is
the result of converting the value of the operand to the target subtype.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0264-1]}
A @nt{type_conversion} that is a view conversion
denotes a view of the object denoted by the operand. This
view is a variable of the target type if the operand denotes
a variable; otherwise@Chg{Version=[3],New=[,],Old=[]}
it is a constant of the target type.

@PDefn2{Term=[nominal subtype], Sec=(associated with a @nt<type_conversion>)}
The nominal subtype of a @nt<type_conversion> is its target subtype.
@end{StaticSem}

@begin{RunTime}
@Leading@PDefn2{Term=[evaluation], Sec=(value conversion)}
@Defn2{Term=[corresponding value],
  Sec=(of the target type of a conversion)}
@Defn{conversion}
For the evaluation of a @nt<type_conversion> that is a value conversion,
the operand is evaluated, and then
the value of the operand is @i(converted) to a @i(corresponding)
value of the target type, if any.
@IndexCheck{Range_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of runtime check)}
If there is no value
of the target type that corresponds to the operand value,
Constraint_Error is raised@Redundant[; this can only
happen on conversion to a modular type,
and only when the operand value is outside the base range of
the modular type.]
Additional rules follow:
@begin(itemize)
@Defn2{Term=[type conversion],sec=(numeric)}
@Defn2{Term=[conversion],sec=(numeric)}
Numeric Type Conversion
@begin(inneritemize)
  If the target and the operand types are both integer types, then
  the result is the value of the target type that corresponds
  to the same mathematical integer as the operand.

  If the target type is a decimal fixed point type, then the result is
  truncated (toward 0) if the
  value of the operand is not a multiple of the @i{small} of the target
  type.

  @Defn{accuracy}
  If the target type is some other real type,
  then the result is within the accuracy of the target type
  (see @RefSec{Numeric Performance Requirements},
  for implementations that support the Numerics Annex).
  @begin(Discussion)
    An integer type might have more bits of precision than a real type,
    so on conversion (of a large integer), some precision might be lost.
  @end(Discussion)

  If the target type is an integer type and the operand type is real,
  the result is rounded to the nearest integer (away from zero
  if exactly halfway between two integers).
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00267-01]}
  This was implementation defined in Ada 83.
    There seems no reason to preserve the nonportability
    in Ada 95. Round-away-from-zero is the conventional
    definition of rounding, and standard Fortran and COBOL both specify
    rounding away from zero, so for interoperability, it seems important
    to pick this. This is also the most easily @lquotes@;undone@rquotes@; by hand.
    Round-to-nearest-even is an alternative, but that is quite complicated
    if not supported by the hardware. In any case, this operation is not
    @Chg{Version=[2],New=[usually],Old=[expected to be]} part of an inner loop,
    so predictability and portability are judged most important.
    @Chg{Version=[2],New=[A],Old=[We anticipate that
    a]} floating point attribute function Unbiased_Rounding @Chg{Version=[2],
    New=[is],Old=[will be]} provided@Chg{Version=[2],
    New=[ (see @RefSecNum{Attributes of Floating Point Types})],Old=[]}
    for those applications that require round-to-nearest-even@Chg{Version=[2],
    New=[, and a floating point attribute function Machine_Rounding (also see
    @RefSecNum{Attributes of Floating Point Types}) is provided for those
    applications that require the highest possible performance], Old=[]}.
    @lquotes@;Deterministic@rquotes@; rounding is required for static
    conversions to integer as well.
    See @RefSecNum{Static Expressions and Static Subtypes}.
@end{Discussion}
@end(inneritemize)

@Defn2{Term=[type conversion],sec=(enumeration)}
@Defn2{Term=[conversion],sec=(enumeration)}
Enumeration Type Conversion
@begin(inneritemize)
  The result is the value of the target type with the same
  position number as that of the operand value.
@end(inneritemize)

@Defn2{Term=[type conversion],sec=(array)}
@Defn2{Term=[conversion],sec=(array)}
Array Type Conversion
@begin(inneritemize)
  @IndexCheck{Length_Check}
  If the target subtype is a constrained array subtype, then
  a check is made that the length of each dimension of the value of
  the operand equals the length of the corresponding dimension of the
  target subtype.
  The bounds of the result are those of the target subtype.

  @IndexCheck{Range_Check}
  If the target subtype is an unconstrained array subtype, then the
  bounds of the result are obtained by converting each bound of the value
  of the operand to the corresponding index type of the target type.
  @PDefn2{Term=[implicit subtype conversion],Sec=(array bounds)}
  For each nonnull index range, a check is made that the
  bounds of the range belong to the corresponding index subtype.
  @begin(Discussion)
    Only nonnull index ranges are checked, per AI83-00313.
  @end(Discussion)

  In either array case, the value of each component of the result is that
  of the matching component of the operand value
  (see @RefSecNum{Relational Operators and Membership Tests}).
  @begin{Ramification}
    This applies whether or not the component is initialized.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00392-01]}
  @ChgAdded{Version=[2],Text=[If the component types of the array types are
  anonymous access types, then a check is made that the accessibility level
  of the operand type is not deeper than that of the target type.
  @IndexCheck{Accessibility_Check}]}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[AddedNormal]}
    @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0005-1]}
    @ChgAdded{Version=[2],Text=[This check is needed for operands that are
    @Chg{Version=[5],New=[],Old=[access
    parameters and ]}in instance bodies. Other cases are handled by the
    legality rule given previously.]}
  @end{Reason}

@end(inneritemize)

@Defn2{Term=[type conversion],sec=[composite (non-array)]}
@Defn2{Term=[conversion],sec=[composite (non-array)]}
Composite (Non-Array) Type Conversion
@begin(inneritemize)
  The value of each nondiscriminant component of the result
  is that of the matching component of the operand value.
  @begin{Ramification}
    This applies whether or not the component is initialized.
  @end{Ramification}

  @Redundant[The tag of the result is that of the operand.]
  @IndexCheck{Tag_Check}
  If the operand type is class-wide,
  a check is made that the tag of the operand identifies
  a (specific) type that is covered by or descended from
  the target type.
  @begin{Ramification}
    This check is certain to succeed
    if the operand type is itself covered by or descended from
    the target type.
  @end{Ramification}
  @begin{TheProof}
    The fact that a @nt{type_conversion} preserves the tag
    is stated officially in @RefSec{Tagged Types and Type Extensions}
  @end{TheProof}

  For each discriminant of the target type that corresponds to
  a discriminant of the operand type, its value is that of
  the corresponding discriminant of the operand value;
  @IndexCheck{Discriminant_Check}
  if it corresponds
  to more than one discriminant of the operand type, a check is made
  that all these discriminants are equal in the operand value.

  For each discriminant of the target type that corresponds to
  a discriminant that is specified by the @nt<derived_type_definition>
  for some ancestor of the operand type (or if class-wide,
  some ancestor of the specific type identified by the tag
  of the operand), its
  value in the result is that specified by the @nt<derived_type_definition>.
@begin{Ramification}
  It is a ramification of the rules for the discriminants of derived types
  that each discriminant of the result is covered either by this
  paragraph or the previous one. See @RefSecNum(Discriminants).
@end{Ramification}

  @IndexCheck{Discriminant_Check}
  For each discriminant of the operand type that corresponds
  to a discriminant that is specified by the @nt<derived_type_definition>
  for some ancestor of the target type,
  a check is made that in the operand value it equals the value
  specified for it.

  @IndexCheck{Range_Check}
  For each discriminant of the result, a check is made that its
  value belongs to its subtype.
@end(inneritemize)

@Defn2{Term=[type conversion],sec=(access)}
@Defn2{Term=[conversion],sec=(access)}
Access Type Conversion
@begin(inneritemize)
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0148-1],ARef=[AI05-0248-1]}
  For an access-to-object type,
  a check is made that the accessibility level of the operand
  type is not deeper than that of the target type@Chg{Version=[3],New=[,
  unless the target type is an anonymous access type of a stand-alone
  object. If the target type is that of such a stand-alone object, a check is
  made that the accessibility level of the operand type is not deeper than that
  of the declaration of the stand-alone object@Redundant[; then if the check
  succeeds, the accessibility level of the target type becomes that of the
  operand type]],Old=[]}.
  @IndexCheck{Accessibility_Check}

  @begin{Ramification}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0148-1]}
  This check is needed for operands that are access
  parameters@Chg{Version=[3],New=[, for stand-alone anonymous access objects,],Old=[]}
  and in instance bodies.

  Note that this check can never fail for the implicit conversion
  to the anonymous type of an access parameter that is done when
  calling a subprogram with an access parameter.
  @end{Ramification}

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01],ARef=[AI95-00231-01]}
  If the @Chg{Version=[2],New=[],Old=[target type is an anonymous access type,
  a check is made that the value of the operand is not null;
  if the target is not an anonymous access type, then the
  result is null if the ]}operand value is null@Chg{Version=[2],New=[, the
  result of the conversion is the null value of the target type.],Old=[.
  @IndexCheck{Access_Check}]}
  @begin{Ramification}
    @ChgRef{Version=[2],Kind=[Revised]}
    A conversion to an anonymous access type
    happens implicitly as part of initializing
    @Chg{Version=[2],New=[or assigning to an anonymous access object],
    Old=[an access discriminant or access parameter]}.
  @end{Ramification}
  @begin{Reason}
    @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00231-01]}
    @ChgDeleted{Version=[2],Text=[As explained in @RefSec{Access Types},
    it is important that a value of an anonymous access type
    can never be null.]}
  @end{Reason}

  If the operand value is not null, then
  the result designates the same
  object (or subprogram) as is designated by the operand value,
  but viewed as being of the target designated subtype (or profile);
  any checks associated with evaluating a conversion to
  the target designated subtype are performed.
@begin{Ramification}
  The checks are certain to succeed if
  the target and operand designated subtypes statically match.
@end{Ramification}
@end(inneritemize)
@end(itemize)

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3],ARef=[AI05-0290-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0071-1]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0333-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[]}@Comment{Conditional "leading"}
@IndexCheck{Range_Check}
@IndexCheck{Discriminant_Check}
@IndexCheck{Index_Check}
@Chg{Version=[2],New=[@IndexCheck{Access_Check}],Old=[]}
After conversion of the value to the target type,
if the target subtype is constrained,
a check is performed that the value satisfies this constraint.@Chg{Version=[2],
New=[ If the target subtype excludes null,
then a check is made that the value is not null.],Old=[]}@Chg{Version=[3],
New=[ If predicate checks are enabled
for the target subtype (see @RefSecNum{Subtype Predicates}), a check
is performed that the @Chg{Version=[4],New=[value satisfies the
predicates],Old=[predicate]} of the target
subtype@Chg{Version=[4],New=[],Old=[ is satisfied for the
value]}@Chg{Version=[5],New=[, unless the conversion
is:],Old=[]}.@Defn2{Term=[predicate check],
Sec=[subtype conversion]}@Defn2{Term=[check, language-defined],
Sec=[controlled by assertion policy]}],Old=[]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01]}
  The @Chg{Version=[2],New=[first],Old=[above]} check
  @Chg{Version=[2],New=[above ],Old=[]}is a Range_Check for scalar subtypes, a
  Discriminant_Check or Index_Check for access subtypes, and a Discriminant_Check
  for discriminated subtypes. The Length_Check for an array conversion is
  performed as part of the conversion to the target type.@Chg{Version=[2],
  New=[ The check for exclusion of null is an Access_Check.],Old=[]}
@end{Ramification}
@begin{Itemize}
  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0333-1]}
  @ChgAdded{Version=[5],Text=[a view conversion that is an actual
  parameter of mode @key<out>; or]}

  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0333-1]}
  @ChgAdded{Version=[5],Text=[an implicit subtype conversion of an actual
  parameter of mode @key<out> to the nominal subtype of its formal
  parameter.]}
@end{Itemize}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0333-1]}
  @ChgAdded{Version=[5],Text=[The reverse conversion applied to by-copy @key{out}
  parameters is @i<not> a view conversion and it is to the nominal subtype
  of the @i<actual> parameter, therefore any enabled predicate
  checks @i<are> performed.]}
@end{Ramification}

@PDefn2{Term=[evaluation], Sec=(view conversion)}
For the evaluation of a view conversion, the operand @nt<name> is
evaluated, and a new view of the
object denoted by the operand is created, whose type is the
target type;
@IndexCheck{Length_Check}
@IndexCheck{Tag_Check}
@IndexCheck{Discriminant_Check}
if the target type is composite, checks are
performed as above for a value conversion.

@Leading@;The properties of this new view are as follows:
@begin(itemize)
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0017],ARef=[AI95-00184-01]}
  If the target type is composite, the bounds or discriminants (if any)
  of the view are as defined above for a value conversion;
  each nondiscriminant component of the view denotes the matching
  component of the operand object; the
  subtype of the view is constrained if either the target subtype
  or the operand object is constrained,
  @Chg{New=[or if the target subtype is indefinite,],Old=[]}
  or if the operand type is a descendant of the target type@Chg{New=[],Old=[,]}
  and has discriminants that were not inherited from
  the target type;

  If the target type is tagged, then an assignment to the
  view assigns to the corresponding part of the object denoted
  by the operand; otherwise, an assignment to the view
  assigns to the object, after converting
  the assigned value to the subtype of the object (which
  might raise Constraint_Error);
  @PDefn2{Term=[implicit subtype conversion],Sec=(assignment to view conversion)}

@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0074-1]}
  Reading the value of the view yields the result of converting
  the value of the operand object to the target subtype
  (which might raise Constraint_Error), except if the object
  is of an @Chg{Version=[4],New=[elementary],Old=[access]} type and the
  view conversion is passed as an @key(out) parameter;
  in this latter case, the value of the operand object
  @Chg{Version=[4],New=[may be],Old=[is]} used to initialize the
  formal parameter without checking against any constraint of the
  target subtype
  (@Chg{Version=[4],New=[as described more precisely in],Old=[see]}
  @RefSecNum(Parameter Associations)).
  @PDefn2{Term=[implicit subtype conversion],Sec=(reading a view conversion)}
  @begin(Reason)
    @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0074-1]}
    This ensures that even an @key(out) parameter of
    an @Chg{Version=[4],New=[elementary],Old=[access]} type is
    initialized reasonably.
  @end(Reason)
@end(itemize)

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0290-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0096-1]}
@Defn2{Term=[Program_Error],Sec=(raised by failure of runtime check)}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of runtime check)}@Chg{Version=[3],New=[
@Defn2{Term=(Assertion_Error),Sec=(raised by failure of runtime check)}],Old=[]}
If an Accessibility_Check fails, Program_Error is raised.
@Chg{Version=[3],New=[If a predicate check fails, @Chg{Version=[4],New=[the
effect is as defined in subclause @RefSec{Subtype Predicates}],
Old=[Assertions.Assertion_Error is
raised]}. ],Old=[]}Any other check associated with a conversion raises
Constraint_Error if it fails.

Conversion to a type is the same as conversion to an unconstrained
subtype of the type.
@begin{Reason}
This definition is needed because the semantics of various
constructs involves converting to a type,
whereas an explicit @nt{type_conversion} actually converts to a subtype.
For example, the evaluation of a @nt{range} is defined to convert the
values of the expressions to the type of the range.
@end{Reason}
@begin{Ramification}
A conversion to a scalar type, or, equivalently,
to an unconstrained scalar subtype,
can raise Constraint_Error if the value is outside the base range of the
type.
@end{Ramification}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0027-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded],ARef=[AI12-0226-1]}
@ChgAdded{Version=[4],Type=[Leading],Text=[Evaluation of a value conversion of
@Chg{Version=[5],New=[an object],Old=[a composite type]} either creates a new
anonymous object@Redundant[ (similar to the
object created by the evaluation of an @nt{aggregate} or a function call)] or
yields a new view of the operand object without creating a new object:]}

@begin{Itemize}
   @ChgRef{Version=[4],Kind=[Added]}
   @ChgAdded{Version=[4],Text=[If the target type is a by-reference type and
   there is a type that is an ancestor of both the target type and the operand
   type then no new object is created;]}

   @ChgRef{Version=[4],Kind=[Added]}
   @ChgAdded{Version=[4],Text=[If the target type is an array type having
   aliased components and the operand type is an array type having unaliased
   components, then a new object is created;]}

   @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0226-1]}
   @ChgAdded{Version=[5],Text=[If the target type is an elementary type,
   then a new object is created;]}

   @ChgRef{Version=[4],Kind=[Added]}
   @ChgRef{Version=[5],Kind=[RevisedAdded]}@Comment{To mark a paragraph number change}
   @ChgAdded{Version=[4],Text=[Otherwise, it is unspecified whether a new object
   is created.@PDefn{unspecified}]}
@end{Itemize}

@ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0027-1]}
@ChgRef{Version=[5],Kind=[RevisedAdded]}@Comment{To mark a paragraph number change}
@ChgAdded{Version=[4],Text=[If a new object is created, then the initialization
of that object is an assignment operation.]}

@begin{Reason}
   @ChgRef{Version=[4],Kind=[AddedNormal]}
   @ChgAdded{Version=[4],Text=[This makes a difference in the case of converting
   from an array type with unaliased components to one with aliased components
   if the element type has a controlled part.]}

   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0226-1]}
   @ChgAdded{Version=[5],Text=[For an elementary type, the representation might
   change so we require a new object to avoid problems.]}
@end{Reason}

@begin{ImplNote}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0226-1]}
   @ChgAdded{Version=[5],Text=[The temporary object need not be materialized
   in most cases; it should be handled like the return object of a predefined
   operator. Generally, whether the object exists can only be detected if it
   is renamed (unless a part of the type is controlled).]}
@end{ImplNote}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0226-1]}
   @ChgAdded{Version=[5],Text=[This set of rules does not apply in those cases
   where the operand is not an object (such as a value conversion of a named
   number); in such cases, the result isn't an object, so it isn't necessary to
   describe what that means. The rules cover all value conversions of composite
   types (since there aren't any values of composite types separate from
   objects).]}
@end{Discussion}
@end{RunTime}

@begin{Notes}
@RootDefn{implicit subtype conversion}
In addition to explicit @nt<type_conversion>s,
type conversions are performed implicitly in situations where the
expected type and the actual type of a construct differ,
as is permitted by the type resolution rules
(see @RefSecNum(The Context of Overload Resolution)).
For example, an integer literal is
of the type @i(universal_integer), and is implicitly converted
when assigned to a target of some specific integer type.
Similarly, an actual parameter of a specific
tagged type is implicitly converted when the corresponding
formal parameter is of a class-wide type.

@NoPrefix@;@RootDefn{implicit subtype conversion}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of runtime check)}
Even when the expected and actual types are the same,
implicit subtype conversions are performed to adjust the array bounds (if any)
of an operand to match the desired target subtype, or to
raise Constraint_Error if the (possibly adjusted) value does not satisfy
the constraints of the target subtype.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00230-01]}
A ramification of the
overload resolution rules is that the operand of
an (explicit) @nt<type_conversion> cannot be
@Chg{Version=[2],New=[],Old=[the literal @key(null), ]}an @nt<allocator>,
an @nt<aggregate>, a @nt<string_literal>,
a @nt<character_literal>, or an @nt<attribute_reference>
for an Access or Unchecked_Access attribute.
Similarly, such an @nt{expression} enclosed by parentheses is not
allowed. A @nt<qualified_expression> (see @RefSecNum(Qualified Expressions))
can be used instead of such a @nt<type_conversion>.

The constraint of the target subtype has no effect
for a @nt<type_conversion> of an elementary type passed
as an @key(out) parameter. Hence, it is recommended
that the first subtype be specified
as the target to minimize confusion
(a similar recommendation applies to renaming and
generic formal @key(in out) objects).
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of numeric type conversion:)
@begin{Example}
Real(2*J)      @RI[--  value is converted to floating point]
Integer(1.6)   @RI[--  value is 2]
Integer(-0.4)  @RI[--  value is 0]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Example of conversion between derived types:)
@end{WideAbove}
@begin{Example}
@key(type) A_Form @key(is) @key(new) B_Form;

X : A_Form;
Y : B_Form;

X := A_Form(Y);
Y := B_Form(X);  @RI[--  the reverse conversion ]
@end{Example}

@begin{WideAbove}
@leading@keepnext@i(Examples of conversions between array types:)
@end{WideAbove}

@begin{Example}
@key(type) Sequence @key(is) @key(array) (Integer @key(range) <>) @key(of) Integer;
@key(subtype) Dozen @key(is) Sequence(1 .. 12);
Ledger : @key(array)(1 .. 100) @key(of) Integer;

Sequence(Ledger)            @RI[--  bounds are those of Ledger]
Sequence(Ledger(31 .. 42))  @RI[--  bounds are 31 and 42]
Dozen(Ledger(31 .. 42))     @RI[--  bounds are those of Dozen ]
@end{Example}
@end{Examples}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
A @nt<character_literal> is not allowed as the
operand of a @nt<type_conversion>,
since there are now two character types in package Standard.

The component subtypes have to statically match in an array conversion,
rather than being checked for matching constraints at run time.

Because sliding of array bounds is now provided for operations where it
was not in Ada 83,
programs that used to raise Constraint_Error might now
continue executing and produce a reasonable result.
This is likely to fix more bugs than it creates.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
A @nt<type_conversion> is considered the name of an object
in certain circumstances (such a @nt<type_conversion>
is called a view conversion).
In particular, as in Ada 83,
a @nt<type_conversion> can appear as an @key(in out) or @key(out)
actual parameter. In addition, if the target type is tagged
and the operand is the @nt<name> of an object, then so
is the @nt<type_conversion>, and it can be used as the @nt<prefix>
to a @nt<selected_component>, in an @nt<object_renaming_declaration>, etc.

We no longer require type-mark conformance between
a parameter of the form of a type conversion, and the corresponding
formal parameter. This had caused some problems for
inherited subprograms (since there isn't really a type-mark
for converted formals), as well as for renamings, formal subprograms,
etc. See AI83-00245, AI83-00318, AI83-00547.

We now specify @lquotes@;deterministic@rquotes@; rounding from real to integer types
when the value of the operand is exactly between two integers
(rounding is away from zero in this case).

@lquotes@;Sliding@rquotes@; of array bounds
(which is part of conversion to an array subtype)
is performed in more cases in Ada 95 than in Ada 83.
Sliding is not performed on
the operand of a membership test,
nor on the operand of a @nt{qualified_expression}.
It wouldn't make sense on a membership test,
and we wish to retain a connection between subtype membership
and subtype qualification. In general, a subtype membership test returns
True if and only if a corresponding subtype qualification
succeeds without raising an exception.
Other operations that take arrays perform sliding.
@end{Extend83}

@begin{DiffWord83}
We no longer explicitly list the kinds of things that are not allowed
as the operand of a @nt<type_conversion>, except in a NOTE.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
The rules in this @Chg{Version=[3],New=[subclause],Old=[clause]} subsume the rules
for "parameters of the form of a type conversion,"
and have been generalized to cover the use of a type conversion
as a @nt<name>.
@end{DiffWord83}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00246-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}
  @b[Amendment Correction:] Conversions
  between unrelated array types that are limited or (for view conversions)
  might be by-reference types are now illegal. The representations of two such
  arrays may differ, making the conversions impossible. We make the check here,
  because legality should not be based on representation properties.
  Such conversions are likely to be rare, anyway. There is a potential that
  this change would make a working program illegal (if the types have the same
  representation).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[If a discriminated full type has a partial view
  (private type) that is constrained, we do not allow conversion between
  access-to-unconstrained and access-to-constrained subtypes designating the
  type. Ada 95 allowed this conversion and the declaration of various access
  subtypes, requiring that the designated object be constrained and thus making
  details of the implementation of the private type visible to the client of
  the private type. See @RefSecNum{Allocators} for more on this topic.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00230-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}Conversion rules for
  @i<universal_access> were defined. These allow the use of anonymous access
  values in equality tests (see
  @RefSecNum{Relational Operators and Membership Tests}), and also allow the
  use of @b<null> in type conversions and other
  contexts that do not provide a single expected type.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00384-01]}
  @ChgAdded{Version=[2],Text=[A type conversion from
  an access-to-discriminated and unconstrained object to an
  access-to-discriminated and constrained one is allowed. Ada 95 only allowed
  the reverse conversion, which was weird and asymmetric. Of course, a
  constraint check will be performed for this conversion.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0017],ARef=[AI95-00184-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Wording was added to ensure that
  view conversions are constrained, and that a tagged view conversion has a
  tagged object. Both rules are needed to avoid having a way to change the
  discriminants of a constrained object.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0008],ARef=[AI95-00168-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Wording was added to ensure
  that the aliased status of array components cannot change in a view
  conversion. This rule was needed to avoid having a way to change the
  discriminants of an aliased object. This rule was repealed later, as
  Ada 2005 allows changing the discriminants of an aliased object.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01]}
  @ChgAdded{Version=[2],Text=[Wording was added to check subtypes that exclude
  null (see @RefSecNum{Access Types}).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00251-01]}
  @ChgAdded{Version=[2],Text=[The organization of the legality rules was
  changed, both to make it clearer, and to eliminate an unintentional
  incompatibility with Ada 83. The old organization prevented type conversions
  between some types that were related by derivation (which Ada 83 always
  allowed).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00330-01]}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0005-1]}
  @ChgAdded{Version=[2],Text=[Clarified that an untagged type conversion
  appearing as a generic actual parameter for a generic @key{in out} formal
  parameter is not a view conversion (and thus is illegal). This confirms
  the ACATS tests, so all implementations already follow this
  @Chg{Version=[3],New=[interpretation],Old=[intepretation]}.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[Rules added by the Corrigendum to eliminate
  problems with discriminants of aliased components changing were removed, as
  we now generally allow discriminants of aliased components to be changed.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00392-01]}
  @ChgAdded{Version=[2],Text=[Accessibility checks on conversions involving
  types with anonymous access components were added. These components have
  the level of the type, and conversions can be between types at different
  levels, which could cause dangling access values in the absence of such
  checks.]}
@end{DiffWord95}

@begin{Inconsistent2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0148-1]}
  @ChgAdded{Version=[3],Text=[@Defn{inconsistencies with Ada 2005}A
  stand-alone object of an anonymous access-to-object type now has dynamic
  accessibility. Normally, this will make programs legal that were illegal
  in Ada 2005. However, it is possible that a program that previously raised
  Program_Error now will not. It is very unlikely that an existing program
  intentionally depends on the exception being raised; the change is more likely
  to fix bugs than introduce them.]}
@end{Inconsistent2005}

@begin{Diffword2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0115-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Clarified that a root numeric
  type is not considered a common ancestor for a conversion.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0290-1]}
  @ChgAdded{Version=[3],Text=[Added rules so that predicate aspects (see
  @RefSecNum{Subtype Predicates}) are enforced on subtype conversion.]}
@end{Diffword2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0333-1]}
  @ChgAdded{Version=[5],Text=[@b<Correction:> Predicate checks are no longer
  made on any @key{out} parameters before a call (they're still made when the
  call returns). This was already true for elementary @key{out} parameters. If
  a program depends on a predicate check failing on an inbound @key{out}
  composite parameter, it will get an incorrect result. This seems
  quite unlikely, as programs (outside of ACATS tests) that depend on the
  failure of checks are very rare, and the predicate might be checking
  uninitialized components (making check failure unreliable).]}
@end{Inconsistent2012}

@begin{Incompatible2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0095-1]}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0005-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Because of a rule added in
  @RefSecNum{Formal Private and Derived Types}, the checks for the
  legality of an access type conversion in a generic body were strengthened
  to use an @Chg{Version=[5],New=[assume-the-worst],Old=[assume the worst]}
  rule. This case is rather unlikely @Chg{Version=[5],New=[because],Old=[as]}
  a formal private or derived type with discriminants is required along with a
  conversion between two access types whose designated types don't statically
  match, and any such programs were at risk having objects disappear while
  valid access values still pointed at them.]}
@end{Incompatible2012}

@begin{Diffword2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0027-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Moved the generic boilerplate
  so that it covers all @LegalityTitle in this subclause. This was always
  intended, but it is not expected to change anything other than conversions
  between unrelated arrays.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0027-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Added a formal definition of
  the copy potentially created by a value conversion of a composite type,
  so properties like finalization and accessibility are properly defined.
  This model was always intended and expected (else
  @RefSecNum{Change of Representation} would not work), but it was not
  previously formally defined.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0071-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Updated wording of
  type conversions to use the new term "satisfies the predicates"
  (see @RefSecNum{Subtype Predicates}).]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0074-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Clarified the wording
  describing the effect of view conversions of @key[out] parameters
  such that it is clear that the detailed effect is defined in
  @RefSecNum(Parameter Associations), not here.]}

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0096-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Updated wording of
  type conversions so that the exception raise or other effect of a failed
  predicate check is as defined in @RefSecNum{Subtype Predicates}; we don't
  want to repeat those rules here. This doesn't change the behavior for
  predicate checks possible in original Ada 2012, only ones using the new
  aspect Predicate_Failure.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0064-2]}
  @ChgAdded{Version=[5],Text=[Required Nonblocking
  (see @RefSecNum{Intertask Communication}) matching for
  access-to-subprogram conversions.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0226-1]}
  @ChgAdded{Version=[5],Text=[Described the objects associated with
  value conversions of elementary types. This is necessary to support
  an extension documented in @RefSecNum{Objects and Named Numbers}.]}
@end{Diffword2012}


@LabeledClause{Qualified Expressions}

@begin{Intro}
@Redundant[A @nt<qualified_expression> is used to state explicitly the type,
and to verify the subtype, of an operand that is either an @nt<expression>
or an @nt<aggregate>.
@IndexSeeAlso{Term=[type conversion],See=(qualified_expression)}]
@end{Intro}

@begin{Syntax}
@Syn{lhs=<qualified_expression>,rhs="
   @Syn2{subtype_mark}@SingleQuote@;(@Syn2{expression}) | @Syn2{subtype_mark}@SingleQuote@Syn2{aggregate}"}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0325-1]}
@PDefn2{Term=[operand], Sec=(of a @nt{qualified_expression})}
The @Chg{Version=[5],New=[expected type for the ],Old=[]}@i(operand) (the
@nt{expression} or @nt{aggregate})
@Chg{Version=[5],New=[is],Old=[shall resolve to be of the type]} determined
by the @nt{subtype_@!mark}@Chg{Version=[5],New=[],Old=[,
or a universal type that covers it]}.@Chg{Version=[5],New=[ Furthermore, the
operand shall resolve to be either the specified expected type or a universal
type that covers it.],Old=[]}
@begin{Reason}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0325-1]}
  @ChgAdded{Version=[5],Text=[The first sentence defines the expected type for
  rules that assume one is defined. The second sentence prevents the use of the
  various implicit conversions that are usually allowed for expected
  types (except the one for numeric literals). The intent is that a qualified
  expression is similar to an assertion about the subtype of the operand, and
  thus implicit conversions would interfere with that intent.]}
@end{Reason}
@end{Resolution}

@begin{StaticSem}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0003-1]}
@ChgAdded{Version=[3],Text=[@Redundant[If the operand of a @nt{qualified_expression}
denotes an object, the @nt{qualified_expression} denotes a constant view
of that object.] The nominal subtype of a @nt{qualified_expression}
is the subtype denoted by the @nt{subtype_mark}.]}
@begin{TheProof}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[This is stated in @RefSecNum{Objects and Named Numbers}.]}
@end{TheProof}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0100-1]}
@PDefn2{Term=[evaluation], Sec=(qualified_expression)}
@IndexCheck{Range_Check}
@IndexCheck{Discriminant_Check}
@IndexCheck{Index_Check}
The evaluation of a @nt{qualified_expression} evaluates the
operand (and if of a universal type, converts it
to the type determined by the @nt{subtype_mark})
and checks that its value belongs to the subtype denoted by
the @nt{subtype_mark}.
@PDefn2{Term=[implicit subtype conversion],Sec=(qualified_expression)}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of runtime check)}
The exception Constraint_Error is raised if this check fails.@Chg{Version=[4],
New=[ Furthermore, if predicate checks are enabled for the subtype
denoted by the @nt{subtype_mark}, a check is performed as defined in
subclause @RefSec{Subtype Predicates} that the value satifies the predicates
of the subtype.],Old=[]}
@begin{Ramification}
  This is one of the few contexts in Ada 95 where implicit subtype conversion
  is not performed prior to a constraint check, and hence no
  @lquotes@;sliding@rquotes@; of array bounds is provided.

  @ChgRef{Version=[4],Kind=[Added],ARef=[AI12-0100-1]}
  @ChgAdded{Version=[4],Text=[The effect of a failed predicate check
  is as defined in @RefSecNum{Subtype Predicates}; such a check could raise
  any exception, not just Constraint_Error or Assertion_Error.]}
@end{Ramification}
@begin{Reason}
  Implicit subtype conversion is not provided because a
  @nt<qualified_expression> with a constrained target subtype is
  essentially an assertion about the subtype of the operand, rather
  than a request for conversion. An explicit @nt<type_conversion> can
  be used rather than a @nt<qualified_expression> if subtype
  conversion is desired.

  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0100-1]}
  @ChgAdded{Version=[4],Text=[We do a predicate check here so that
  a @nt{qualified_expression} never allows something that the equivalent
  @nt{type_conversion} would not allow.]}
@end{Reason}
@end{RunTime}

@begin{Notes}
When a given context does not uniquely identify an expected type,
a @nt<qualified_expression> can be used to do so.
In particular, if an overloaded @nt<name> or
@nt<aggregate> is passed to an overloaded subprogram, it
might be necessary to qualify the operand to resolve its type.
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of disambiguating expressions using qualification:)
@begin{Example}
@key(type) Mask @key(is) (Fix, Dec, Exp, Signif);
@key(type) Code @key(is) (Fix, Cla, Dec, Tnz, Sub);

Print (Mask'(Dec));  @RI[--  Dec is of type Mask]
Print (Code'(Dec));  @RI[--  Dec is of type Code ]

@key(for) J @key(in) Code'(Fix) .. Code'(Dec) @key(loop) ... @RI[-- qualification needed for either Fix or Dec]
@key(for) J @key(in) Code @key(range) Fix .. Dec @key(loop) ...    @RI[-- qualification unnecessary]
@key(for) J @key(in) Code'(Fix) .. Dec @key(loop) ...        @RI[-- qualification unnecessary for Dec]

Dozen'(1 | 3 | 5 | 7 => 2, @key(others) => 0) @RI[-- see @RefSecNum{Type Conversions} ]
@end{Example}
@end{Examples}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0003-1]}
  @ChgAdded{Version=[3],Text=[Added a definition of
  the nominal subtype of a @nt{qualified_expression}.]}
@end{DiffWord2005}

@begin{Inconsistent2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0100-1]}
  @ChgAdded{Version=[4],Text=[@Defn{inconsistencies with Ada 2012}@b<Corrigendum:>
  A @nt{qualified_expression} now performs a predicate check for the named
  subtype (if it is enabled). Original Ada 2012 did not include that check
  (an omission). While this is formally inconsistent (an exception could
  be raised when none would be raised by original Ada 2012), cases when this
  could be the case are likely to be rare (the qualified expression would have
  to have a stricter subtype than the following usage) and the check is more
  likely to detect bugs than be unexpected.]}
@end{Inconsistent2012}

@begin{DiffWord2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0325-1]}
  @ChgAdded{Version=[5],Text=[Reworded the resolution rule so that the
  operand of a @nt{qualified_expression} has an expected type. This eliminates
  an annoying inconsistency in the language definition.]}
@end{DiffWord2012}


@LabeledClause{Allocators}

@begin{Intro}
@Redundant[The evaluation of an @nt<allocator> creates an object and yields
an access value that designates the object.
@IndexSee{Term=[new],See=(allocator)}
@IndexSee{Term=[malloc],See=(allocator)}
@IndexSeeAlso{Term=[heap management],See=(allocator)}]
@end{Intro}

@begin{Syntax}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0111-3]}
@Syn{lhs=<allocator>,rhs="
   @key{new} @Chg{Version=[3],New=<[@Syn2{subpool_specification}] >,Old=<>}@Syn2{subtype_indication}@Chg{Version=[3],New=<
>,Old=<>} | @key{new} @Chg{Version=[3],New=<[@Syn2{subpool_specification}] >,Old=<>}@Syn2{qualified_expression}"}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0111-3]}
@AddedSyn{Version=[3],lhs=<@Chg{Version=[3],New=<subpool_specification>,Old=<>}>,
rhs="@Chg{Version=[3],New=<(@SynI{subpool_handle_}@Syn2{name})>,Old=<>}"}

@begin{SyntaxText}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0104-1]}
@ChgAdded{Version=[3],Text=[For an @nt{allocator} with a @nt{subtype_indication},
the @nt{subtype_indication} shall not specify a @nt{null_exclusion}.]}

@begin{Reason}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[Such an uninitialized @nt{allocator} would
necessarily raise Constraint_Error, as the default value is @key[null].
Also note that the syntax does not allow a @nt{null_exclusion} in
an initialized @nt{allocator}, so it makes sense to make the
uninitialized case illegal as well.]}
@end{Reason}
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0010],ARef=[AI95-00127-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0111-3],ARef=[AI05-0269-1]}
@PDefn2{Term=[expected type],Sec=(allocator)}
The expected type for an @nt<allocator> shall be a single access-to-object
type @Chg{New=[with],Old=[whose]} designated type
@Chg{New=[@i<D> such that either @i<D>],Old=[]} covers the type determined
by the @nt<subtype_mark> of the @nt<subtype_@!indication> or
@nt<qualified_@!expression>@Chg{New=[, or the expected type is anonymous and
the determined type is @i<D>'Class],Old=[]}.@Chg{Version=[3],New=[
A @SynI{subpool_handle_}@nt{name} is expected to be of any type descended from
Subpool_Handle, which is the type used to identify a subpool, declared
in package System.Storage_Pools.Subpools
(see @RefSecNum{Storage Subpools}).@PDefn2{Term=[expected type],Sec=(subpool_handle_name)}],Old=[]}
@begin{Discussion}
  See @RefSec(The Context of Overload Resolution) for the meaning
  of @lquotes@;shall be a single ... type whose ...@rquotes@;
@end{Discussion}
@begin{Ramification}
@ChgRef{Version=[1],Kind=[Added],Ref=[8652/0010],ARef=[AI95-00127-01]}
@Chg{New=[An @nt{allocator} is allowed as a controlling parameter of a dispatching
call (see @RefSecNum{Dispatching Operations of Tagged Types}).],Old=[]}
@end{Ramification}
@end{Resolution}

@begin{Legality}
@Defn{initialized allocator}
An @i(initialized) allocator is an @nt{allocator}
with a @nt{qualified_expression}.
@Defn{uninitialized allocator}
An @i(uninitialized) allocator is one with
a @nt{subtype_indication}.
In the @nt<subtype_indication> of an uninitialized allocator, a
@nt<constraint> is permitted only if the @nt<subtype_mark> denotes an
@Redundant[unconstrained] composite subtype;
if there is no @nt<constraint>, then the @nt<subtype_mark>
shall denote a definite subtype.
@IndexSee{Term=[constructor],See=[initialized allocator]}
@begin{Ramification}
  For example, ... @key[new] S'Class ... (with no initialization
  expression) is illegal,
  but ... @key[new] S'Class'(X) ... is legal,
  and takes its tag and constraints from the initial value X.
  (Note that the former case cannot have a constraint.)
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00287-01]}
If the type of the @nt<allocator> is an access-to-constant type,
the @nt<allocator> shall be an initialized allocator.
@Chg{Version=[2],New=[],Old=[If the designated type is limited,
the @nt<allocator> shall be an uninitialized allocator.]}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[Deleted],ARef=[AI95-00287-01]}
  @ChgDeleted{Version=[2],Text=[For an access-to-constant type whose designated
  type is limited, @nt{allocator}s are illegal.
  The Access attribute is legal for such a type, however.]}
@end{Ramification}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[If a @nt{subpool_specification} is given,
the type of the storage pool of the access type shall be a descendant
of Root_Storage_Pool_With_Subpools.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00344-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded]}@ChgNote{Because the paragraph numbers changed}
@ChgAdded{Version=[2],Text=[If the designated type of the type of the
@nt{allocator} is class-wide, the accessibility level of the type determined by the
@nt{subtype_indication} or @nt{qualified_expression} shall not be statically
deeper than that of the type of the @nt{allocator}.]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents the allocated object from outliving
  its type.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0051-1]}
@ChgAdded{Version=[2],Text=[If the @Chg{Version=[3],New=[subtype determined
by the @nt{subtype_indication} or @nt{qualified_expression}],Old=[designated
subtype of the type]} of the @nt{allocator} has one or more
@Chg{Version=[3],New=[],Old=[unconstrained ]}access discriminants, then the
accessibility level of the anonymous access type of each access
discriminant@Chg{Version=[3],New=[],Old=[,
as determined by the @nt{subtype_indication} or @nt{qualified_expression} of
the @nt{allocator},]} shall not be statically deeper than that of the type
of the @nt{allocator} (see @RefSecNum{Operations of Access Types}).]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This prevents the allocated object from outliving
  its discriminants.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00366-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0052-1],ARef=[AI05-0157-1]}
@ChgAdded{Version=[2],Text=[An @nt{allocator} shall not be of an access type
for which the Storage_Size has been specified by a static expression with value
zero or is defined by the language to be zero. @Chg{Version=[3],New=[],
Old=[@PDefn{generic contract issue}In addition to the places
where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}), this rule applies also in the private
part of an instance of a generic unit. This rule does not apply in the body of
a generic unit or within a body declared within the declarative region of a
generic unit, if the type of the allocator is a descendant of a formal access
type declared within the formal part of the generic unit.]}]}

@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[An @nt{allocator} for an access type that has
  Storage_Size specified to be zero is required to raise Storage_Error anyway.
  It's better to detect the error at compile-time, as the @nt{allocator}
  might be executed infrequently. This also simplifies the rules for Pure
  units, where we do not want to allow any allocators for library-level access
  types, as they would represent state.]}

  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0157-1]}
  @ChgAdded{Version=[2],Text=[@Chg{Version=[3],New=[We don't need a special
  rule to cover generic formals (unlike many other similar @LegalityTitle).
  There are only two cases of interest. For formal access types, the
  Storage_Size property is not known in the generic, and surely isn't
  static, so this @LegalityName can never apply. For a formal derived type,
  this @LegalityName can only be triggered by a parent type having one of
  the appropriate properties. But Storage_Size can never be specified for
  a derived access type, so it always has the same value for all child types;
  additionally, a type derived from a remote access type (which has Storage_Size
  defined to be zero) is also a remote access type. That means that any actual
  that would match the formal derived type necessarily has the same
  Storage_Size properties, so it is harmless (and preferable) to check them
  in the body - they are always known in that case.
  For other formal types,@nt{allocator}s are not allowed, so we don't need
  to consider them. So we don't need an assume-the-best rule
  here.],Old=[The last sentence covers the case of children of
  generics, and formal access types of formal packages of the generic unit.]}]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0052-1]}
@ChgAdded{Version=[3],Text=[If the designated type of the type of the
@nt{allocator} is limited, then
the @nt{allocator} shall not be used to define the value of an access
discriminant, unless the discriminated type is immutably limited
(see @RefSecNum{Limited Types}).]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[Because coextensions work very much like parts,
  we don't want users creating limited coextensions for nonlimited types. This
  would be similar to extending a nonlimited type with a limited component. We
  check this on the @nt{allocator}. Note that there is an asymmetry in what
  types are considered limited; this is required to preserve privacy. We have
  to assume that the designated type might be limited as soon as we see a
  limited partial view, but we want to ensure that the containing object is of
  a type that is always limited.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0052-1]}
@ChgAdded{Version=[3],Text=[@PDefn{generic contract issue}In addition to
the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}), these rules apply also in the private
part of an instance of a generic unit.]}
@begin{Discussion}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This applies to all of the @LegalityTitle
  of this subclause.]}
@end{Discussion}
@end{Legality}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0041-1]}
If the designated type of the type of the @nt<allocator> is elementary,
then the subtype of the created object is the designated
subtype.
If the designated type is composite, then the
@Chg{Version=[2],New=[subtype of the ],Old=[]}created object is
@Chg{Version=[2],New=[the designated
subtype when the designated subtype is constrained or there is
@Chg{Version=[3],New=[an ancestor of the designated type that
has a constrained],Old=[a]} partial
view@Chg{Version=[3],New=[],Old=[ of the designated type that is constrained]};
otherwise, the created],Old=[always constrained;
if the designated subtype is constrained,
then it provides the constraint of the created object;
otherwise, the]} object is constrained by its initial value
@Redundant[(even if the designated subtype is unconstrained with defaults)].
@PDefn{constrained by its initial value}
  @begin{Discussion}
  See AI83-00331.
  @end{Discussion}
  @begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00363-01]}
  All objects created by an @Chg{Version=[2],New=[@nt{allocator}],Old=[allocator]}
  are aliased,
  and @Chg{Version=[2],New=[most],Old=[all]} aliased composite objects
  need to be constrained so that access subtypes work reasonably.
  @Chg{Version=[2],New=[Problematic access subtypes are prohibited for
  types with a constrained partial view.],Old=[]}
  @end{Reason}
  @begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[If there is a constrained partial view of the type, this
    allows the objects to be unconstrained. This eliminates privacy breaking
    (we don't want the objects to act differently simply because they're
    allocated). Such a created object is effectively constrained by its initial
    value if the access type is an access-to-constant type, or the designated
    type is limited (in all views), but we don't need to state that here. It is
    implicit in other rules. Note, however, that a value of an
    access-to-constant type can designate a variable object via 'Access or
    conversion, and the variable object might be assigned by some other access
    path, and that assignment might alter the discriminants.]}
  @end{Discussion}
@end{StaticSem}

@begin{RunTime}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
@PDefn2{Term=[evaluation], Sec=(allocator)}
For the evaluation of an @Chg{Version=[2],New=[initialized allocator],
Old=[@nt<allocator>]}, the @Chg{Version=[2],New=[],Old=[elaboration of
the @nt<subtype_indication> or the ]}evaluation of the
@nt<qualified_expression> is performed first.
@PDefn2{Term=[evaluation], Sec=(initialized allocator)}
@Defn2{Term=[assignment operation], Sec=(during evaluation of an
initialized allocator)}
@Chg{Version=[2],New=[An],Old=[For the evaluation of an initialized allocator,
an]} object of the designated type is created and the value of the
@nt<qualified_expression> is converted to the designated subtype
and assigned to the object.
@PDefn2{Term=[implicit subtype conversion],Sec=(initialization expression of allocator)}
@begin{Ramification}
  The conversion might raise Constraint_Error.
@end{Ramification}

@PDefn2{Term=[evaluation], Sec=(uninitialized allocator)}
@Leading@keepnext@;@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
For the evaluation of an uninitialized allocator@Chg{Version=[2],New=[, the
elaboration of the @nt{subtype_indication} is performed first. Then],Old=[]}:
@begin(itemize)
@Defn2{Term=[assignment operation], Sec=(during evaluation of an
uninitialized allocator)}
  If the designated type is elementary, an object of the
  designated subtype is created and any implicit initial value is assigned;

@ChgRef{Version=[1],Kind=[Revised],Ref=[8652/0002],ARef=[AI95-00171-01]}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00373-01]}
@Chg{Version=[2],New=[],Old=[@Defn2{Term=[assignment operation],
Sec=(during evaluation of an uninitialized allocator)}]}
  If the designated type is composite, an object of the
  designated type is created with tag, if any, determined
  by the @nt<subtype_mark> of the @nt<subtype_indication>@Chg{Version=[2],New=[.
  This object is then initialized by default (see
  @RefSecNum{Object Declarations}) using],Old=[;
  any per-object constraints on subcomponents are elaborated
  @Chg{New=[(see @RefSecNum{Record Types}) ],Old=[]}and any implicit initial
  values for the subcomponents of the object are obtained as determined by]}
  the @nt<subtype_indication>
  @Chg{Version=[2],New=[to determine its nominal subtype],
  Old=[and assigned to the corresponding subcomponents]}.
  @IndexCheck{Index_Check}
  @IndexCheck{Discriminant_Check}
  A check is made that the value of the object belongs to the designated
  subtype.
  @Defn2{Term=[Constraint_Error],Sec=(raised by failure of runtime check)}
  Constraint_Error is raised if this check fails.
  This check and the initialization of the object are performed in
  an arbitrary order.@PDefn2{Term=[arbitrary order],Sec=[allowed]}

@begin{Discussion}
AI83-00150.
@end{Discussion}
@end(itemize)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00344-01],ARef=[AI95-00416-01]}
@ChgRef{Version=[3],Kind=[RevisedAdded],ARef=[AI05-0024-1],ARef=[AI05-0051-1],ARef=[AI05-0234-1]}
@ChgAdded{Version=[2],Text=[For any @nt{allocator}, if the designated type of
the type of the @nt{allocator}
is class-wide, then a check is made that the @Chg{Version=[3],New=[master],
Old=[accessibility level]} of the type
determined by the @nt{subtype_indication}, or by the tag of the value of the
@nt{qualified_expression}, @Chg{Version=[3],New=[includes the elaboration],
Old=[is not deeper than that]} of the type of the @nt{allocator}. If
@Chg{Version=[3],New=[any part of ],Old=[]}the
@Chg{Version=[3],New=[subtype determined by the @nt{subtype_indication} or
@nt{qualified_expression}],Old=[designated subtype]} of the @nt{allocator}
@Chg{Version=[3],New=[(or by the tag of the value if the type of the
@nt{qualified_expression} is class-wide) ],Old=[]}has
one or more @Chg{Version=[3],New=[],Old=[unconstrained ]}access
discriminants, then a check is made that the accessibility
level of the anonymous access type of each access discriminant is
not deeper than that of the type of the @nt{allocator}.
Program_Error is raised
if either such check fails.@IndexCheck{Accessibility_Check}
@Defn2{Term=[Program_Error],Sec=(raised by failure of runtime check)}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
  @ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0024-1]}
  @ChgAdded{Version=[2],Text=[The
  @Chg{Version=[3],New=[master],Old=[accessibility]} check on class-wide types
  prevents the allocated object from outliving its type. We need the run-time
  check in instance bodies, or when the type of the @nt{qualified_expression}
  is class-wide (other cases are statically detected).]}

  @ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0024-1]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[We can't use the normal
    accessibility level @lquotes@;deeper than@rquotes@; check
    here because we may have @lquotes@;incomparable@rquotes@; levels if
    the appropriate master and the type declaration belong to two different
    tasks. This can happen when
    checking the master of the tag for an allocator initialized by
    a parameter passed in to an accept statement, if the type of the allocator
    is an access type declared in the enclosing task body. For example:]}

@begin{Example}
@ChgRef{Version=[3],Kind=[Added]}
@ChgAdded{Version=[3],Text=[@key[task body] TT @key[is]
   @key[type] Acc_TC @key[is access] T'Class;
   P : Acc_TC;
@key[begin]
   @key[accept] E(X : T'Class) @key[do]
      P := @key[new] T'Class'(X);
         @RI[--  Master check on tag of X.]
         @RI[--  Can't use "accessibility levels" since they might be incomparable.]
         @RI[--  Must revert to checking that the master of the type identified by]
         @RI[--  X'tag includes the elaboration of Acc_TC, so it is sure to outlive it.]
   @key[end] E;]}
@end{Example}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[The accessibility check on access discriminants
  prevents the allocated object from outliving its discriminants.]}

@end{Reason}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00280-01]}
@Chg{Version=[2],New=[If the object to be created by an @nt<allocator> has a
controlled or protected part, and the finalization of the collection of the
type of the @nt{allocator} (see @RefSecNum{Completion and Finalization}) has
started, Program_Error is raised.@IndexCheck{Allocation_Check}
@Defn2{Term=[Program_Error],Sec=(raised by failure of runtime check)}],Old=[]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[If the object has a controlled or protected part, its
  finalization is likely to be nontrivial. If the allocation was allowed,
  we could not know whether the finalization would actually be performed.
  That would be dangerous to otherwise safe abstractions, so we mandate
  a check here. On the other hand, if the finalization of the object will
  be trivial, we do not require (but allow) the check, as no real harm
  could come from late allocation.]}
@end{Reason}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This check can only fail if an @nt{allocator}
  is evaluated in code reached from a Finalize routine for a type declared
  in the same master. That's highly unlikely; Finalize routines are much
  more likely to be deallocating objects than allocating them.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00280-01]}
@Chg{Version=[2],New=[If the object to be created by an @nt<allocator>
contains any tasks, and the master of the type of the @nt<allocator> is
completed, and all of the dependent tasks of the master are terminated
(see @RefSecNum{Task Dependence - Termination of Tasks}), then
Program_Error is raised.@IndexCheck{Allocation_Check}
@Defn2{Term=[Program_Error],Sec=(raised by failure of runtime check)}],Old=[]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[A task created after waiting for tasks has
  finished could depend on freed data structures, and certainly would never
  be awaited.]}
@end{Reason}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0111-3]}
@ChgAdded{Version=[3],Text=[If the @nt{allocator} includes a
@SynI{subpool_handle_}@nt{name}, Constraint_Error is raised if the subpool
handle is @key[null]. Program_Error is raised if the subpool does not @i<belong>
(see @RefSecNum{Storage Subpools}) to the storage pool of the access type of the
@nt{allocator}.@IndexCheck{Access_Check}@IndexCheck{Allocation_Check}
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of runtime check)}
@Defn2{Term=[Program_Error],Sec=(raised by failure of runtime check)}]}

@begin{ImplNote}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[This can be implemented by comparing the result of
  Pool_of_Subpool to a reference to the storage pool object. Pool_of_Subpool's
  parameter is @key[not null], so the check for null falls out naturally.]}
@end{ImplNote}

@begin{Reason}
   @ChgRef{Version=[3],Kind=[AddedNormal]}
   @ChgAdded{Version=[3],Text=[This detects cases where the subpool belongs to
   another pool, or to no pool at all. This includes detecting dangling subpool
   handles so long as the subpool object (the object designated by the handle)
   still exists. (If the subpool object has been deallocated, execution is
   erroneous; it is likely that this check will still detect the problem, but
   there cannot be a guarantee.)]}
@end{Reason}

@Redundant[If the created object contains any tasks,
they are activated
(see @RefSecNum(Task Execution - Task Activation)).]
Finally, an access value that designates the created object is returned.
@end{RunTime}

@begin{Bounded}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00280-01]}
@ChgAdded{Version=[2],Text=[@PDefn2{Term=(bounded error),Sec=(cause)} It is a
bounded error if the finalization of the collection of the type (see
@RefSecNum{Completion and Finalization}) of the @nt<allocator> has started. If
the error is detected, Program_Error is raised. Otherwise, the allocation
proceeds normally.@Defn2{Term=[Program_Error],Sec=(raised by detection of a bounded error)}]}
@begin{Discussion}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[This check is required in some cases; see above.]}
@end{Discussion}
@end{Bounded}

@begin{Notes}
Allocators cannot create objects of an abstract type.
See @RefSecNum{Abstract Types and Subprograms}.

If any part of the created object is controlled, the initialization
includes calls on corresponding Initialize or Adjust procedures.
See @RefSecNum{Assignment and Finalization}.

As explained in @RefSec{Storage Management},
the storage for an object allocated by an @nt{allocator} comes from a
storage pool (possibly user defined).
The exception Storage_Error is raised by an @nt<allocator> if there
is not enough storage.
Instances of Unchecked_Deallocation may be used to explicitly reclaim
storage.

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
Implementations are permitted, but not required,
to provide garbage collection@Chg{Version=[3],New=[],Old=[
(see @RefSecNum{Default Storage Pools})]}.
@begin{Ramification}
  Note that in an @nt<allocator>,
  the exception Constraint_Error can be
  raised by the evaluation of the @nt<qualified_expression>,
  by the elaboration of the @nt<subtype_indication>, or by the
  initialization.
@end{Ramification}
@begin{Discussion}
  By default, the implementation provides the storage pool.
  The user may exercise more control over storage management by
  associating a user-defined pool with an access type.
@end{Discussion}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i{Examples of allocators:}
@begin{Example}
@key(new) Cell'(0, @key(null), @key(null))                          @RI[-- initialized explicitly, see @RefSecNum{Incomplete Type Declarations}]
@key(new) Cell'(Value => 0, Succ => @key(null), Pred => @key(null)) @RI[-- initialized explicitly]
@key(new) Cell                                          @RI[-- not initialized]

@key(new) Matrix(1 .. 10, 1 .. 20)                      @RI[-- the bounds only are given]
@key(new) Matrix'(1 .. 10 => (1 .. 20 => 0.0))          @RI[-- initialized explicitly]

@key(new) Buffer(100)                                   @RI[-- the discriminant only is given]
@key(new) Buffer'(Size => 80, Pos => 0, Value => (1 .. 80 => 'A')) @RI[-- initialized explicitly]

Expr_Ptr'(@key(new) Literal)                  @RI[-- allocator for access-to-class-wide type, see @RefSecNum{Type Extensions}]
Expr_Ptr'(@key(new) Literal'(Expression @key[with] 3.5))      @RI[-- initialized explicitly]

@end{Example}
@end{Examples}

@begin{Incompatible83}
@ChgRef{Version=[1],Kind=[Revised]}@ChgNote{Presentation AI-00019}
@Defn{incompatibilities with Ada 83}
The @nt<subtype_indication> of an uninitialized allocator may
not have an explicit @nt<constraint> if the designated type is an access type.
In Ada 83, this was permitted even though the @nt<constraint> had
no @Chg{New=[e],Old=[a]}ffect on the subtype of the created object.
@end{Incompatible83}

@begin{Extend83}
@Defn{extensions to Ada 83}
Allocators creating objects of type @i(T)
are now overloaded on access types designating
@i(T')Class and all class-wide types that cover @i(T).

Implicit array subtype conversion (sliding) is now performed
as part of an initialized allocator.
@end{Extend83}

@begin{DiffWord83}
We have used a new organization, inspired by the ACID
document, that makes it clearer what is the subtype of
the created object, and what subtype conversions take place.

Discussion of storage management issues,
such as garbage collection and the raising of Storage_Error,
has been moved to @RefSec{Storage Management}.
@end{DiffWord83}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00363-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}If the
  designated type has a constrained partial view,
  the allocated object can be unconstrained. This might cause the object to
  take up a different amount of memory, and might cause the operations to work
  where they previously would have raised Constraint_Error. It's unlikely that
  the latter would actually matter in a real program (Constraint_Error usually
  indicates a bug that would be fixed, not left in a program.) The former
  might cause Storage_Error to be raised at a different time than in an Ada 95
  program.]}
@end{Inconsistent95}

@begin{Incompatible95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00366-01]}
  @ChgAdded{Version=[2],Text=[@Defn{incompatibilities with Ada 95}An
  @nt{allocator} for an access type that has Storage_Size specified to be
  zero is now illegal. Ada 95 allowed the @nt{allocator}, but it had to
  raise Storage_Error if executed. The primary impact of this change should
  be to detect bugs.]}
@end{Incompatible95}

@begin{Extend95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0010],ARef=[AI95-00127-01]}
  @ChgAdded{Version=[2],Text=[@Defn{extensions to Ada 95}@b<Corrigendum:> An
  @nt{allocator} can be a controlling parameter of a dispatching call. This
  was an oversight in Ada 95.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00287-01]}
  @ChgAdded{Version=[2],Text=[Initialized @nt{allocator}s are allowed when
  the designated type is limited.]}
@end{Extend95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],Ref=[8652/0002],ARef=[AI95-00171-01]}
  @ChgAdded{Version=[2],Text=[@b<Corrigendum:> Clarified the elaboration of
  per-object constraints for an uninitialized allocator.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00280-01]}
  @ChgAdded{Version=[2],Text=[Program_Error is now raised if the @nt{allocator}
  occurs after the finalization of the collection or the waiting for tasks.
  This is not listed as an incompatibility as the Ada 95 behavior was
  unspecified, and Ada 95 implementations tend to generate programs that crash
  in this case.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00344-01]}
  @ChgAdded{Version=[2],Text=[Added accessibility checks to class-wide
  @nt{allocator}s. These checks could not fail in Ada 95 (as all of the
  designated types had to be declared at the same level, so the access type
  would necessarily have been at the same level or more nested than the type
  of allocated object).]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00373-01]}
  @ChgAdded{Version=[2],Text=[Revised the description of evaluation of
  uninitialized allocators to use @lquotes@;initialized by default@rquotes
  so that the ordering requirements are the same for all kinds of objects
  that are default-initialized.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00416-01]}
  @ChgAdded{Version=[2],Text=[Added accessibility checks to access
  discriminants of @nt{allocator}s. These checks could not fail in Ada 95
  as the discriminants always have the accessibility of the object.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0052-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Added a rule to prevent limited coextensions of nonlimited types. Allowing
  this would have far-reaching implementation costs. Because of those costs, it
  seems unlikely that any implementation ever supported it properly and thus it
  is unlikely that any existing code depends on this capability.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0104-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Added a rule to
  make @nt{null_exclusion}s illegal for uninitialized @nt{allocator}s,
  as such an @nt{allocator} would always raise Constraint_Error.
  Programs that depend on the unconditional raising of a predefined
  exception should be very rare.]}
@end{Incompatible2005}

@begin{Extend2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0111-3]}
  @ChgAdded{Version=[3],Text=[@Defn{extensions to Ada 2005}Subpool handles
  (see @RefSecNum{Storage Subpools}) can be specified in an @nt{allocator}.]}
@end{Extend2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0024-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the master check
  for tags since the masters may be for different tasks and thus incomparable.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0041-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the rules for
  when a designated object is constrained by its initial value so that
  types derived from a partial view are handled properly.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0051-1],ARef=[AI05-0234-1]}
  @ChgAdded{Version=[3],Text=[@b<Correction:> Corrected the accessibility
  check for access discriminants so that it does not depend on the
  designated type (which might not have discriminants when the allocated
  type does).]}
@end{DiffWord2005}



@LabeledClause{Static Expressions and Static Subtypes}

@begin{Intro}
Certain expressions of a scalar or string type are defined to be static.
Similarly, certain discrete ranges are defined to be static, and
certain scalar and string subtypes are defined to be static subtypes.
@Redundant[@Defn{static}
@i(Static) means determinable at compile time,
using the declared properties or values of the program entities.]
@IndexSeeAlso{Term=[constant],See=(static)}
@begin{Discussion}
  As opposed to more elaborate data flow analysis, etc.
@end{Discussion}
@end{Intro}

@begin{MetaRules}
For an expression to be static,
it has to be calculable at compile time.

Only scalar and string expressions are static.

To be static, an expression cannot have any nonscalar, nonstring
subexpressions (though it can have nonscalar constituent @nt<name>s).
A static scalar expression cannot have any nonscalar subexpressions.
There is one exception @em a membership test for a string subtype
can be static, and the result is scalar, even though a subexpression
is nonscalar.

The rules for evaluating static expressions are designed
to maximize portability of static calculations.

@begin{Reason}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0201-1]}
@ChgAdded{Version=[5],Text=[We support static string expressions so that, for
example, the @nt{aspect_definition} for a Link_Name aspect can contain a
concatenation. We don't support static aggregates (even for string types) or
non-string static nonscalar types; we're trying to keep it cheap and simple
(from the implementer's viewpoint).]}
@end{Reason}
@end{MetaRules}

@begin{StaticSem}@Comment{Was "Intro" by mistake until version 5}
@Leading@Defn2{Term=[static], Sec=(expression)}
A static expression is
@Redundant[a scalar or string expression that is]
one of the following:

@begin{Itemize}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0249-1]}
a @nt{numeric_literal}@Chg{Version=[5],New=[ of a numeric type],Old=[]};
@begin{Ramification}
  @ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0249-1]}
  A @nt<numeric_literal>@Chg{Version=[5],New=[ of a numeric type],Old=[]}
  is always a static expression, even if
  its expected type is not that of a static subtype. However, if its value
  is explicitly converted to, or qualified by, a nonstatic subtype,
  the resulting expression is nonstatic.@Chg{Version=[5],New=[ Non-numeric
  types can have numeric literals if aspect Integer_Literal or Real_Literal
  is used; these are never static.],Old=[]}
@end{Ramification}

a @nt{string_literal} of a static string subtype;
@begin(Ramification)
  That is, the constrained subtype defined by the index range
  of the string is static. Note that elementary values don't
  generally have subtypes, while composite values do (since
  the bounds or discriminants are inherent in the value).
@end(Ramification)

a @nt{name}
that denotes the declaration
of a named number or a static constant;
@begin{Ramification}
Note that enumeration
literals are covered by the @nt{function_call} case.
@end{Ramification}

a @nt{function_call}
whose @SynI{function_}@nt{name} or
@SynI{function_}@nt{prefix} statically denotes a static function,
and whose actual parameters, if any (whether given explicitly or by default),
are all static expressions;
@begin{Ramification}
This includes uses of operators that are equivalent to
@nt{function_call}s.
@end{Ramification}


an @nt{attribute_reference} that denotes a scalar value,
and whose @nt{prefix} denotes a static scalar subtype;

@begin{Ramification}
  Note that this does not include the case of an attribute
that is a function;
a reference to
such an attribute is not even an expression.
See above for function @i{calls}.

An implementation may define the staticness and other
properties of implementation-defined attributes.
@end{Ramification}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0368-1]}
an @nt{attribute_reference} whose
@nt{prefix} statically @Chg{Version=[5],New=[names],Old=[denotes]} a
statically constrained array object or array subtype,
and whose @nt<attribute_designator>
is First, Last, or Length,
with an optional dimension;

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0064-2]}
@ChgAdded{Version=[5],Text=[an @nt{attribute_reference} whose
@nt{prefix} denotes a non-generic
entity that is not declared in a generic unit, and whose
@nt{attribute_designator} is Nonblocking;]}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0201-1]}
a @nt{type_conversion}
whose @nt{subtype_mark} denotes a static
@Chg{Version=[5],New=<@Redundant[(scalar or string)]>,Old=[scalar]}
subtype, and whose operand is a static expression;

a @nt{qualified_expression}
whose @nt{subtype_mark} denotes a
static @Redundant[(scalar or string)] subtype,
and whose operand is a static expression;
@begin{Ramification}
This rules out the @nt{subtype_mark}'@nt{aggregate} case.
@end{Ramification}
@begin{Reason}
Adding qualification to an expression shouldn't make it nonstatic, even
for strings.
@end{Reason}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0158-1],ARef=[AI05-0269-1]}
@ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0039-1]}
a membership test whose
@Chg{Version=[4],New=[@SynI{tested_}@nt{simple_expression}],Old=[@nt{simple_expression}]}
is a static expression,
and whose @Chg{Version=[3],New=[@nt{membership_choice_list} consists only of
@nt{membership_choice}s that are either static
@Chg{Version=[4],New=[@SynI{choice_}@nt{simple_expression}s],Old=[@nt{choice_expression}s]},
static @nt{range}s, or @nt{subtype_mark}s that denote],Old=[@nt{range}
is a static range or whose @nt{subtype_mark} denotes]} a
static @Redundant[(scalar or string)] subtype;
@begin{Reason}
Clearly, we should allow membership tests in exactly the same cases
where we allow @nt{qualified_expression}s.
@end{Reason}

a short-circuit control form
both of whose @nt{relation}s are static expressions;

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[a @nt{conditional_expression} all of whose
@nt{condition}s, @SynI{selecting_}@nt{expression}s,
and @SynI{dependent_}@nt{expression}s are static expressions;]}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0368-1]}
@ChgAdded{Version=[5],Text=[a @nt{declare_expression} whose
@Syni{body_}@nt{expression} is static and each of whose
declarations, if any, is either the declaration of a static constant or is
an @nt{object_renaming_declaration} with an @Syni{object_}@nt{name} that
statically names the renamed object;]}

a static expression enclosed in parentheses.
@end{Itemize}
@begin(Discussion)
  @Defn2{Term=[static], Sec=(value)}
  Informally, we talk about a @i(static value). When we do,
  we mean a value specified by a static expression.
@end(Discussion)
@begin{Ramification}
  The language requires a static
  expression in a @nt<number_declaration>,
  a numeric type definition, a @nt<discrete_choice> (sometimes),
  certain representation items, an @nt<attribute_designator>,
  and when specifying the value of a discriminant
  governing a @nt{variant_part}
  in a @nt<record_aggregate> or @nt<extension_aggregate>.
@end{Ramification}

@Leading@Defn{statically denote}
A @nt{name} @i(statically denotes) an entity if it
denotes the entity and:
@begin(itemize)
  It is a @nt<direct_name>, expanded name, or
  @nt{character_literal},
  and it denotes a declaration other than a @nt<renaming_declaration>;
  or

  It is an @nt{attribute_reference} whose @nt{prefix} statically denotes
  some entity; or

  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0322-1]}
  @ChgAdded{Version=[5],Text=[It is a @nt{target_name} (see
  @RefSecNum{Target Name Symbols}) in an @nt{assignment_statement} whose
  @SynI{variable_}@nt{name} statically denotes some entity; or]}

  It denotes a @nt<renaming_declaration> with a @nt<name> that
  statically denotes the renamed entity.
@end(itemize)
@begin{Ramification}
@nt{Selected_component}s that are not expanded names
and @nt{indexed_component}s do not statically denote things.
@end{Ramification}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0368-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[A @nt{name} @i<statically names>
an object if it:@Defn{Statically names}]}
@begin{Itemize}
  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0368-1]}
  @ChgAdded{Version=[5],Text=[statically denotes the declaration of an object
    @Redundant[(possibly through one or more renames)];]}
    @begin{TheProof}
      @ChgRef{Version=[5],Kind=[AddedNormal]}
      @ChgAdded{Version=[5],Text=[Follows from the definition of statically
      denotes.]}
    @end{TheProof}
  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0368-1],ARef=[AI12-0373-1]}
  @ChgAdded{Version=[5],Text=[is a @nt{selected_component} whose prefix
    statically names an object, there is no implicit dereference of the prefix,
    and the @nt{selector_name} does not denote a
    @nt{component_declaration} occurring within a @nt{variant_part}; or]}
    @begin{Reason}
      @ChgRef{Version=[5],Kind=[AddedNormal]}
      @ChgAdded{Version=[5],Text=[We disallow components in a @nt{variant_part}
      so that no discriminant checks are needed to evaluate the
      @nt{selected_component}. Note that other kinds of discriminant-dependent
      components do not need any checks on access (only when they are
      changed).]}
    @end{Reason}
  @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0368-1]}
  @ChgAdded{Version=[5],Text=[is an @nt{indexed_component} whose prefix
    statically names an object, there is no implicit dereference of the prefix,
    the object is statically constrained, and the index expressions of the
    object are static and have values that are within
    the range of the index constraint.]}
@end{Itemize}

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0368-1]}
@ChgAdded{Version=[5],Text=[For an entity other than an object, a @nt{name}
statically names an entity if the @nt{name} statically denotes the entity.]}


@Leading@Defn2{Term=[static], Sec=(function)}
A @i{static function} is one of the following:
@begin{Ramification}
  These are the functions whose calls can be static expressions.
@end{Ramification}
@begin{Itemize}
a predefined operator whose parameter and result
types are all scalar types none of which are descendants of
formal scalar types;

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0201-1]}
@ChgAdded{Version=[5],Text=[a predefined relational operator whose parameters
are of a string type that is not a descendant of a formal array type;]}

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0201-1]}
a predefined concatenation operator whose result type is a string
type@Chg{Version=[5],New=[ that is not a descendant of a formal array
type],Old=[]};

an enumeration literal;

@ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0075-1]}
@ChgAdded{Version=[5],Text=[a static expression function
(see @RefSecNum{Expression Functions});]}

a language-defined attribute that is a function,
if the @nt{prefix} denotes a static scalar subtype,
and if the parameter and result types are scalar.
@end{Itemize}

In any case, a generic formal subprogram is not a static function.

@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0201-1]}
@Defn2{Term=[static], Sec=(constant)}
A @i(static constant) is
a constant view declared by a full constant declaration
or an @nt<object_@!renaming_@!declaration> with a static nominal subtype,
having a value defined by a static scalar expression or by
a static string expression@Chg{Version=[5],New=[],Old=[ whose value has a
length not exceeding the maximum length of a @nt{string_@!literal} in the
implementation]}.
@begin{Ramification}
A deferred constant is not static;
the view introduced by the corresponding full constant declaration
can be static.
@end{Ramification}
@begin{Reason}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0229-1]}
@ChgRef{Version=[5],Kind=[Deleted],ARef=[AI12-0201-1]}
@ChgDeleted{Version=[5],Text=[The reason for restricting the length of static
string constants is so
that compilers don't have to store giant strings in their symbol tables.
Since most string constants will be initialized
from @nt{string_literal}s, the length limit seems pretty natural.
The reason for avoiding nonstring types is also to save symbol table
space.
We're trying to keep it cheap and simple
(from the implementer's viewpoint),
while still allowing, for example,
the @Chg{Version=[3],New=[@nt{aspect_definition} for a Link_Name aspect],
Old=[link name of a pragma Import]} to contain a concatenation.]}

@ChgRef{Version=[5],Kind=[Deleted],ARef=[AI12-0201-1]}
@ChgDeleted{Version=[5],Text=[The length we're talking about is the maximum
number of characters in the value represented by a @nt{string_literal},
not the number of characters in the source representation;
the quotes don't count.]}
@end{Reason}

@Defn2{Term=[static], Sec=(range)}
A @i(static range) is a @nt{range} whose bounds are
static expressions,
@Redundant[or a @nt<range_@!attribute_@!reference> that is equivalent to
such a @nt<range>.]
@Defn2{Term=[static], Sec=(discrete_range)}
A @i(static @nt<discrete_@!range>) is one that is a static range
or is a @nt<subtype_@!indication> that defines a static scalar subtype.
The base range of a scalar type is a static range, unless the
type is a descendant of a formal scalar type.

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00263-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3]}
@Defn2{Term=[static], Sec=(subtype)}
A @i(static subtype) is either a @i(static scalar subtype) or a
@i(static string subtype).
@Defn2{Term=[static], Sec=(scalar subtype)}
A static scalar subtype is an unconstrained scalar subtype whose
type is not a descendant of a
formal @Chg{Version=[2],New=[],Old=[scalar ]}type, or
a constrained scalar subtype formed by imposing a compatible
static constraint on a static scalar subtype.
@Defn2{Term=[static], Sec=(string subtype)}
A static string subtype is an unconstrained string subtype
whose index subtype and component subtype are
static@Chg{Version=[2],New=[],Old=[ (and whose type is not a descendant
of a formal array type)]},
or a constrained string subtype formed by imposing a compatible static
constraint on a static string subtype.
In any case, the subtype of a generic formal object of mode @key[in out],
and the result subtype of a generic formal function, are not static.@Chg{Version=[3],
New=[ Also, a subtype is not static if any Dynamic_Predicate specifications
apply to it.],Old=[]}
@begin{Ramification}
  String subtypes are the only composite subtypes that can be static.
@end{Ramification}
@begin{Reason}
@Leading@;The part about generic formal objects of mode @key[in out]
is necessary because the subtype of the formal is not required
to have anything to do with the subtype of the actual.
For example:
@begin{Example}
@key[subtype] Int10 @key[is] Integer @key[range] 1..10;

@key[generic]
    F : @key[in] @key[out] Int10;
@key[procedure] G;

@key[procedure] G @key[is]
@key[begin]
    @key[case] F @key[is]
        @key[when] 1..10 => @key[null];
        --@RI{ Illegal!}
    @key[end] @key[case];
@key[end] G;

X : Integer @key[range] 1..20;
@key[procedure] I @key[is] @key[new] G(F => X); --@RI{ OK.}
@end{Example}

The @nt{case_statement} is illegal, because the subtype of F is not
static, so the choices have to cover all values of Integer,
not just those in the range 1..10.
A similar issue arises for generic formal functions,
now that function calls are object names.
@end{Reason}

@Leading@Defn2{Term=[static], Sec=(constraint)}
The different kinds of @i(static constraint) are defined as follows:
@begin(itemize)
  A null constraint is always static;

  @Defn2{Term=[static], Sec=(range constraint)}
  @Defn2{Term=[static], Sec=(digits constraint)}
  @Defn2{Term=[static], Sec=(delta constraint)}
  A scalar constraint is static if it has no
  @nt<range_constraint>,
  or one with a static range;

  @Defn2{Term=[static], Sec=(index constraint)}
  An index constraint is static if each
  @nt<discrete_range> is static, and each index subtype of the
  corresponding array type is static;

  @Defn2{Term=[static], Sec=(discriminant constraint)}
  A discriminant constraint is static if
  each @nt<expression> of the constraint is static,
  and the subtype of each discriminant is static.
@end(itemize)

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00311-01]}
@Chg{Version=[2],New=[In any case, the constraint of the first subtype of a
scalar formal type is neither static nor null.],Old=[]}

@Defn{statically constrained}
A subtype is @i(statically constrained) if it is constrained,
and its constraint is static.
An object is @i(statically constrained) if its nominal subtype is
statically constrained,
or if it is a static string constant.
@end{StaticSem}

@begin{Legality}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0147-1]}
@ChgAdded{Version=[3],Type=[Leading],Text=[An expression is @i<statically
unevaluated> if it is part of:@Defn{statically unevaluated}]}

@begin{Itemize}
@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0147-1]}
@ChgAdded{Version=[3],Text=[the right operand of a static short-circuit control
form whose value is determined by its left operand; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[a @SynI{dependent_}@nt{expression} of an
@nt{if_expression} whose
associated @nt{condition} is static and equals False; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[a @nt{condition} or @SynI{dependent_}@nt{expression}
of an @nt{if_expression} where the @nt{condition} corresponding to at least one
preceding @SynI{dependent_}@nt{expression} of the @nt{if_expression} is static
and equals True; or]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[We need this bullet so that only a
  single @SynI{dependent_}@nt{expression} is evaluated in a static
  @nt{if_expression} if there is more than one @nt{condition} that evaluates to
  True. The part about @nt{condition}s makes]}
@begin{Example}
@ChgRef{Version=[3],Kind=[AddedNormal]}
@ChgAdded{Version=[3],Text=[(@key[if] N = 0 @key[then] Min @key[elsif] 10_000/N > Min @key[then] 10_000/N @key[else] Min)]}
@end{Example}
  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[legal if N and Min are static and N = 0.]}
@end{Reason}

@begin{Discussion}
@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
@ChgAdded{Version=[3],Text=[We need the "of the @nt{if_expression}" here so
there is no confusion for nested @nt{if_expression}s; this rule only applies to
the @nt{condition}s and @Syni{dependent_}@nt{expression}s of a single
@nt{if_expression}. Similar
reasoning applies to the "of a @nt{case_expression}" of the last bullet.]}
@end{Discussion}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0188-1],ARef=[AI05-0269-1]}
@ChgAdded{Version=[3],Text=[a @SynI{dependent_}@nt{expression} of a
@nt{case_expression} whose @SynI{selecting_}@nt{expression} is static and
whose value is not covered by the corresponding @nt{discrete_choice_list}; or]}

@ChgRef{Version=[3],Kind=[Added],ARef=[AI05-0158-1]}
@ChgRef{Version=[4],Kind=[RevisedAdded],ARef=[AI12-0039-1]}
@ChgAdded{Version=[3],Text=[a
@Chg{Version=[4],New=[@SynI{choice_}@nt{simple_expression}],Old=[@nt{choice_expression}]}
(or a @nt{simple_expression}
of a @nt{range} that occurs as a @nt{membership_choice} of a
@nt{membership_choice_list}) of a static membership test that is preceded in
the enclosing @nt{membership_choice_list} by another item whose individual
membership test (see @RefSecNum{Relational Operators and Membership Tests})
statically yields True.]}
@end{Itemize}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0147-1]}
@Leading@;A static expression is evaluated at compile time except when it is
@Chg{Version=[3],New=[statically unevaluated],Old=[part
of the right operand of a static short-circuit control form whose value
is determined by its left operand]}.
@Chg{Version=[3],New=[The compile-time],Old=[This]} evaluation
@Chg{Version=[3],New=[of a static expression ],Old=[]}is performed exactly,
without performing Overflow_Checks.
For a static expression that is evaluated:
@begin{Itemize}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0262-1]}
The expression is illegal if its evaluation fails a language-defined
check other than Overflow_@!Check.@Chg{Version=[3],New=[ For the purposes of
this evaluation, the assertion policy is assumed to be Check.],Old=[]}

@begin{Reason}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0262-1]}
  @ChgAdded{Version=[3],Text=[Assertion policies can control whether checks are
  made, but we don't want assertion policies to affect legality. For Ada 2012,
  subtype predicates are the only checks controlled by the assertion policy that
  can appear in static expressions.]}
@end{Reason}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00269-01]}
If the expression is not part of a larger static
expression@Chg{Version=[2],New=[ and the expression is expected to be of a
single specific type],Old=[]},
then its value shall be within the base range of its expected type.
Otherwise, the value may be arbitrarily large or small.
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00269-01]}
  @ChgAdded{Version=[2],Text=[If the expression is expected to be of a universal
  type, or of @lquotes@;any integer type@rquotes, there are no limits on the
  value of the expression.]}
@end{Ramification}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00269-01]}
If the expression is of type @i<universal_real> and its expected type is
a decimal fixed point type,
then its value shall be a multiple of the @i<small> of the decimal
type.@Chg{Version=[2],New=[ This restriction
does not apply if the expected type is a
descendant of a formal scalar type
(or a corresponding actual type in an instance).],Old=[]}
@begin{Ramification}
  This means that a @nt{numeric_literal} for a decimal type cannot have
  @lquotes@;extra@rquotes@; significant digits.
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00269-01]}
  @ChgAdded{Version=[2],Text=[The small is not known for a generic formal
  type, so we have to exclude formal types from this check.]}
@end{Reason}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00269-01]}
@Chg{Version=[2],New=[@PDefn{generic contract issue}
In addition to the places where @LegalityTitle normally apply
(see @RefSecNum{Generic Instantiation}),
the above restrictions also apply in the private part of an
instance of a generic unit.],Old=[The last two restrictions above
do not apply if the expected type is a descendant of a formal scalar type
(or a corresponding actual type in an instance).]}

@begin{Discussion}
  Values outside the base range are not permitted
  when crossing from the @lquotes@;static@rquotes@; domain to the @lquotes@;dynamic@rquotes@; domain.
  This rule is designed to enhance portability of programs
  containing static expressions.
  Note that this rule applies to the exact value,
  not the value after any rounding or truncation.
  (See below for the rounding and truncation requirements.)

  @Leading@;Short-circuit control forms are a special case:
@begin{Example}
N: @key[constant] := 0.0;
X: @key[constant] Boolean := (N = 0.0) @key[or] @key[else] (1.0/N > 0.5); --@RI{ Static.}
@end{Example}

The declaration of X is legal, since the divide-by-zero part of the
expression is not evaluated.
X is a static constant equal to True.

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0075-1]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[The preceding
  @ldquote@;statically unevaluated@rdquote rule allows]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[X : @key[constant] := (@key[if] True @key[then] 37 @key[else] (1 / 0));]}
@end{Example}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[but does not allow]}

@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[function] If_Then_Else (Flag : Boolean; X, Y : Integer) @key[return] Integer @key[is]
   (@key[if] Flag @key[then] X @key[else] Y) @key[with] Static; -- @Examcom{see @RefSecNum{Expression Functions}}
X : @key[constant] := If_Then_Else (True, 37, 1 / 0);]}
@end{Example}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Type=[Trailing],Text=[because evaluation of a function
  call includes evaluation of all of its actual parameters.]}
@end{Discussion}
@begin{Ramification}
  @ChgRef{Version=[2],Kind=[DeletedNoDelMsg],ARef=[AI95-00269-01]}
  @ChgDeleted{Version=[2],Text=[There is no requirement to recheck these rules
  in an instance; the base range check will generally be performed at run time
  anyway.]}
@end{Ramification}
@end{Legality}

@begin{ImplReq}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00268-01],ARef=[AI95-00269-01]}
For a real static expression that is not part of a larger static
expression,
and whose expected type is not a descendant of a formal
@Chg{Version=[2],New=[],Old=[scalar ]}type,
the implementation shall round or truncate
the value (according to the Machine_Rounds
attribute of the expected type) to the nearest machine
number of the expected type;
if the value is exactly half-way between two machine
numbers, @Chg{Version=[2],New=[the],Old=[any]} rounding
@Chg{Version=[2],New=[],Old=[shall be ]}performed
@Chg{Version=[2],New=[is implementation-defined],Old=[away from zero]}.
If the expected type is a descendant of a formal
@Chg{Version=[2],New=[],Old=[scalar ]}type,
@Chg{Version=[2],New=[or if the static expression appears in
the body of an instance of a generic unit and the corresponding expression is
nonstatic in the corresponding generic body, then],Old=[]}
no special rounding or truncating is required @em normal
accuracy rules apply (see @RefSecNum(Numerics)).
@ChgImplDef{Version=[2],Kind=[Added],Text=[@Chg{Version=[2],New=[Rounding of
real static expressions which are exactly half-way between two machine numbers.],Old=[]}]}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00268-01]}
  Discarding extended precision enhances portability
  by ensuring that the value of a
  static constant of a real type is always a machine number of the type.
  @Chg{Version=[2],New=[],Old=[Deterministic rounding of exact halves also enhances portability.]}

  When the expected type is a descendant of a formal floating point type,
  extended precision (beyond that of the machine numbers)
  can be retained when evaluating
  a static expression, to ease code sharing for generic
  instantiations. For similar reasons,
  normal (nondeterministic) rounding or truncating rules apply
  for descendants of a formal fixed point type.

  @ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00269-01]}
  @ChgAdded{Version=[2],Text=[There is no requirement for exact evaluation or
  special rounding in an instance body (unless the expression is static in
  the generic body). This eliminates a potential contract issue where the
  exact value of a static expression depends on the actual parameters (which
  could then affect the legality of other code).]}
@end{Reason}
@begin{ImplNote}

  Note that the implementation of static expressions has to keep track
  of plus and minus zero for a type whose Signed_Zeros attribute is
  True.

  @ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00100-01]}
  Note that the only @Chg{Version=[2],New=[machine numbers],Old=[values]} of
  a fixed point type are the multiples of
  the small, so a static conversion to a fixed-point type, or division
  by an integer, must do truncation to a multiple of small.
  It is not correct for the implementation to do all static calculations
  in infinite precision.

@end{ImplNote}

@end{ImplReq}

@begin{ImplAdvice}
@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00268-01]}
@ChgAdded{Version=[2],Text=[For a real static expression that is not part of a
larger static expression, and whose expected type is not a descendant of a
formal type, the rounding should be the same as the default rounding
for the target system.]}
@ChgImplAdvice{Version=[2],Kind=[AddedNormal],Text=[@ChgAdded{Version=[2],
Text=[A real static expression with a nonformal type that is not part of a
larger static expression should be rounded the same as the target system.]}]}
@end{ImplAdvice}

@begin{Notes}
An expression can be static even if it occurs in a context where
staticness is not required.
@begin{Ramification}

@Leading@keepnext@;For example:
@begin{Example}
X : Float := Float'(1.0E+400) + 1.0 - Float'(1.0E+400);
@end{Example}

The expression is static,
which means that the value of X must be exactly 1.0,
independent of the accuracy or range of the run-time floating point
implementation.

The following kinds of expressions are never static:
@nt{explicit_dereference},
@nt{indexed_component},
@nt{slice},
@key{null},
@nt{aggregate},
@nt{allocator}.
@end{Ramification}

A static (or run-time) @nt<type_conversion> from a real type to
an integer type performs rounding. If the operand value is exactly half-way
between two integers, the rounding is performed away from zero.
@begin{Reason}
  We specify this for portability. The reason for not choosing
  round-to-nearest-even, for example, is that this method is easier
  to undo.
@end{Reason}
@begin{Ramification}
  The attribute Truncation
  (see @RefSecNum{Attributes of Floating Point Types})
  can be used to perform a (static) truncation prior to conversion,
  to prevent rounding.
@end{Ramification}
@begin{ImplNote}

  The value of the literal
  0E999999999999999999999999999999999999999999999
  is zero.
  The implementation must take care to evaluate such literals properly.

@end{ImplNote}
@end{Notes}

@begin{Examples}
@Leading@keepnext@i(Examples of static expressions:)
@begin{Example}
1 + 1       @RI[-- 2]
@key(abs)(-10)*3  @RI[-- 30]

Kilo : @key(constant) := 1000;
Mega : @key(constant) := Kilo*Kilo;   @RI[-- 1_000_000]
Long : @key(constant) := Float'Digits*2;

Half_Pi    : @key(constant) := Pi/2;           @RI[-- see @RefSecNum(Number Declarations)]
Deg_To_Rad : @key(constant) := Half_Pi/90;
Rad_To_Deg : @key(constant) := 1.0/Deg_To_Rad; @RI[-- equivalent to 1.0/((3.14159_26536/2)/90)]
@end{Example}
@end{Examples}

@begin{Extend83}
@Defn{extensions to Ada 83}
The rules for static expressions and static subtypes are generalized
to allow more kinds of compile-time-known expressions to be used
where compile-time-known values are required, as follows:
@begin(itemize)
Membership tests and short-circuit control forms
may appear in a static expression.

The bounds and length of
statically constrained array objects or subtypes are static.

The Range attribute of a statically constrained array subtype or
object gives a static range.

A @nt{type_conversion} is static if the @nt{subtype_mark} denotes a
static scalar subtype and the operand is a static expression.

All numeric literals are now static, even if the expected
type is a formal scalar type.
This is useful in @nt{case_statement}s and @nt{variant_part}s,
which both now allow a value of a formal scalar type to
control the selection, to
ease conversion of a package into a generic package.
Similarly, named array aggregates are also permitted for array
types with an index type that is a formal scalar type.
@end(itemize)

The rules for the evaluation of static expressions
are revised to require exact evaluation at compile time,
and force a machine number result when crossing from the static realm
to the dynamic realm,
to enhance portability and predictability.
Exact evaluation is not required for
descendants of a formal scalar type,
to simplify generic code sharing and to avoid generic
contract model problems.

@Leading@;Static expressions are legal even if an intermediate
in the expression goes outside the base range of the type. Therefore, the
following will succeed in Ada 95, whereas it might raise an
exception in Ada 83:
@begin{Example}
@key[type] Short_Int @key[is] @key[range] -32_768 .. 32_767;
I : Short_Int := -32_768;
@end{Example}

This might raise an exception in Ada 83 because "32_768" is out of range,
even though "@en@;32_768" is not. In Ada 95, this will always succeed.


Certain expressions involving string operations
(in particular concatenation and membership tests)
are considered static in Ada 95.


The reason for this change is to simplify the rule requiring
compile-time-known string expressions as the link name in an interfacing
pragma, and to simplify the preelaborability rules.
@end{Extend83}

@begin{Incompatible83}
@Defn{incompatibilities with Ada 83}
An Ada 83 program that uses an out-of-range static value
is illegal in Ada 95, unless the expression is part of a larger
static expression, or the expression is not evaluated due to being on
the right-hand side of a short-circuit control form.
@end{Incompatible83}

@begin{DiffWord83}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0299-1]}
This @Chg{Version=[3],New=[subclause],Old=[clause]}
(and @RefSec{Multiplying Operators})
subsumes the RM83 section on Universal Expressions.

The existence of static string expressions
necessitated changing the definition of static subtype to
include string subtypes.
Most occurrences of "static subtype" have been changed to "static scalar
subtype",
in order to preserve the effect of the Ada 83 rules.
This has the added benefit of clarifying the difference between "static
subtype" and "statically constrained subtype", which has been a source
of confusion.
In cases where we allow static string subtypes,
we explicitly use phrases like "static string subtype"
or "static (scalar or string) subtype",
in order to clarify the meaning for those who have gotten used to the
Ada 83 terminology.


@Leading@;In Ada 83, an expression was considered nonstatic if it raised an
exception.
Thus, for example:
@begin{Example}
Bad: @key[constant] := 1/0; --@RI{ Illegal!}
@end{Example}

was illegal because 1/0 was not static.
In Ada 95, the above example is still illegal,
but for a different reason:
1/0 is static, but there's a separate rule forbidding the exception
raising.

@end{DiffWord83}

@begin{Inconsistent95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00268-01]}
  @ChgAdded{Version=[2],Text=[@Defn{inconsistencies with Ada 95}
  @b[Amendment Correction:] Rounding of
  static real expressions is implementation-defined in Ada 2005, while it was
  specified as away from zero in (original) Ada 95. This could make subtle
  differences in programs. However, the original Ada 95 rule required rounding
  that (probably) differed from the target processor, thus creating anomalies
  where the value of a static expression was required to be different than the
  same expression evaluated at run time.]}
@end{Inconsistent95}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00263-01],ARef=[AI95-00268-01]}
  @ChgAdded{Version=[2],Text=[The Ada 95 wording that defined static subtypes
  unintentionally failed to exclude formal derived types that happen to be
  scalar (these aren't formal scalar types); and had a parenthetical remark
  excluding formal string types - but that was neither necessary nor
  parenthetical (it didn't follow from other wording). This issue also
  applies to the rounding rules for real static expressions.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00269-01]}
  @ChgAdded{Version=[2],Text=[Ada 95 didn't clearly define the bounds of a value of
  a static expression for universal types and for "any integer/float/fixed
  type". We also make it clear that we do not intend exact evaluation of
  static expressions in an instance body if the expressions aren't static in the
  generic body.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00311-01]}
  @ChgAdded{Version=[2],Text=[We clarify that the first subtype of a scalar
  formal type has a nonstatic, nonnull constraint.]}
@end{DiffWord95}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0147-1],ARef=[AI05-0188-1]}
  @ChgAdded{Version=[3],Text=[Added wording to define staticness and
  the lack of evaluation for @nt{if_expression}s and @nt{case_expression}s.
  These are new and defined elsewhere.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
  @ChgAdded{Version=[3],Text=[Added wording to prevent subtypes that have
  dynamic predicates (see @RefSecNum{Subtype Predicates}) from being static.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0158-1]}
  @ChgAdded{Version=[3],Text=[Revised wording for membership tests to allow
  for the new possibilities allowed by the @nt{membership_choice_list}.]}
@end{DiffWord2005}

@begin{Incompatible2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0201-1]}
  @ChgAdded{Version=[5],Text=[@Defn{incompatibilities with Ada 2012}
  Added a missing exclusion for concatenations of a string type descended from
  a formal array type. This could potentially make some expression non-static;
  but as that could only matter in a context where a static string is required
  (such as the Link_Name aspect), it is quite unlikely.]}
@end{Incompatible2012}

@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0201-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Expressions
  involving string relational operators or string type conversions now can be
  static.]}
@end{Extend2012}

@begin{Diffword2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0064-2]}
  @ChgAdded{Version=[5],Text=[Defined the staticness of the Nonblocking
  attribute (see @RefSecNum{Intertask Communication}).]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0075-1]}
  @ChgAdded{Version=[5],Text=[Expression functions can be static if declared
  correctly; this is documented as an extension in @RefSecNum{Expression Functions}.]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0249-1]}
  @ChgAdded{Version=[5],Text=[A @nt{numeric_literal} can be non-static if
  they are defined by an Integer_Literal or Real_Literal aspect
  (see @RefSecNum{User-Defined Literals}).]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0322-1]}
  @ChgAdded{Version=[5],Text=[Clarified that a target name symbol can statically
  denote an entity if the associated @SynI{variable_}@nt{name} statically
  denotes an entity. This is necessary so that target names participate in
  the anti-order-dependence checks of @RefSecNum{Parameter Associations}.]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI12-0368-1],ARef=[AI12-0373-1]}
  @ChgAdded{Version=[3],Text=[Added wording to define staticness for
  @nt{declare_expression}s. Also moved @ldquote@;statically names@rdquote@;
  definition here and used it in array attribute prefix wording.]}

@end{Diffword2012}


@NotISORMNewPageVer{Version=[3]}@Comment{For printed version of Ada 2012 RM}
@LabeledSubClause{Statically Matching Constraints and Subtypes}

@begin{StaticSem}
@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00311-01]}
@Defn2{Term=[statically matching], Sec=(for constraints)}
A constraint @i(statically matches) another
constraint if@Chg{Version=[2],New=[:],Old=[ both are null constraints, both are
static and have equal corresponding bounds or discriminant values,
or both are nonstatic and result from the same elaboration of
a @nt<constraint>
of a @nt<subtype_@!indication> or the same evaluation of a @nt<range>
of a @nt<discrete_@!subtype_@!definition>.]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}

@Chg{Version=[2],New=[both are null constraints;],Old=[]}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[A null constraint has nothing to do with
  null exclusions! Unconstrained array subtypes, subtypes with unknown
  discriminants, and subtypes with no explicit constraint have null
  constraints (see @RefSecNum{Types and Subtypes}). This terminology
  became confusing when null exclusions were introduced in the
  2007 Amendment.]}
@end{Discussion}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[both are static and have equal corresponding bounds or discriminant
values;],Old=[]}

@ChgRef{Version=[2],Kind=[Added]}
@Chg{Version=[2],New=[both are nonstatic and result from the same elaboration
of a @nt<constraint>
of a @nt<subtype_@!indication> or the same evaluation of a @nt<range>
of a @nt<discrete_@!subtype_@!definition>; or],Old=[]}

@ChgRef{Version=[2],Kind=[Added],ARef=[AI95-00311-01]}
@Chg{Version=[2],New=[both are nonstatic and come from the same
@nt{formal_type_declaration}.],Old=[]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Revised],ARef=[AI95-00231-01],ARef=[AI95-00254-01]}
@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0153-3]}
@ChgRef{Version=[5],Kind=[Revised],ARef=[AI12-0059-1]}
@Defn2{Term=[statically matching], Sec=(for subtypes)}
A subtype @i(statically matches) another subtype of the same type
if they have statically matching constraints@Chg{Version=[2],New=[,
@Chg{Version=[3],New=[all predicate specifications that apply to
them come from the same declarations, @Chg{Version=[5],New=[Object_Size (see
@RefSecNum{Operational and Representation Attributes}) has been specified to
have a nonconfirming value for either both or neither, and the nonconfirming
values, if any, are the same, ],Old=[]}],Old=[]}and, for
access subtypes, either both or neither exclude null],Old=[]}.
Two anonymous access@Chg{Version=[2],New=[-to-object],Old=[]} subtypes
statically match if their designated subtypes statically
match@Chg{Version=[2],New=[, and either both or neither
exclude null, and either both or neither are access-to-constant. Two anonymous
access-to-subprogram subtypes statically match if their designated profiles are
subtype conformant, and either both or neither exclude null],Old=[]}.
@begin{Ramification}
  Statically matching constraints and subtypes are the basis
  for subtype conformance of profiles (see @RefSecNum(Conformance Rules)).
@end{Ramification}
@begin{Reason}
  @ChgRef{Version=[2],Kind=[AddedNormal]}
  @ChgAdded{Version=[2],Text=[Even though anonymous access types always
  represent different types, they can statically match. That's important so
  that they can be used widely. For instance, if this wasn't true, access
  parameters and access discriminants could never conform, so they couldn't
  be used in separate specifications.]}
@end{Reason}
@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0059-1]}
  @ChgAdded{Version=[5],Text=[If one of the subtypes is not yet frozen, an
  implementation may have to repeat the check when the subtypes are both
  frozen (as it is impossible to check the Object_Size part before the subtypes
  are frozen). This recheck can only make a previously statically matching
  subtype fail to match; it cannot make a match legal.]}
@end{Ramification}
@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0059-1]}
  @ChgAdded{Version=[5],Text=[We exclude the case where both Object_Sizes are
  confirming so that we don't introduce an incompatibility for
  existing Ada code. But practically the implementation can simply
  check that the Object_Size values are the same, as we have a rule in
  @RefSecNum{Operational and Representation Aspects} that the
  subtype-specific aspects (such as Object_Size) are always the same for
  statically matching subtypes. We wrote the rules this way to avoid having
  wording that appeared to require predicting the future ("would statically
  match if ...").]}
  @Comment{We generally do not reference particular paragraphs in the AARM;
  the paragraph referred to above is paragraph 14. In part, this is because
  we don't have a way in this tool to link to a particular paragraph,
  even though a user could add #P14 to the appropriate link and get directly
  there.}
@end{Discussion}

@Defn2{Term=[statically matching], Sec=(for ranges)}
Two ranges of the same type @i{statically match} if both result
from the same evaluation of a @nt{range},
or if both are static and have equal corresponding bounds.
@begin{Ramification}
The notion of static matching of ranges is used in
@RefSec{Formal Array Types};
the index ranges of formal and actual constrained array subtypes have to
statically match.
@end{Ramification}

@ChgRef{Version=[3],Kind=[Revised],ARef=[AI05-0086-1],ARef=[AI05-0153-3]}
@Defn2{Term=[statically compatible],
  Sec=(for a constraint and a scalar subtype)}
A constraint is @i(statically compatible) with a scalar subtype if
it statically matches the constraint of the subtype, or if both
are static and the constraint is compatible with the subtype.
@Defn2{Term=[statically compatible],
  Sec=(for a constraint and an access or composite subtype)}
A constraint is @i(statically compatible) with an access or composite subtype
if it statically matches the constraint of the subtype, or
if the subtype is unconstrained.@Chg{Version=[3],New=[],Old=[
@Defn2{Term=[statically compatible],
  Sec=(for two subtypes)}
One subtype is @i(statically compatible) with a second subtype if
the constraint of the first is statically compatible with the
second subtype.]}
@begin{Discussion}
  Static compatibility is required when constraining a parent subtype
  with a discriminant from a new @nt<discriminant_part>.
  See @RefSecNum{Discriminants}. Static compatibility is also used
  in matching generic formal derived types.

  Note that
  statically compatible with a subtype does not imply
  compatible with a type. It is OK since the terms are
  used in different contexts.
@end{Discussion}

@ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3]}
@ChgAdded{Version=[3],Type=[Leading],Text=[@Defn2{Term=[statically compatible],
  Sec=(for two subtypes)}Two statically matching subtypes are statically
compatible with each other. In addition, a subtype @i<S1> is statically
compatible with a subtype @i<S2> if:]}
@begin{Itemize}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Text=[the constraint of @i<S1> is statically compatible
  with @i<S2>, and]}

  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0086-1]}
  @ChgAdded{Version=[3],Text=[if @i<S2> excludes null, so does @i<S1>, and]}

  @ChgRef{Version=[3],Kind=[AddedNormal]}
  @ChgAdded{Version=[3],Type=[Leading],Text=[either:]}
  @begin{InnerItemize}
    @ChgRef{Version=[3],Kind=[AddedNormal]}
    @ChgAdded{Version=[3],Text=[all predicate specifications that apply to
    @i<S2> apply also to @i<S1>, or]}

    @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0290-1]}
    @ChgRef{Version=[4],Kind=[Revised],ARef=[AI12-0071-1]}
    @ChgAdded{Version=[3],Text=[both subtypes are static, every value that
    satisfies the @Chg{Version=[4],New=[predicates],Old=[predicate]} of @i<S1>
    also satisfies the @Chg{Version=[4],New=[predicates],Old=[predicate]} of
    @i<S2>, and it is not the case that both types each have at least one
    applicable predicate specification, predicate checks are enabled (see
    @RefSecNum{Pragmas Assert and Assertion_Policy}) for @i<S2>, and
    predicate checks are not enabled for @i<S1>.]}
  @end{InnerItemize}
@end{Itemize}
@end{StaticSem}

@begin{DiffWord83}
This subclause is new to Ada 95.
@end{DiffWord83}

@begin{DiffWord95}
  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00231-01],ARef=[AI95-00254-01]}
  @ChgAdded{Version=[2],Text=[Added static matching rules for null exclusions
  and anonymous access-to-subprogram types; both of these are new.]}

  @ChgRef{Version=[2],Kind=[AddedNormal],ARef=[AI95-00311-01]}
  @ChgAdded{Version=[2],Text=[We clarify that the constraint of the first
  subtype of a scalar formal type statically matches itself.]}
@end{DiffWord95}

@begin{Incompatible2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0086-1]}
  @ChgAdded{Version=[3],Text=[@Defn{incompatibilities with Ada 2005}@b<Correction:>
  Updated the statically compatible rules to take null exclusions into account.
  This is technically incompatible, as it could cause a legal Ada 2005 program
  to be rejected; however, such a program violates the intent of the rules
  (for instance, @RefSecNum{Discriminants}(15)) and this probably will simply
  detect bugs.]}
@end{Incompatible2005}

@begin{DiffWord2005}
  @ChgRef{Version=[3],Kind=[AddedNormal],ARef=[AI05-0153-3],ARef=[AI05-0290-1]}
  @ChgAdded{Version=[3],Text=[Modified static matching and static compatibility
  to take predicate aspects (see @RefSecNum{Subtype Predicates}) into account.]}
@end{DiffWord2005}

@begin{DiffWord2012}
  @ChgRef{Version=[4],Kind=[AddedNormal],ARef=[AI12-0071-1]}
  @ChgAdded{Version=[4],Text=[@b<Corrigendum:> Updated wording of
  static compatibility to use the new term "satisfies the predicates"
  (see @RefSecNum{Subtype Predicates}).]}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0059-1]}
  @ChgAdded{Version=[5],Text=[Updated wording to take nonconfirming values of
  Object_Size into account.]}
@end{Diffword2012}



@LabeledAddedClause{Version=[5],Name=[Image Attributes]}


@begin{Intro}
@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0315-1]}
@ChgAdded{Version=[5],Text=[An @i{image} of a value is a string representing
  the value in display form.@Defn2{Term=[image], Sec=(of a value)}
  The attributes Image, Wide_Image, and Wide_Wide_Image are available
  to produce the image of a value as a String, Wide_String, or
  Wide_Wide_String (respectively). User-defined images for a given type
  can be implemented by overriding the default implementation of the
  attribute Put_Image.]}
@end{Intro}

@begin{StaticSem}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1],ARef=[AI12-0315-1]}
@ChgAdded{Version=[5],Type=[Leading],Keepnext=[T],Text=[For @PrefixType{every
  subtype S of a type T other than @i{universal_real} or @i{universal_fixed}},
  the following type-related operational attribute is defined:]}

@begin(description)

@ChgAttribute{Version=[5],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Put_Image>, ARef=[AI12-0020-1], ARef=[AI12-0320-1], ARef=[AI12-0340-1],
  InitialVersion=[5], Text=[@Chg{Version=[5],New=[S'Put_Image denotes a
     procedure with the following specification:],Old=[]}
@begin(Descexample)
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[@b(procedure) S'Put_Image
   (@RI(Buffer) : @key[in out] Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    @RI(Arg)    : @key[in] T);]}
@end(Descexample)
@Comment{These two paragraphs have to be Added rather than AddedNormal so that
the paragraphs in thee Annex have the correct paragraph numbers.}
    @ChgRef{Version=[5],Kind=[Added],ARef=[AI12-0315-1],ARef=[AI12-0340-1]}
    @ChgAdded{Version=[5],NoPrefix=[T],Text=[The
      default implementation of S'Put_Image writes
      (using Wide_Wide_Put) an @i<image> of the value of
      @i<Arg>.]}]}@Comment{End of Annex text here.}

@EndPrefixType{}
@end{Description}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],NoPrefix=[T],Text=[The Put_Image attribute may be
specified for any specific type T either via an @nt{attribute_definition_clause}
or via an @nt{aspect_specification} specifying the Put_Image aspect of the
type.@AspectDefn{Put_Image}]}

@ChgAspectDesc{Version=[5],Kind=[AddedNormal],Aspect=[Put_Image],
  Text=[@ChgAdded{Version=[5],Text=[Procedure to define the image of a
    given type.]}]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[In contrast, the Image, Wide_Image, and
     Wide_Wide_Image attributes and their associated aspects can not be
     specified. The behavior of any of these attributes is defined in terms of
     calls to the corresponding Put_Image procedure, so changes in their
     behavior may be accomplished via a Put_Image specification.]}

   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[In earlier versions of Ada, Image and related
     attributes were defined only for scalar types. The definition of these
     attributes is now very different, but there should be
     no change in the behavior of existing programs as a result of these
     changes.]}
@end{Discussion}


@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1],ARef=[AI12-0340-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[The behavior of the default
  implementation of S'Put_Image depends on the class of T. For
  an elementary type, the implementation is equivalent to:]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[procedure] Scalar_Type'Put_Image
  (Buffer : @key[in out] Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
   Arg    : @key[in] Scalar_Type) @key[is]
@key[begin]
   Buffer.Wide_Wide_Put (@i{<described below>});
@key[end] Scalar_Type'Put_Image;]}
@end{Example}

@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Type=[Leading],Text=[where the Wide_Wide_String value
  written out to the stream is defined as follows:]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For an integer type, the image written out is the
  corresponding decimal literal, without underlines, leading zeros, exponent, or
  trailing spaces, but with a single leading character that is either a minus
  sign or a space.]}

@begin{ImplNote}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[If the machine supports negative zeros for signed
     integer types, it is not specified whether " 0" or "-0" should be returned
     for negative zero. We don't have enough experience with such machines to
     know what is appropriate, and what other languages do. In any case, the
     implementation should be consistent.]}
@end{ImplNote}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[We allow S'Put_Image when S is
   @i{universal_integer} or @i{root_integer}, because the details of the
   desired string do not depend on properties of an integer type. While
   S'Put_Image cannot be called directly for these types (as they cannot be
   named), it can be called as part of evaluating an Image attribute. Note
   that other rules of the language ensure that an implementation can evaluate
   any @i{universal_integer} attribute using type @i{root_integer}; therefore,
   Constraint_Error could be raised by the evaluation of an Image attribute
   if the value of the prefix is outside of the range of
   @i{root_integer}.]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For an enumeration type, the image written out is
  either the corresponding identifier in upper case or the corresponding
  character literal (including the two apostrophes); neither leading nor
  trailing spaces are included. For a @i{nongraphic character} (a value of a
  character type that has no enumeration literal associated with it), the value
  is a corresponding language-defined name in upper case (for example, the image
  of the nongraphic character identified as @i{nul} is @exam{"NUL"} @em the
  quotes are not part of the image).@Defn{nongraphic character}]}

@begin{ImplNote}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[For an enumeration type T that has "holes"
     (caused by an enumeration_representation_clause), T'Put_Image should raise
     Program_Error if the value is one of the holes (which is a bounded error
     anyway, since holes can be generated only via uninitialized variables and
     similar things).]}
@end{ImplNote}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For a floating point type, the image written out is
  a decimal real literal best approximating the value (rounded away from zero
  if halfway between) with a single leading character that is either a minus
  sign or a space, a single digit (that is nonzero unless the value is zero), a
  decimal point, S'Digits-1 (see @RefSecNum{Operations of Floating Point Types})
  digits  after the decimal point (but one if
  S'Digits is one), an upper case E, the sign of the exponent (either + or -),
  and two or more digits (with leading zeros if necessary) representing the
  exponent. If S'Signed_Zeros is True, then the leading character is a minus sign
  for a negatively signed zero.]}

@begin{Honest}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[Leading zeros are present in the exponent only
     if necessary to make the exponent at least two digits.]}
@end{Honest}

@begin{Reason}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[This image is intended to conform to that
     produced by Text_IO.Float_IO.Put in its default format.]}
@end{Reason}

@begin{ImplNote}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[The rounding direction is specified here to
      ensure portability of output results.]}
@end{ImplNote}

@begin{Reason}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0315-1]}
   @ChgAdded{Version=[5],Text=[We do not allow S'Put_Image when S is
   @i{universal_real}, as the details of the desired string depend on the
   properties of the (specific) type of S. Specifically, @i{universal_real}
   does not have a defined value for S'Digits.]}
@end{Reason}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For a fixed point type, the image written out is a
  decimal real literal best approximating the value (rounded away from zero if
  halfway between) with a single leading character that is either a minus sign
  or a space, one or more digits before the decimal point (with no redundant
  leading zeros), a decimal point, and
  S'Aft (see @RefSecNum{Operations of Fixed Point Types}) digits after the
  decimal point.]}

@begin{Reason}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[This image is intended to conform to that produced by
      Text_IO.Fixed_IO.Put.]}
@end{Reason}

@begin{ImplNote}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[The rounding direction is specified here to
      ensure portability of output results.]}

   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[For a machine that supports negative zeros,
      it is not specified whether " 0.000" or "-0.000" is returned. See
      corresponding comment above about integer types with signed zeros.]}
@end{ImplNote}

@begin{Reason}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0315-1]}
   @ChgAdded{Version=[5],Text=[We do not allow S'Put_Image when S is
   @i{universal_fixed}, as the details of the desired string depend on the
   properties of the (specific) type of S. Specifically, @i{universal_fixed}
   does not have a defined value for S'Aft.]}
@end{Reason}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For an access type (named or anonymous), the image
  written out depends on whether the value is @key[null]. If it is @key[null],
  then the image is @exam{"NULL"}. Otherwise the image is a left parenthesis
  followed by @exam{"ACCESS"}, a space, and a sequence of graphic characters,
  other than space or right parenthesis, representing the location of the
  designated object, followed by a right parenthesis, as in @exam{"(ACCESS
  FF0012AC)"}.]}

@begin{Honest}
   @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0315-1]}
   @ChgAdded{Version=[5],Text=[S'Put_Image is defined for @i{universal_access},
   but it can never be called (as no legal @nt{prefix} of Image has that type,
   and that type cannot be named preventing direct calls).]}
@end{Honest}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=<For an array type T, the default implementation of
  T'Put_Image generates an image based on (named, not positional) array
  aggregate syntax (with '[' and ']' as the delimiters) using calls to the
  Put_Image procedures of the index type(s) and the element type to generate
  images for values of those types.>}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[In general, the default implementation of
     T'Put_Image for a composite type will involve some sequence of calls to
     Wide_Wide_String'Write and calls to the Put_Image procedures of component
     types and, in the case of an array type, index types. The
     Wide_Wide_String'Write calls may pass in either literal values (e.g., "(",
     ")", "'(", " => ", or ", "), or other things (such as component names for
     record values, task_id images for tasks, or the Wide_Wide_Expanded_Name of
     the tag in the class-wide case).]}
@end{Discussion}

@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Type=[Leading],Text=[An array type might generate
    an image such as:]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text={[ 1 => [ 1 => [ 123 => True,  124 => False]
         2 => [ 123 => False,  124 => False]],
  2 => [ 1 => [ 123 => True,  124 => True],
         2 => [ 123 => True,  124 => False]]]}}
@end{example}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[although perhaps with different white space
      and/or line breaking.]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[The case of a null array is handled specially, using
  ranges for index bounds and @exam{"<>"} as a syntactic component-value
  placeholder.]}

@begin{Discussion}
   @ChgRef{Version=[5],Kind=[AddedNormal]}
   @ChgAdded{Version=[5],Text=[This might generate an image such as
     @exam{"[ 1 ..  3 => [ 1 ..  0 => [ 1 .. 5 => <>]]]"}, where the use
     of "<>" (among other things) indicates that the overall array is a
     null array and has no actual elements.]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[The order in which components are written for a
  composite type is the same canonical order in which components of a composite
  type T are written out by the default implementation of T'Write.
  @Redundant[This is also the order that is used in determining the meaning of a
  positional aggregate of type T.]]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For a class-wide type, the default implementation of
  T'Put_Image generates an image based on qualified expression syntax.
  Wide_Wide_String'Write is called with Wide_Wide_Expanded_Name of @i{Arg}'Tag.
  Then S'Put_Image is called, where S is the specific type identified by
  @i{Arg}'Tag.]}

@begin{ImplNote}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[This will typically require a dispatching
      call.]}
@end{ImplNote}

@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Type=[Leading],Text=[This might generate an image such as:]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text={SOME_PACKAGE.SOME_TAGGED_TYPE'
   (COMPONENT_1 =>  123, COMPONENT_2 => 456)}}
@end{Example}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[The parentheses in this case are generated by
      the call to Some_Tagged_Type'Put_Image.]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For a type extension, the default implementation of
  T'Put_Image depends on whether there exists a noninterface ancestor of T
  (other than T itself) for which the Put_Image aspect has been
  @Redundant[explicitly] specified. If so, then T'Put_Image will generate an
  image based on extension aggregate syntax where the ancestor type of the
  extension aggregate is the nearest ancestor type whose Put_Image aspect has
  been specified.]}

@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[This might generate an image such as
       @exam{"(This Text Was User-Generated with C1 =>  123, C2 =>  456)"}
       where the "This Text was User-Generated" portion of the text was
       generated by the call to the user-specified Put_Image routine.]}
@end{Discussion}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[If no such ancestor exists, then the default
  implementation of T'Put_Image is the same as described below for an untagged
  record type.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For an untagged record type, a specific tagged
  record type other than a type extension which meets the criteria described in
  the previous paragraph, or a protected type, the default implementation of
  T'Put_Image generates an image based on (named, not positional) record
  aggregate syntax (except that for a protected type, the initial left
  parenthesis is followed by @exam{"PROTECTED with "}). Component names are
  displayed in upper case, following the rules for the image of an enumeration
  value. Component values are displayed via calls to the component type's
  Put_Image procedure.]}

@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Type=[Leading],Text=[This might generate an image such as:]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text={"(FOO => [1 => 'c',  2 => 'a',  3 => 't'], BAR => TRUE)"}}
@end{Example}
@end{Discussion}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[The image written out for a record having no
  components (including any interface type) is @exam{"(NULL@ RECORD)"}. The
  image written out for a componentless protected type is
  @exam{"(PROTECTED@ NULL@ RECORD)"}. In the case of a protected type T, a call to the default
  implementation of T'Put_Image begins only one protected (read-only) action.]}

@begin{ImplNote}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[The expected, but not required, implementation
    model for generating the image of a protected record involves the compiler
    producing a "helper" protected function which T'Put_Image would call. The
    result type of this function might be a null record; it is only a function
    because it does not need a write-lock, not because it returns a meaningful
    result.]}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Type=[Leading],Text=[The assertion in the following example
    should succeed:]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@key[type] T1 (D1, D2 : Positive) @key[is record] ... @key[end record]; -- @examcom{untagged}
@key[type] T2 (D : Positive) @key[is new] T1 (D1 => D, D2 => D);
X : T2 (D => 123) := ... ;
@key[pragma] Assert (X'Image /= T1(X)'Image);]}
@end{Example}
@end{ImplNote}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Text=[For an undiscriminated task type, the default
  implementation of T'Put_Image generates an image of the form @exam{"(TASK
  <task_id_image>)"} where <task_id_image> is the result obtained by calling
  Task_Identification.Image with the id of the given task and then passing that
  String to Characters.Conversions.To_Wide_Wide_String.]}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[For a discriminated task type, the
default implementation of T'Put_Image also includes discriminant values, as in:]}
@begin{Example}
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=["(TASK <task_id_image> with D1 =>  123, D2 =>  456)"]}
@end{Example}

@begin{Ramification}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[If T is an unchecked union type, then the
      default implementation of T'Put_Image will raise Program_Error.]}
@end{Ramification}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Type=[Leading],Keepnext=[T],Text=[For @PrefixType{every
  subtype S of a type T}, the following attributes are defined:]}

@begin{Description}

@ChgAttribute{Version=[5],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Wide_Wide_Image>, ARef=[AI12-0020-1], ARef=[AI12-0340-1],
  InitialVersion=[2], Text=[@Chg{Version=[5],New=[S'Wide_Wide_Image
     denotes a function with the following specification:],Old=[]}
@begin(Descexample)
@ChgRef{Version=[5],Kind=[Added]}
@ChgAdded{Version=[5],Text=[@b(function) S'Wide_Wide_Image(@RI(Arg) : S'Base)
  @b(return) Wide_Wide_String]}
@end(Descexample)
@Comment{We have to use "Added" on these two so that the annex paragraph
numbers come out correctly. Only the initial text gets overridden.}
     @ChgRef{Version=[5],Kind=[Added]}
     @ChgAdded{Version=[5],NoPrefix=[T],Text=[S'Wide_Wide_Image calls
      S'Put_Image passing @i<Arg>
      (which will typically store a sequence of character values
      in a text buffer) and then returns the result of retrieving the
      contents of that buffer with Wide_Wide_Get.]}]}@Comment{End of Annex text here.}
     @ChgRef{Version=[5],Kind=[AddedNormal]}
     @ChgAdded{Version=[5],Text=[The lower bound of the
      result is one.]}

@ChgAttribute{Version=[5],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Wide_Image>, ARef=[AI12-0020-1], ARef=[AI12-0340-1],
  InitialVersion=[0], Text=[@Chg{Version=[5],New=[S'Wide_Image denotes a
     function with the following specification:],Old=[]}
@Comment{We use an InitialVersion of 0 here so this item uses the existing
paragraph numbers in the Annex. It will leave the prefix uninserted, but that
will save a vast amount of messing around.}
@begin(Descexample)
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@b(function) S'Wide_Image(@RI(Arg) : S'Base)
  @b(return) Wide_String]}
@end(Descexample)

     @ChgRef{Version=[5],Kind=[AddedNormal]}
     @ChgAdded{Version=[5],NoPrefix=[T],Text=[S'Wide_Image calls S'Put_Image
     passing @i<Arg> (which will typically store a sequence of character values
     in a text buffer) and then returns the result of retrieving the
     contents of that buffer with Wide_Get.]}]}@Comment{End of Annex text here.}
     @ChgAdded{Version=[5],Text=[The lower bound of the result is one.]}

@ChgAttribute{Version=[5],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<T>, Prefix=<S>, AttrName=<Image>, ARef=[AI12-0020-1], ARef=[AI12-0340-1],
  InitialVersion=[0], Text=[@Chg{Version=[5],New=[S'Image denotes a function
     with the following specification:],Old=[]}
@Comment{We use an InitialVersion of 0 here so this item uses the existing
paragraph numbers in the Annex. It will leave the prefix uninserted, but that
will save a vast amount of messing around.}
@begin(Descexample)
@ChgRef{Version=[5],Kind=[AddedNormal]}
@ChgAdded{Version=[5],Text=[@b(function) S'Image(@RI(Arg) : S'Base)
  @b(return) String]}
@end(Descexample)

     @ChgRef{Version=[5],Kind=[AddedNormal]}
     @ChgAdded{Version=[5],NoPrefix=[T],Text=[S'Image calls S'Put_Image
     passing @i<Arg> (which will typically store a sequence of character values
     in a text buffer) and then returns the result of retrieving the
     contents of that buffer with Get.]}]}@Comment{End of Annex text here.}
     @ChgAdded{Version=[5],Text=[The lower bound of the result is one.]}

@EndPrefixType{}
@end(description)


@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0124-1],ARef=[AI12-0020-1],ARef=[AI12-0315-1]}
@ChgAdded{Version=[5],Type=[Leading],Keepnext=[T],Text=[For @PrefixType{a
  @nt{prefix} X of a type T other than @i{universal_real} or
  @i{universal_fixed}}, the following attributes are defined:]}

@begin(description)
@ChgAttribute{Version=[5],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<F>, Prefix=<X>, AttrName=<Wide_Wide_Image>,
  InitialVersion=[5], ARef=[AI12-0124-1], ARef=[AI12-0020-1],
  Text=[@Chg{Version=[5],New=[X'Wide_Wide_Image denotes the result of
  calling function S'Wide_Wide_Image with @i<Arg> being X, where S is the
  nominal subtype of X.],Old=[]}]}

@ChgAttribute{Version=[5],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<F>, Prefix=<X>, AttrName=<Wide_Image>,
  InitialVersion=[5], ARef=[AI12-0124-1], ARef=[AI12-0020-1],
  Text=[@Chg{Version=[5],New=[X'Wide_Image denotes the result of
  calling function S'Wide_Image with @i<Arg> being X, where S is the
  nominal subtype of X.],Old=[]}]}

@ChgAttribute{Version=[5],Kind=[AddedNormal],ChginAnnex=[T],
  Leading=<F>, Prefix=<X>, AttrName=<Image>,
  InitialVersion=[5], ARef=[AI12-0124-1], ARef=[AI12-0020-1],
  Text=[@Chg{Version=[5],New=[X'Image denotes the result of
  calling function S'Image with @i<Arg> being X, where S is the
  nominal subtype of X.],Old=[]}]}
@EndPrefixType{}
@end(description)

@end{StaticSem}

@begin{ImplPerm}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=[An implementation may transform the
image generated by the default implementation of S'Put_Image for a composite
subtype S in the following ways:]}

@begin{Itemize}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[If S is a composite subtype, the leading character
  of the image of a component value or index value is a space, and the
  immediately preceding character is an open parenthesis or bracket, then the
  space may be omitted. The same transformation is also permitted if the leading
  character of the component image is a space (in which case one of the two
  spaces may be omitted).]}
@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text={This means that it is permitted to generate
      @exam{"[1 => 123, 2 => 456]"} instead of @exam{"[ 1 =>  123,  2 =>  456]"}.}}
@end{Discussion}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[If S is an array subtype, the low bound of the
    array in each dimension equals the low bound of the corresponding index
    subtype, and the array value is not a null array value, then positional
    array aggregate syntax may be used.]}

@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text={This means that it is permitted to
       generate @exam{"[TRUE, TRUE, FALSE]"} instead of
       @exam{"[ 1 => TRUE,  2 => TRUE,  3 => FALSE]"} if the low bound of the
       index subtype is one.}}
@end{Discussion}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[If S is an array subtype and the given value can
    be displayed using @nt{named_array_aggregate} syntax where some
    @nt{discrete_choice_list} identifies more than one index value by
    identifying a sequence of one or more ranges and values separated by
    vertical bars, then this image may be generated instead; this may involve
    the reordering of component values.]}

@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text={This means that it is permitted to
       generate @exam{"[ 1 ..  2 |  5 => TRUE,  3 ..  4 => FALSE]"} instead of
       @exam{"[ 1 => TRUE,  2 => TRUE,  3 => FALSE,  4 => FALSE,  5 => TRUE]"}.}}
@end{Discussion}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[Similarly, if S is a record subtype (or a
    discriminated type) and the given value can be displayed using named
    component association syntax where the length of some component_choice_list
    is greater than one, then this image may be generated instead; this may
    involve the reordering of component values.]}

@begin{Discussion}
    @ChgRef{Version=[5],Kind=[AddedNormal]}
    @ChgAdded{Version=[5],Text=[This means that it is permitted to generate
       @exam{"(F1 | F2 => TRUE)"} instead of
       @exam{"(F1 => TRUE, F2 => TRUE)"}.]}
@end{Discussion}

  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1],ARef=[AI12-0340-1]}
  @ChgAdded{Version=[5],Text=[Additional spaces (Wide_Wide_Characters with
    position 32), and calls to the New_Line operation of a text buffer, may be
    inserted to improve readability of the generated image.]}

@end{Itemize}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0304-1]}
@ChgAdded{Version=[5],Text=[For each language-defined nonscalar type T,
T'Put_Image may be specified.]}

@begin{Discussion}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[This permission applies, in particular, to
    nonscalar types declared in language-defined generic packages,
    and to any language-defined private type, even if an implementation
    chooses to complete it as a scalar type.]}
@end{Discussion}

@begin{Ramification}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[For any language-defined scalar type T, T'Put_Image should
    not be specified; the Image attribute needs to return the language-defined
    image for such types. This is important for compatibility: the Image
    attribute has been available for scalar types for many Ada revisions,
    and programs can (and do!) depend on its behavior.]}
@end{Ramification}

@end{ImplPerm}

@begin{ImplReq}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0304-1]}
@ChgAdded{Version=[5],Type=[Leading],Text=<For each language-defined container
  type T (that is, each of the Vector, List, Map, Set, Tree, and Holder types
  defined in the various children of Ada.Containers), T'Put_Image shall be
  specified so that T'Image produces a result consistent with array aggregate
  syntax (using '[' and ']' as delimiters) as follows:>}

@begin{Itemize}
  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[Vector images shall be consistent with the
    default image of an array type with the same index and component types.]}

    @begin{Discussion}
      @ChgRef{Version=[5],Kind=[AddedNormal]}
      @ChgAdded{Version=[5],Text=[In particular, this means that the format is
	that of a named array aggregate. We have no recommendation on how to
	handle empty elements; if the implementation can identify them, it may
	wish to display them specially, but otherwise, they're just
	uninitialized elements.]}
    @end{Discussion}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[Map images shall be consistent with named array
    aggregate syntax, using key value images in place of discrete choice names.
    For example, @exam{[Key1 => Value1, Key2 => Value2]}.]}

    @begin{Discussion}
      @ChgRef{Version=[5],Kind=[AddedNormal]}
      @ChgAdded{Version=[5],Text=[There is no recommendation about the order
        in which key/element pairs occur within a map image. In the case of
        multiple key values whose corresponding element values have the same
        image, there is no recommendation about factoring (that is,
        generating @exam{Key1 | Key2 => Some_Value} instead of @exam{Key1 =>
        Some_Value, Key2 => Some_Value}).]}
    @end{Discussion}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[Set, List, and Holder images shall be consistent
    with positional array aggregate syntax. List elements shall occur in order
    within an image of a list. The image of an empty holder shall be @exam{[]}.]}

    @begin{Discussion}
      @ChgRef{Version=[5],Kind=[AddedNormal]}
      @ChgAdded{Version=[5],Text=[There is no recommendation about the order
        in which set elements occur within the image of a set.]}
    @end{Discussion}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[Tree images (and images of subtrees of trees)
    shall be consistent with positional array aggregate syntax.
    For example, @exam{[[1, 2], [111, 222, 333]]}.]}

@end{Itemize}

@ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0304-1]}
@ChgAdded{Version=[5],Text=[For each language-defined nonscalar type T that has
a primitive language-defined Image function whose profile is type conformant
with that of T'Image (for example, Ada.Numerics.Float_Random.State has such an
Image function), T'Put_Image shall be specified so that T'Image yields the same
result as that Image function.]}

@end{ImplReq}

@begin{ImplAdvice}

  @ChgRef{Version=[5],Kind=[AddedNormal]}
  @ChgAdded{Version=[5],Text=[For each language-defined private type T,
    T'Image should generate an image that would be meaningful based only on the
    relevant public interfaces, as opposed to requiring knowledge of the
    implementation of the private type.]}

@ChgImplAdvice{Version=[5],Kind=[AddedNormal],InitialVersion=[5],Text=[@ChgAdded{Version=[5],
Text=[each language-defined private type T, T'Image should generate an image
that would be meaningful based only on the relevant public interfaces.]}]}


@end{ImplAdvice}


@begin{Extend2012}
  @ChgRef{Version=[5],Kind=[AddedNormal],ARef=[AI12-0020-1],ARef=[AI12-0315-1],ARef=[AI12-0340-1]}
  @ChgAdded{Version=[5],Text=[@Defn{extensions to Ada 2012}Attribute
    Put_Image is new. Attributes Image, Wide_Image, and Wide_Wide_Image
    now can be used with any type, and are defined in terms of Put_Image so
    that they can be redefined.]}
@end{Extend2012}

