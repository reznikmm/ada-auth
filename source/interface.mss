@comment{ $Source: e:\\cvsroot/ARM/Source/interface.mss,v $ }
@comment{ $Revision: 1.12 $ $Date: 2000/05/19 04:12:07 $ $Author: Randy $ }
@Part(interface, Root="ada.mss")

@Comment{$Date: 2000/05/19 04:12:07 $}
@LabeledNormativeAnnex{Interface to Other Languages}

@begin{Intro}
@Defn{interface to other languages}
@Defn2{Term=[language], Sec=(interface to non-Ada)}
@Defn{mixed-language programs}
This Annex
 describes features for writing mixed-language programs.
General interface support is presented first;
then specific support for C, COBOL, and Fortran is defined,
in terms of language interface packages for each of
these languages.
@begin{Ramification}
This Annex is not a ``Specialized Needs'' annex.
Every implementation must support all non-optional features defined here
(mainly the package Interfaces).
@end{Ramification}
@end{Intro}

@begin{MetaRules}
Ada should have strong support for mixed-language programming.
@end{MetaRules}

@begin{Extend83}
Much of the functionality in this Annex is new to Ada 9X.
@end{Extend83}

@begin{DiffWord83}
This Annex contains what used to be RM83-13.8.
@end{DiffWord83}

@LabeledClause{Interfacing Pragmas}

@begin{Intro}
A @nt{pragma} Import is used to import an entity defined in a foreign
language into an Ada program,
thus allowing
 a foreign-language subprogram to be called from Ada,
or a foreign-language variable to be accessed from Ada.
In contrast,
a @nt{pragma} Export  is used to export an Ada entity
to a foreign language, thus allowing
 an Ada subprogram to be called from a foreign language,
or an Ada object
 to be accessed from a foreign language.
The @nt[pragma]s
Import and Export
are intended primarily for objects and
subprograms, although implementations are allowed to support other
entities.

A @nt{pragma} Convention is used to specify that an Ada entity should use
the conventions of another language.
It is intended primarily for types and ``callback'' subprograms.
For example,
``@key{pragma} Convention(Fortran, Matrix);''
implies that Matrix should be represented according to the
conventions of the supported  Fortran implementation, namely
column-major order.

A @nt{pragma} Linker_Options is used to specify the system linker
parameters needed when a given compilation unit is included in a
partition.

@end{Intro}

@begin{Syntax}
@begin{SyntaxText}
@RootDefn{interfacing pragma}
@PDefn2{Term=[interfacing pragma], Sec=(Import)}
@PDefn2{Term=[pragma, interfacing], Sec=(Import)}
@PDefn2{Term=[interfacing pragma], Sec=(Export)}
@PDefn2{Term=[pragma, interfacing], Sec=(Export)}
@PDefn2{Term=[interfacing pragma], Sec=(Convention)}
@PDefn2{Term=[pragma, interfacing], Sec=(Convention)}
@PDefn2{Term=[pragma, interfacing], Sec=(Linker_Options)}
An @i{interfacing pragma} is a representation
@nt[pragma] that is
one of the @nt{pragma}s Import, Export,
or Convention.
Their forms, together with that of the related
@nt[pragma] Linker_Options, are as follows:
@end{SyntaxText}
@PragmaSyn`@key{pragma} @prag(Import)(
    [Convention =>] @SynI{convention_}@Syn2{identifier}, [Entity =>] @Syn2{local_name}
 [, [External_Name =>] @SynI{string_}@Syn2{expression}] [, [Link_Name =>] @SynI{string_}@Syn2{expression}]);'

@PragmaSyn`@key{pragma} @prag(Export)(
    [Convention =>] @SynI{convention_}@Syn2{identifier}, [Entity =>] @Syn2{local_name}
 [, [External_Name =>] @SynI{string_}@Syn2{expression}] [, [Link_Name =>] @SynI{string_}@Syn2{expression}]);'

@PragmaSyn`@key{pragma} @prag(Convention)([Convention =>] @SynI{convention_}@Syn2{identifier},[Entity =>] @Syn2{local_name});'

@PragmaSyn`@key{pragma} @prag(Linker_Options)(@SynI{string_}@Syn2{expression});'

@begin{SyntaxText}
A @nt[pragma] Linker_Options is allowed only at the place of a
@nt[declarative_item].
@end{SyntaxText}
@end{Syntax}

@begin{Resolution}
@PDefn2{Term=[expected type],
  Sec=(link name)}
The expected type for a @SynI{string_}@Syn2{expression}
in an interfacing pragma or in pragma Linker_Options is String.
@begin{Ramification}
There is no language-defined support for
external or link names of type
Wide_String, or of other string types.
Implementations may, of course, have additional pragmas for that
purpose.
Note that allowing both String and Wide_String in the same @nt{pragma} would
cause ambiguities.
@end{Ramification}
@end{Resolution}

@begin{Legality}
@Defn{convention}
The @SynI{convention_}@nt{identifier} of an interfacing pragma
shall be the name of a @i{convention}.
The convention names are implementation defined,
except for certain language-defined ones,
such as Ada and Intrinsic,
as explained in @RefSec{Conformance Rules}.
@Redundant[Additional convention names generally represent
the calling conventions of foreign languages,
language implementations, or specific run-time models.]
@Defn{calling convention}
The convention of a callable entity is its @i{calling convention}.
@ImplDef{Implementation-defined convention names.}
@begin{Discussion}
We considered representing the convention names using an enumeration
type declared in System.
Then, @SynI{convention_}@nt{identifier} would be changed to
@SynI{convention_}@nt{name},
and we would make its expected type be the enumeration type.
We didn't do this because it seems to introduce extra complexity,
and because the list of available languages is better represented as
the list of children of package Interfaces @em a more open-ended sort
of list.
@end{Discussion}

@Defn2{Term=[compatible],Sec=[a type, with a convention]}
If @i[L] is a @i[convention_]@nt[identifier] for a language, then a
type T is said to be @i{compatible with convention L},
(alternatively,
is said to be an @i[L-compatible type]) if any of
the following conditions are met:
@begin[itemize]
T is declared in a language interface package
corresponding to @i[L] and is defined to be
@i[L]-compatible
(see @refsecnum(Interfacing with C),
@refsecnum(The Package Interfaces.C.Strings),
@refsecnum(The Generic Package Interfaces.C.Pointers),
@refsecnum(Interfacing with COBOL),
@refsecnum(Interfacing with Fortran)),

@Defn2{Term=[eligible],Sec=[a type, for a convention]}
Convention @i[L] has been specified for T in
a @nt[pragma] Convention, and T is @i{eligible for
convention @i[L]}; that is:
@begin[itemize]
T is an array type with either an
unconstrained or statically-constrained first subtype, and
its component type is @i[L]-compatible,

T is a record type that has no discriminants and that only has
components with statically-constrained subtypes, and
each component type is @i[L]-compatible,

T is an access-to-object type, and its designated type is @i[L]-compatible,

T is an access-to-subprogram type,
and its designated profile's parameter and result types are all @i[L]-compatible.
@end[itemize]

T is derived from an @i[L]-compatible type,

The implementation permits T as an @i[L]-compatible type.
@begin{discussion}

For example, an implementation might permit Integer as a C-compatible
type, though the C type to which it corresponds might be different
in different environments.@end{discussion}
@end[itemize]

If @nt[pragma] Convention   applies to a type,
then the type shall either be
compatible with or eligible for
the convention specified in the pragma.
@begin[ramification]
If a type is derived from an @i[L]-compatible type, the derived type
is by default @i[L]-compatible, but it is also permitted to specify
pragma Convention for the derived type.

It is permitted to specify pragma Convention for an incomplete type,
but in the complete declaration each component must be
@i[L]-compatible.

If each component of a record type is @i[L]-compatible, then
the record type itself is only @i[L]-compatible if it has a pragma
Convention.
@end[Ramification]

A @nt{pragma} Import shall be the completion of a declaration.
@Defn{notwithstanding}
Notwithstanding any rule to the contrary,
a @nt{pragma} Import may serve as the completion of any
kind of (explicit) declaration if supported by an implementation for
that kind of declaration.
If a completion is a @nt{pragma} Import,
then it shall appear in the same
@nt{declarative_part}, @nt{package_specification}, @nt{task_definition}
or @nt{protected_definition}
as the declaration.
For a library unit, it shall appear in the same @nt{compilation},
before any subsequent @nt{compilation_unit}s other than @nt{pragma}s.
If the @nt{local_name} denotes more than one entity,
then the @nt{pragma} Import is the completion of all of them.
@begin(Discussion)
  For declarations of deferred constants and subprograms, we mention
  pragma Import
  explicitly as a possible completion.  For other declarations that
  require completions, we ignore the possibility of pragma Import.
  Nevertheless, if an implementation chooses to allow a @nt{pragma} Import
  to complete the declaration of a task, protected type, incomplete
  type, private type, etc., it may do so, and the normal completion
  is then not allowed for that declaration.
@end(Discussion)

@Defn{imported entity} @Defn{exported entity}
An entity specified as the Entity argument to a @nt[pragma] Import
(or @nt[pragma] Export)
 is said to be @i{imported} (respectively, @i{exported}).

The declaration of an imported object shall not include an explicit
initialization expression.
@Redundant[Default initializations are not performed.]
@begin{TheProof}
This follows from the ``Notwithstanding ...''
wording in the Dynamics Semantics paragraphs below.
@end{TheProof}

The type of an imported or exported object shall be compatible with the
convention specified in the corresponding @nt[pragma].
@begin[Ramification]
This implies, for example, that importing an Integer object might be illegal,
whereas importing an object of type Interfaces.C.int would be permitted.
@end[Ramification]

For an imported or exported subprogram, the result and parameter types
shall each be compatible with the convention specified in the
corresponding pragma.

The external name and link name @i{string}_@nt[expression]s
 of a @nt{pragma} Import or Export, and the @i[string]_@nt[expression]
of a @nt[pragma] Linker_Options,  shall be
static.
@end{Legality}

@begin{StaticSem}
@PDefn2{Term=[representation pragma], Sec=(Import)}
@PDefn2{Term=[pragma, representation], Sec=(Import)}
@PDefn2{Term=[representation pragma], Sec=(Export)}
@PDefn2{Term=[pragma, representation], Sec=(Export)}
@PDefn2{Term=[representation pragma], Sec=(Convention)}
@PDefn2{Term=[pragma, representation], Sec=(Convention)}
@PDefn2{Term=[aspect of representation], Sec=(convention, calling convention)}
@Defn2{Term=[convention], Sec=(aspect of representation)}
Import, Export, and Convention @nt{pragma}s are representation pragmas
that specify the @i{convention} aspect of representation.
@PDefn2{Term=[aspect of representation], Sec=(imported)}
@Defn2{Term=[imported], Sec=(aspect of representation)}
@PDefn2{Term=[aspect of representation], Sec=(exported)}
@Defn2{Term=[exported], Sec=(aspect of representation)}
In addition, Import and Export @nt{pragma}s specify the
@i{imported} and @i{exported} aspects of representation,
respectively.

@PDefn2{Term=[program unit pragma], Sec=(Import)}
@PDefn2{Term=[pragma, program unit], Sec=(Import)}
@PDefn2{Term=[program unit pragma], Sec=(Export)}
@PDefn2{Term=[pragma, program unit], Sec=(Export)}
@PDefn2{Term=[program unit pragma], Sec=(Convention)}
@PDefn2{Term=[pragma, program unit], Sec=(Convention)}
An interfacing pragma is a program unit pragma
when applied to a program unit
(see @RefSecNum{Pragmas and Program Units}).

An interfacing pragma defines the convention
of the entity denoted by the @nt{local_name}.
The convention represents the calling convention or representation
convention of the entity.
For an access-to-subprogram type, it represents the calling
convention of designated subprograms.
In addition:
@begin{Itemize}
A @nt{pragma} Import specifies that the entity is defined externally (that is,
outside the Ada program).

A @nt{pragma} Export specifies that the entity is used externally.

A @nt{pragma} Import or Export optionally specifies an
entity's external name, link name, or both.
@end{Itemize}

@Defn{external name}
An @i{external name} is a string value for the
name used by a foreign language program either for an
entity that an Ada program imports, or for referring to
an entity that an Ada program exports.

@Defn{link name}
A @i[link name] is a string value for the name of an exported
or imported entity, based on the conventions of the
foreign language's compiler in interfacing with the
system's linker tool.

The meaning of link names is implementation defined.
@ImplDef{The meaning of link names.}
@begin{Ramification}
For example,
an implementation might always prepend "_",
and then pass it to the system linker.
@end{Ramification}
If neither a link name nor
the Address attribute of an imported or exported entity is specified,
then a link name is chosen in an implementation-defined manner,
based on the external name if one is specified.
@ImplDef{The manner of choosing link names when neither the link name
nor the address of an imported or exported entity is specified.}
@begin{Ramification}
Normally, this will be the entity's defining name,
or some simple transformation thereof.
@end{Ramification}

Pragma Linker_Options has the
effect of passing its string argument as a parameter to
the system linker (if one exists), if the immediately
enclosing compilation unit is included in the partition
being linked.  The interpretation of the string argument, and the
 way in which the string arguments from
multiple Linker_Options pragmas are combined, is implementation
defined.
@ImplDef(The effect of pragma Linker_Options.)
@end{StaticSem}

@begin{RunTime}
@PDefn2{Term=[elaboration], Sec=(declaration named by a @nt{pragma} Import)}
@Defn{notwithstanding}
Notwithstanding what this International Standard says elsewhere,
the elaboration of a declaration denoted by the
@nt{local_name} of
a @nt{pragma} Import does not create the entity.
Such an elaboration has no other effect than to allow the defining name
to denote the external entity.
@begin{Ramification}
This implies that default initializations are skipped.
(Explicit initializations are illegal.)
For example, an imported access object is @i{not}
initialized to @key[null].

Note that the @nt{local_name}
 in a @nt{pragma} Import might denote
more than one declaration; in that case, the entity of all of those
declarations will be the external entity.
@end{Ramification}
@begin{Discussion}
This ``notwithstanding'' wording is better than saying
``unless named by a @nt{pragma} Import'' on every definition of
elaboration.
It says we recognize the contradiction, and this rule takes
precedence.
@end{Discussion}
@end{RunTime}




@begin{ImplAdvice}
    If an implementation supports pragma Export
     to a given language, then it should also
    allow the main subprogram to be written in that language.
    It should support some mechanism for invoking the elaboration of the
    Ada library units included in the system, and for invoking the
    finalization of the environment task.
    On typical systems, the recommended mechanism is to provide two
    subprograms whose link names are "adainit" and "adafinal".
    Adainit should contain the elaboration code for library units.
    Adafinal should contain the finalization code.
    These subprograms should have no effect the second and subsequent
    time they are called.
@begin{ramification}
For example, if the main subprogram is written in C,
it can call adainit before the first call to an Ada subprogram,
and adafinal after the last.@end{ramification}

Automatic elaboration of preelaborated packages should be provided
when @nt[pragma] Export is supported.

For each supported convention @i[L] other than Intrinsic,
an implementation should support Import and Export @nt{pragma}s for
objects of @i[L]-compatible types and for
subprograms, and @nt(pragma) Convention for @i[L]-eligible
types and for subprograms,
presuming the other language has corresponding features.
@nt(Pragma) Convention need not be supported for scalar types.
@begin{reason}
Pragma Convention is not necessary for scalar types,
since the language interface packages declare scalar types corresponding
to those provided by the respective foreign languages.
@end[reason]
@begin{ImplNote}
If an implementation supports interfacing to C++,
it should do so via the convention identifier C_Plus_Plus
(in additional to any C++-implementation-specific
ones).
@end{ImplNote}
@begin{Reason}
The reason for giving the advice about C++ is to encourage
uniformity among implementations, given that the name of the language is
not syntactically legal as an @nt{identifier}.
We place this advice in the AARM, rather than the RM9X proper,
because (as of this writing) C++ is not an international standard,
and we don't want to refer to a such a language from an international
standard.
@end{Reason}
@end{ImplAdvice}

@begin{Notes}
Implementations may place restrictions on interfacing
pragmas;
for example, requiring each exported entity to be declared
at the library level.
@begin{TheProof}
Arbitrary restrictions are allowed by
@RefSecNum{Representation Items}.
@end{TheProof}
@begin{Ramification}
Such a restriction might be to disallow them altogether.
Alternatively, the implementation might allow them only for certain
kinds of entities,
or only for certain conventions.
@end{Ramification}

A @nt{pragma} Import specifies the conventions for accessing external
entities.  It is possible that the actual entity is written in assembly
language, but reflects the conventions of a particular
language.  For example, @key{pragma} Import(Ada, ...) can be used to
interface to an assembly language routine that obeys the
Ada compiler's calling conventions.

To obtain ``call-back'' to an Ada subprogram from a foreign language
environment, @key(pragma) Convention should be specified both for the
access-to-subprogram type and the specific subprogram(s) to which 'Access
is applied.

It is illegal to specify more than one of
Import, Export, or Convention for a given entity.

The @nt{local_name}
 in an interfacing pragma can denote more than one
entity in the case of overloading.
Such a @nt{pragma} applies to all of the denoted entities.

See also @RefSec{Machine Code Insertions}.
@begin{Ramification}
The Intrinsic convention (see @refsecnum(Conformance Rules))
implies that the entity is somehow ``built
in'' to the implementation.
Thus, it generally does not make sense for users to specify Intrinsic
in a @nt{pragma} Import.
The intention is that only implementations will specify
Intrinsic in a @nt{pragma} Import.
The language also defines certain subprograms to be Intrinsic.
@end{Ramification}
@begin{Discussion}
There are many imaginable interfacing pragmas that don't make any
sense.
For example, setting the Convention of a protected procedure to Ada
is probably wrong.
Rather than enumerating all such cases, however,
we leave it up to implementations to decide what
is sensible.
@end{Discussion}

If both External_Name and Link_Name are specified for an Import
or Export pragma,
then the External_Name is ignored.

An interfacing pragma might result in an effect that violates
Ada semantics.
@end{Notes}

@begin{Examples}
@i{Example of interfacing pragmas:}
@begin{Example}
@key[package] Fortran_Library @key[is]
  @key[function] Sqrt (X : Float) @key[return] Float;
  @key[function] Exp  (X : Float) @key[return] Float;
@key[private]
  @key[pragma] Import(Fortran, Sqrt);
  @key[pragma] Import(Fortran, Exp);
@key[end] Fortran_Library;
@end{Example}
@end{Examples}

@begin{Extend83}
Interfacing pragmas are new to Ada 9X.
Pragma Import replaces Ada 83's pragma Interface.
Existing implementations can continue to support pragma Interface for
upward compatibility.
@end{Extend83}

@LabeledClause{The Package Interfaces}
@begin{Intro}
Package Interfaces is  the parent of several library
packages that declare
 types and other entities useful for
interfacing to  foreign languages.
It also contains some implementation-defined
types that are  useful across more than one language
(in particular for interfacing to assembly
language).
@ImplDef{The contents of the visible part of package Interfaces
and its language-defined descendants.}
@end{Intro}

@begin{StaticSem}
The  library package Interfaces has the following skeletal
declaration:
@begin{Example}
@RootLibUnit{Interfaces}
@key[package] Interfaces @key[is]
   @key[pragma] Pure(Interfaces);

   @key[type] Integer_@i{n} @key[is] @key[range] -2**(@i{n}-1) .. 2**(@i{n}-1) - 1;  --@i{2's complement}

   @key[type] Unsigned_@i{n} @key[is] @key[mod] 2**@i{n};

   @key[function] Shift_Left  (Value : Unsigned_@i{n}; Amount : Natural) @key[return] Unsigned_@i{n};
   @key[function] Shift_Right (Value : Unsigned_@i{n}; Amount : Natural) @key[return] Unsigned_@i{n};
   @key[function] Shift_Right_Arithmetic (Value : Unsigned_@i{n}; Amount : Natural)
      @key[return] Unsigned_@i{n};
   @key[function] Rotate_Left  (Value : Unsigned_@i{n}; Amount : Natural) @key[return] Unsigned_@i{n};
   @key[function] Rotate_Right (Value : Unsigned_@i{n}; Amount : Natural) @key[return] Unsigned_@i{n};
   ...
@key[end] Interfaces;
@end{Example}
@end{StaticSem}

@begin{ImplReq}
An implementation shall provide the following declarations in the
visible part of package Interfaces:
@begin{Itemize}
Signed and modular integer types of @i{n} bits,
if supported by the target architecture,
for each @i{n} that is at least the size
of a storage element and that is a factor of the word size.
The names of these types are of the form Integer_@i{n} for the
signed types, and Unsigned_@i{n} for the modular types;
@begin{Ramification}
For example, for a typical 32-bit machine the corresponding
types might be Integer_8, Unsigned_8,
Integer_16, Unsigned_16,
Integer_32,  and Unsigned_32.

The wording above implies, for example, that Integer_16'Size =
Unsigned_16'Size = 16.
Unchecked conversions between same-Sized types will work as
expected.
@end{Ramification}

@Defn{shift}@Defn{rotate}
For each such modular type in Interfaces,
shifting and rotating subprograms as specified in the declaration of
Interfaces above.
These subprograms are Intrinsic.
They operate on a bit-by-bit basis,
using the binary representation of the
 value of the operands
to yield a binary representation for the result.
The Amount parameter gives the number of bits by which to shift
or rotate.
For shifting, zero bits are shifted in, except in the case of
Shift_Right_Arithmetic, where one bits are shifted in if Value is
at least half the modulus.
@begin{Reason}
We considered making shifting and rotating be primitive operations of
all modular types.
However, it is a design principle of Ada that all predefined operations
should be operators (not functions named by identifiers).
(Note that an early version of Ada had "@key[abs]" as an identifier,
but it was changed to a reserved word operator before standardization of
Ada 83.)
This is important because the implicit declarations would hide
non-overloadable declarations with the same name,
whereas operators are always overloadable.
Therefore, we would have had to make shift and rotate
into reserved words,
which would have been upward incompatible,
or else invent new operator symbols,
which seemed like too much mechanism.
@end{Reason}

Floating point types corresponding to each floating point format
fully supported by the hardware.
@begin{ImplNote}

The names for these floating point types are not specified.
@Defn{IEEE floating point arithmetic}
However, if IEEE arithmetic is supported, then the names
should be IEEE_Float_32 and IEEE_Float_64 for single and double
precision, respectively.

@end{ImplNote}
@end{Itemize}
@end{ImplReq}

@begin{ImplPerm}
An implementation may provide implementation-defined library units
that are children of Interfaces,
and may add declarations to the visible part of Interfaces
in addition to the ones defined above.
@ImplDef{Implementation-defined children of package Interfaces.
The contents of the visible part of package Interfaces.}
@end{ImplPerm}

@begin{ImplAdvice}
For each implementation-defined convention identifier,
there should be a child package of
package Interfaces with the corresponding name.
This package should contain any declarations that would be useful for
interfacing to the language (implementation) represented by the
convention.
Any declarations useful for interfacing to any language on the
given  hardware architecture
 should be provided directly in Interfaces.
@begin{Ramification}
For example, package Interfaces.XYZ_Pascal might contain
declarations of types that match the data types provided by the
XYZ implementation of Pascal,
so that it will be more convenient to pass parameters to a subprogram
whose convention is XYZ_Pascal.
@end{Ramification}

An implementation supporting an interface to C, COBOL, or Fortran
should provide the corresponding
package or packages described in the following
clauses.
@begin{ImplNote}
The intention is that an implementation might support several
implementations of the foreign language: Interfaces.This_Fortran and
Interfaces.That_Fortran might both exist.
The ``default'' implementation, overridable by the user,
should be declared as a renaming:
@begin{Example}
@key[package] Interfaces.Fortran @key[renames] Interfaces.This_Fortran;
@end{Example}
@end{ImplNote}
@end{ImplAdvice}

@LabeledClause{Interfacing with C}
@begin{Intro}
@Defn{interface to C}
@Defn{C interface}
The facilities relevant to interfacing with
the C language
 are the package Interfaces.C
and its children;
and support for the Import, Export, and
Convention pragmas with @i{convention}_@nt{identifier} C.

The package Interfaces.C contains the basic types, constants and
subprograms that allow an Ada program to pass scalars and strings to C
functions.
@end{Intro}

@begin{StaticSem}
The library package Interfaces.C has the following declaration:
@begin{Example}
@ChildUnit{Parent=[Interfaces],Child=[C],Expanded=[Interfaces.C]}
@key(package) Interfaces.C @key(is)
   @key(pragma) Pure(C);

   @i{-- Declarations based on C's <limits.h>}

   CHAR_BIT  : @key(constant) := @i{implementation-defined};  @i{-- typically 8}
   SCHAR_MIN : @key(constant) := @i{implementation-defined};  @i{-- typically -128}
   SCHAR_MAX : @key(constant) := @i{implementation-defined};  @i{-- typically 127}
   UCHAR_MAX : @key(constant) := @i{implementation-defined};  @i{-- typically 255}

   @i{-- Signed and Unsigned Integers}
@LangDefType{Package=[Interfaces.C],Type=[int]}
   @key(type) int   @key(is) @key(range) @i{implementation-defined};
@LangDefType{Package=[Interfaces.C],Type=[short]}
   @key(type) short @key(is) @key(range) @i{implementation-defined};
@LangDefType{Package=[Interfaces.C],Type=[long]}
   @key(type) long  @key(is) @key(range) @i{implementation-defined};

@LangDefType{Package=[Interfaces.C],Type=[signed_char]}
   @key(type) signed_char @key(is) @key(range) SCHAR_MIN .. SCHAR_MAX;
   @key(for) signed_char'Size @key(use) CHAR_BIT;

@LangDefType{Package=[Interfaces.C],Type=[unsigned]}
   @key(type) unsigned       @key(is) @key(mod) @i{implementation-defined};
@LangDefType{Package=[Interfaces.C],Type=[unsigned_short]}
   @key(type) unsigned_short @key(is) @key(mod) @i{implementation-defined};
@LangDefType{Package=[Interfaces.C],Type=[unsigned_long]}
   @key(type) unsigned_long  @key(is) @key(mod) @i{implementation-defined};

@LangDefType{Package=[Interfaces.C],Type=[unsigned_char]}
   @key(type) unsigned_char @key(is) @key(mod) (UCHAR_MAX+1);
   @key(for) unsigned_char'Size @key(use) CHAR_BIT;

@LangDefType{Package=[Interfaces.C],Type=[plain_char]}
   @key(subtype) plain_char @key(is) @i{implementation-defined};

@LangDefType{Package=[Interfaces.C],Type=[ptrdiff_t]}
   @key(type) ptrdiff_t @key(is) @key(range) @i{implementation-defined};

@LangDefType{Package=[Interfaces.C],Type=[size_t]}
   @key(type) size_t @key(is) @key(mod) @i{implementation-defined};

   @i{-- Floating Point}

@LangDefType{Package=[Interfaces.C],Type=[C_float]}
   @key(type) C_float     @key(is) @key(digits) @i{implementation-defined};

@LangDefType{Package=[Interfaces.C],Type=[double]}
   @key(type) double      @key(is) @key(digits) @i{implementation-defined};

@LangDefType{Package=[Interfaces.C],Type=[long_double]}
   @key(type) long_double @key(is) @key(digits) @i{implementation-defined};


   @i{-- Characters and Strings }

@LangDefType{Package=[Interfaces.C],Type=[char]}
   @key(type) char @key(is) @i{<implementation-defined character type>};

   nul : @key(constant) char := char'First;

   @key[function] To_C   (Item : @key[in] Character) @key[return] char;

   @key[function] To_Ada (Item : @key[in] char) @key[return] Character;

@LangDefType{Package=[Interfaces.C],Type=[char_array]}
   @key(type) char_array @key(is) @key(array) (size_t @key(range) <>) @key(of) @key[aliased] char;
   @key[pragma] Pack(char_array);
   @key(for) char_array'Component_Size @key(use) CHAR_BIT;

   @key(function) Is_Nul_Terminated (Item : @key(in) char_array) @key(return) Boolean;

   @key(function) To_C   (Item       : @key(in) String;
                    Append_Nul : @key(in) Boolean := True)
      @key(return) char_array;

   @key(function) To_Ada (Item     : @key(in) char_array;
                    Trim_Nul : @key(in) Boolean := True)
      @key(return) String;

   @key(procedure) To_C (Item       : @key(in) String;
                   Target     : @key(out) char_array;
                   Count      : @key(out) size_t;
                   Append_Nul : @key(in) Boolean := True);

   @key(procedure) To_Ada (Item     : @key(in) char_array;
                     Target   : @key(out) String;
                     Count    : @key(out) Natural;
                     Trim_Nul : @key(in) Boolean := True);

   @i{-- Wide Character and Wide String}

@LangDefType{Package=[Interfaces.C],Type=[wchar_t]}
   @key(type) wchar_t @key(is) @i{implementation-defined};

   wide_nul : @key(constant) wchar_t := wchar_t'First;

   @key(function) To_C   (Item : @key(in) Wide_Character) @key(return) wchar_t;
   @key(function) To_Ada (Item : @key(in) wchar_t       ) @key(return) Wide_Character;

@LangDefType{Package=[Interfaces.C],Type=[wchar_array]}
   @key(type) wchar_array @key(is) @key(array) (size_t @key(range) <>) @key(of) @key(aliased) wchar_t;

   @key(pragma) Pack(wchar_array);

   @key(function) Is_Nul_Terminated (Item : @key(in) wchar_array) @key(return) Boolean;

   @key(function) To_C   (Item       : @key(in) Wide_String;
                    Append_Nul : @key(in) Boolean := True)
      @key(return) wchar_array;

   @key(function) To_Ada (Item     : @key(in) wchar_array;
                    Trim_Nul : @key(in) Boolean := True)
      @key(return) Wide_String;

   @key(procedure) To_C (Item       : @key(in)  Wide_String;
                   Target     : @key(out) wchar_array;
                   Count      : @key(out) size_t;
                   Append_Nul : @key(in)  Boolean := True);

   @key(procedure) To_Ada (Item     : @key(in)  wchar_array;
                     Target   : @key(out) Wide_String;
                     Count    : @key(out) Natural;
                     Trim_Nul : @key(in)  Boolean := True);

   Terminator_Error : @key(exception);

@key(end) Interfaces.C;
@end{Example}

Each of the types declared in Interfaces.C is C-compatible.

The types int, short, long, unsigned,
ptrdiff_t, size_t, double,
char, and wchar_t correspond respectively
to the C types having the same names.
The types signed_char, unsigned_short, unsigned_long, unsigned_char,
C_float, and long_double correspond respectively
to the C types signed char,
unsigned short, unsigned long, unsigned char, float, and long double.

The type of the subtype plain_char is either signed_char or
unsigned_char, depending on the C implementation.
@begin{DescribeCode}
@begin{Example}
@key(function) To_C   (Item : @key(in) Character) @key(return) char;
@key(function) To_Ada (Item : @key(in) char     ) @key(return) Character;
@end{Example}

The functions To_C and To_Ada map between the Ada type Character
and the C type char.

@begin{Example}
@key(function) Is_Nul_Terminated (Item : @key(in) char_array) @key(return) Boolean;
@end{Example}


The result of Is_Nul_Terminated is True if Item contains nul, and is
False otherwise.


@begin{Example}
@key(function) To_C   (Item : @key(in) String;     Append_Nul : @key(in) Boolean := True)
   @key(return) char_array;

@key(function) To_Ada (Item : @key(in) char_array; Trim_Nul   : @key(in) Boolean := True)
   @key(return) String;
@end{Example}

The result of To_C is a char_array
 value of length Item'Length (if
Append_Nul is False) or Item'Length+1 (if Append_Nul is True).
The lower bound is 0.
For each component Item(I), the corresponding component in the result
is To_C applied to Item(I).
The value nul is appended if Append_Nul is True.

The result of To_Ada is a String whose length is Item'Length (if Trim_Nul is
False) or the length of the slice of Item preceding the first
nul (if
Trim_Nul is True).  The lower bound of the result is 1.
If Trim_Nul is False, then for each component Item(I)
the
corresponding component in the result
is To_Ada applied to Item(I).
If Trim_Nul is True, then for each component Item(I)
before
the first nul the corresponding component in the result
is To_Ada applied to Item(I).
The function propagates Terminator_Error
 if Trim_Nul is True and
Item does not contain nul.

@begin{Example}
@key(procedure) To_C (Item       : @key(in) String;
                Target     : @key(out) char_array;
                Count      : @key(out) size_t;
                Append_Nul : @key(in) Boolean := True);

@key(procedure) To_Ada (Item     : @key(in) char_array;
                  Target   : @key(out) String;
                  Count    : @key(out) Natural;
                  Trim_Nul : @key(in) Boolean := True);
@end{Example}

For procedure To_C, each element of Item is converted (via the
To_C function)
to a char, which is assigned to the corresponding element
of Target.  If Append_Nul is True, nul
is then assigned to the next
element of Target.  In either case, Count is set to the
number of Target elements assigned.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If Target is not long enough, Constraint_Error is propagated.

For procedure To_Ada, each element of Item (if Trim_Nul is False) or
each element of Item preceding the first nul
 (if Trim_Nul is True) is
converted (via the To_Ada function) to a Character, which is
assigned to the corresponding element of Target.
Count
is set to the number of Target elements assigned.
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If Target is not long enough, Constraint_Error is propagated.
If Trim_Nul is True and Item does not contain nul,
then Terminator_Error is propagated.

@begin{Example}
@key(function) Is_Nul_Terminated (Item : @key(in) wchar_array) @key(return) Boolean;
@end{Example}


The result of Is_Nul_Terminated is True if Item contains wide_nul, and is
False otherwise.


@begin{Example}
@key(function) To_C   (Item : @key(in) Wide_Character) @key(return) wchar_t;
@key(function) To_Ada (Item : @key(in) wchar_t       ) @key(return) Wide_Character;
@end{Example}

To_C and To_Ada provide the mappings between the Ada and C wide
character types.

@begin{Example}
@key(function) To_C   (Item       : @key(in) Wide_String;
                 Append_Nul : @key(in) Boolean := True)
   @key(return) wchar_array;

@key(function) To_Ada (Item     : @key(in) wchar_array;
                 Trim_Nul : @key(in) Boolean := True)
   @key(return) Wide_String;

@key(procedure) To_C (Item       : @key(in) Wide_String;
                Target     : @key(out) wchar_array;
                Count      : @key(out) size_t;
                Append_Nul : @key(in) Boolean := True);

@key(procedure) To_Ada (Item     : @key(in) wchar_array;
                  Target   : @key(out) Wide_String;
                  Count    : @key(out) Natural;
                  Trim_Nul : @key(in) Boolean := True);
@end{Example}

The To_C and To_Ada subprograms that convert between Wide_String and
wchar_array have analogous effects to the To_C and To_Ada
subprograms that convert between String
and char_array, except that
wide_nul is used instead of nul.
@begin{Discussion}
The Interfaces.C package provides an implementation-defined character type,
char,
designed to model the C run-time character set, and mappings
between the types char and Character.

One application of the C interface package is
to compose a C string and pass it to a C function.
One way to do this is for the programmer to declare an
object that will hold the C array, and then pass this array to the C
function.  This is realized via the type char_array:
@begin{Example}
@key(type) char_array @key(is) @key(array) (size_t @key(range) <>) of Char;
@end{Example}

The programmer can declare an Ada String, convert it to a char_array, and
pass the char_array as actual parameter to the C function that is expecting
a char *.

An alternative approach is for the programmer to obtain a C char pointer
from an Ada String (or from a char_array) by invoking an allocation
function.  The package Interfaces.C.Strings (see below) supplies
the needed facilities, including a
private type chars_ptr that corresponds to C's
char *, and two allocation functions.  To avoid storage
leakage, a Free procedure releases the storage that was
allocated by one
of these allocate functions.

It is typical for a C function that deals with strings to adopt the
convention that the string is delimited by a nul char.  The C interface
packages support this convention.  A constant nul of type Char is declared,
and the function Value(Chars_Ptr) in Interfaces.C.Strings
returns a char_array up to and including
the first nul in the array that the chars_ptr points to.  The Allocate_Chars
function allocates an array that is nul terminated.

Some C functions that deal with strings take an explicit length as a
parameter, thus allowing strings to be passed that contain nul as
a data element.  Other C functions take an explicit length that is
an upper bound: the prefix of the string up to the char before nul,
or the prefix of the given length, is used by the
function, whichever is shorter.
The C Interface packages support calling such functions.
@end{Discussion}
@end{DescribeCode}
@end{StaticSem}

@begin{ImplReq}
An implementation shall support pragma Convention
with a C @i{convention}_@nt{identifier} for a
C-eligible type  (see
@refsecnum(Interfacing Pragmas))
@end{ImplReq}


@begin{ImplPerm}
An implementation may provide additional declarations in the C
interface packages.
@end{ImplPerm}


@begin{ImplAdvice}
An implementation should support the following interface
correspondences between Ada  and C.
@begin[itemize]
An Ada procedure corresponds to
a void-returning C function.
@begin{discussion}
The programmer can also choose an Ada procedure when
the C function returns an int that is to be discarded.@end{discussion}

An Ada function corresponds to a non-void C function.

An Ada @key[in] scalar parameter is passed as
 a scalar argument to a C function.

An Ada @key[in] parameter of an access-to-object type with designated
type T is passed as a t* argument to a
C function, where t is the C type corresponding to the
Ada type T.

An Ada @key[access] T parameter,
or an Ada @key[out] or @key[in out]  parameter of an elementary type T,
 is passed as
 a t* argument
to a C function, where t is the C type corresponding to the
Ada type T.  In the case of an elementary @key[out] or @key[in out]
parameter, a pointer to a temporary copy is used to preserve
by-copy semantics.


An Ada parameter of a record type T, of any mode, is passed as
 a t* argument to a C function,
where t is the C struct corresponding to the
Ada type T.

An Ada parameter of an array type with component type
T, of any mode, is passed as a t* argument to a
C function, where t is the C type corresponding to the
Ada type T.

An Ada parameter of an access-to-subprogram type
is passed as a pointer to a
C function whose prototype corresponds to the designated subprogram's
specification.
@end[itemize]
@end{ImplAdvice}


@begin{Notes}
Values of type char_array are not implicitly terminated with nul.
If a char_array is to be passed as a parameter to an imported
C function requiring nul termination, it is the programmer's
responsibility to obtain this effect.

To obtain the effect of C's sizeof(item_type),
where Item_Type is the corresponding Ada type,
 evaluate the expression: size_t(Item_Type'Size/CHAR_BIT).

There is no explicit support for C's union types.
Unchecked conversions  can be used to obtain
the effect of C unions.

A C function that takes a variable number of arguments
can correspond to several Ada subprograms, taking various
specific numbers and types of parameters.
@end{Notes}

@begin{Examples}
@i{Example of using the Interfaces.C package:}
@begin{Example}
@i{--Calling the C Library Function strcpy}
@key(with) Interfaces.C;
@key(procedure) Test @key(is)
   @key(package) C @key(renames) Interfaces.C;
   @key(use) @key(type) C.char_array;
   @i{-- Call <string.h>strcpy:}
   @i{-- C definition of strcpy:  char *strcpy(char *s1, const char *s2);}
   @i{--    This function copies the string pointed to by s2 (including the terminating null character)}
   @i{--     into the array pointed to by s1.  If copying takes place between objects that overlap, }
   @i{--     the behavior is undefined.  The strcpy function returns the value of s1.}

   @i{-- Note: since the C function's return value is of no interest, the Ada interface is a procedure}
   @key(procedure) Strcpy (Target : @key(out) C.char_array;
                     Source : @key(in)  C.char_array);

   @key(pragma) Import(C, Strcpy, "strcpy");

   Chars1 :  C.char_array(1..20);
   Chars2 :  C.char_array(1..20);

@key(begin)
   Chars2(1..6) := "qwert" & C.nul;

   Strcpy(Chars1, Chars2);

@i{-- Now Chars1(1..6) = "qwert" & C.Nul}

@key(end) Test;
@end{Example}
@end{Examples}

@LabeledSubClause{The Package Interfaces.C.Strings}

@begin{Intro}
The package Interfaces.C.Strings declares types and subprograms
allowing an Ada program to allocate, reference, update, and free C-style
strings.
In particular, the private type chars_ptr
 corresponds to a common
use of ``char *'' in C programs, and an object of this type can be
passed to a subprogram to which @nt(pragma) Import(C,...) has been applied,
and for which ``char *''
 is the type of the argument of the C function.
@end{Intro}

@begin{StaticSem}
The library package Interfaces.C.Strings has the following
declaration:
@begin{example}
@ChildUnit{Parent=[Interfaces.C],Child=[Strings],Expanded=[Interfaces.C.Strings]}
@key(package) Interfaces.C.Strings @key(is)
   @key[pragma] Preelaborate(Strings);

@LangDefType{Package=[Interfaces.C],Type=[char_array_access]}
   @key(type) char_array_access @key(is) @key(access) @key(all) char_array;

@LangDefType{Package=[Interfaces.C],Type=[chars_ptr]}
   @key(type) chars_ptr @key(is) @key(private);

@LangDefType{Package=[Interfaces.C],Type=[chars_ptr_array]}
   @key(type) chars_ptr_array @key(is) @key(array) (size_t @key(range) <>) @key(of) chars_ptr;

   Null_Ptr : @key(constant) chars_ptr;

   @key(function) To_Chars_Ptr (Item      : @key(in) char_array_access;
                          Nul_Check : @key(in) Boolean := False)
      @key(return) chars_ptr;

   @key(function) New_Char_Array (Chars   : @key(in) char_array) @key(return) chars_ptr;

   @key(function) New_String (Str : @key(in) String) @key(return) chars_ptr;

   @key(procedure) Free (Item : @key(in) @key(out) chars_ptr);

   Dereference_Error : @key(exception);


   @key(function) Value (Item : @key(in) chars_ptr) @key(return) char_array;

   @key(function) Value (Item : @key(in) chars_ptr; Length : @key(in) size_t)
      @key(return) char_array;

   @key(function) Value (Item : @key(in) chars_ptr) @key(return) String;

   @key(function) Value (Item : @key(in) chars_ptr; Length : @key(in) size_t)
      @key(return) String;

   @key(function) Strlen (Item : @key(in) chars_ptr) @key(return) size_t;

   @key(procedure) Update (Item   : @key(in) chars_ptr;
                     Offset : @key(in) size_t;
                     Chars  : @key(in) char_array;
                     Check  : @key(in) Boolean := True);

   @key(procedure) Update (Item   : @key(in) chars_ptr;
                     Offset : @key(in) size_t;
                     Str    : @key(in) String;
                     Check  : @key(in) Boolean := True);

   Update_Error : @key(exception);


@key(private)
   ... -- @i{not specified by the language}
@key(end) Interfaces.C.Strings;
@end{Example}
@begin{discussion}
The string manipulation types and subprograms appear in a
child of Interfaces.C versus being there directly, since it is
useful to have Interfaces.C specified as @nt(pragma) Pure.

Differently named functions New_String and New_Char_Array
are declared, since if there were a single overloaded function
a call with a string literal as actual parameter would be
ambiguous.
@end{discussion}

The type chars_ptr is C-compatible and
corresponds to the use of C's ``char *'' for
a pointer to the first char in a char array terminated by nul.
When an object of type chars_ptr is declared, its value is
by default set to Null_Ptr, unless the object is imported
(see @RefSecNum(Interfacing Pragmas)).
@begin{discussion}

The type char_array_access is not necessarily C-compatible, since
an object of this type may carry ``dope'' information.
The programmer should convert from char_array_access to chars_ptr
for objects imported from, exported to, or passed to C.@end{discussion}
@begin{DescribeCode}
@begin{Example}
@key(function) To_Chars_Ptr (Item      : @key(in) char_array_access;
                       Nul_Check : @key(in) Boolean := False)
   @key(return) chars_ptr;
@end{Example}

If Item is @key(null), then To_Chars_Ptr returns Null_Ptr.
Otherwise, if Nul_Check is True and Item.@key(all) does not contain nul,
then the function propagates Terminator_Error;
if Nul_Check is True and Item.@key(all) does contain nul,
To_Chars_Ptr performs a pointer conversion with no allocation of memory.
@begin{Example}
@key(function) New_Char_Array (Chars   : @key(in) char_array) @key(return) chars_ptr;
@end{Example}

This function returns a pointer to an allocated
 object initialized to
  Chars(Chars'First .. Index) & nul, where
@begin{itemize}
Index = Chars'Last if Chars does not contain nul, or

Index is the smallest size_t value I such that Chars(I+1) = nul.
@end{itemize}

Storage_Error is propagated if the allocation fails.


@begin{Example}
@key(function) New_String (Str : @key(in) String) @key(return) chars_ptr;
@end{Example}

This function is equivalent to New_Char_Array(To_C(Str)).

@begin{Example}
@key(procedure) Free (Item : @key(in) @key(out) chars_ptr);
@end{Example}

If Item is Null_Ptr, then Free has no effect.
Otherwise, Free releases the storage occupied by Value(Item),
 and resets Item to Null_Ptr.
@begin{Example}
@key(function) Value (Item : @key(in) chars_ptr) @key(return) char_array;
@end{Example}

If Item = Null_Ptr then Value propagates Dereference_Error.
Otherwise Value returns the prefix of the array
 of chars pointed to by Item, up to and including the
 first nul.
The lower bound of the result is 0.
If Item does not point to a nul-terminated string, then
execution of Value is erroneous.

@begin{Example}
@key(function) Value (Item : @key(in) chars_ptr; Length : @key(in) size_t)
   @key(return) char_array;
@end{Example}

If Item = Null_Ptr then Value(Item) propagates Dereference_Error.
Otherwise Value returns the shorter of two arrays:
 the first Length chars pointed to by Item, and Value(Item).
The lower bound of the result is 0.
@begin{Ramification}
Value(New_Char_Array(Chars)) = Chars if Chars does not contain
nul; else Value(New_Char_Array(Chars)) is the prefix of Chars
up to and including the first nul.
@end{Ramification}

@begin{Example}
@key(function) Value (Item : @key(in) chars_ptr) @key(return) String;
@end{Example}

Equivalent to To_Ada(Value(Item), Trim_Nul=>True).

@begin{Example}
@key(function) Value (Item : @key(in) chars_ptr; Length : @key(in) size_t)
   @key(return) String;
@end{Example}

Equivalent to To_Ada(Value(Item, Length), Trim_Nul=>True).

@begin{Example}
@key(function) Strlen (Item : @key(in) chars_ptr) @key(return) size_t;
@end{Example}

Returns @i[Val]'Length@en@;1 where @i[Val] = Value(Item);
propagates Dereference_Error if Item = Null_Ptr.
@begin{ramification}
Strlen returns the number of chars in the array pointed to by Item, up to
and including the char immediately before the first nul.

Strlen has the same possibility for erroneous execution
as Value, in cases where the string has not been nul-terminated.

Strlen has the effect of C's strlen function.
@end{Ramification}

@begin{Example}
@key(procedure) Update (Item   : @key(in) chars_ptr;
                  Offset : @key(in) size_t;
                  Chars  : @key(in) char_array;
                  Check  : Boolean := True);
@end{Example}

This procedure updates the value pointed to by Item, starting at
position  Offset,
using Chars as the data to be copied into the array.
Overwriting the nul terminator,
and  skipping with the Offset past the nul terminator,
are both prevented if Check is True, as follows:
@begin[itemize]
Let N = Strlen(Item).
If Check is True, then:
@begin{itemize}
 If Offset+Chars'Length>N, propagate Update_Error.

 Otherwise, overwrite the data in the array pointed to by Item,
  starting at the char at position Offset, with the data in Chars.
@end{itemize}

If Check is False, then
 processing is as above, but with no check that Offset+Chars'Length>N.
@begin{Ramification}
If Chars contains nul, Update's effect may be
to ``shorten'' the pointed-to char array.@end{ramification}
@end[itemize]
@begin{Example}
@key(procedure) Update (Item   : @key(in) chars_ptr;
                  Offset : @key(in) size_t;
                  Str    : @key(in) String;
                  Check  : @key(in) Boolean := True);
@end{Example}

Equivalent to Update(Item, Offset, To_C(Str), Check).
@end{DescribeCode}
@end{StaticSem}

@begin{erron}
Execution of any of the following is erroneous if the Item
parameter is not null_ptr and Item
does not point to a nul-terminated array of chars.
@begin[itemize]
a Value function not taking a Length parameter,

the Free procedure,

the Strlen function.
@end[itemize]

Execution of Free(X) is also erroneous if the chars_ptr X was not returned
by New_Char_Array or New_String.

Reading or updating a freed char_array is erroneous.

Execution of Update is erroneous if Check is False and a call with
Check equal to True would have propagated Update_Error.
@end{erron}

@begin{Notes}
New_Char_Array and New_String might be
implemented either through
the allocation function from the C environment (``malloc'') or through
Ada dynamic memory allocation (``new''). The key points are
@begin{itemize}
the returned value (a chars_ptr) is
represented as a C ``char *'' so
that it may be passed to C functions;

the allocated object should be freed by the programmer via a call of
Free, not by a called C function.
@end{itemize}
@end{Notes}

@LabeledSubClause{The Generic Package Interfaces.C.Pointers}
@begin{Intro}
The generic package Interfaces.C.Pointers allows the Ada programmer to
perform C-style operations on pointers.  It includes an access type
Pointer, Value functions that dereference a Pointer and deliver the
designated array, several pointer arithmetic operations, and ``copy''
procedures that copy the contents of a source pointer into the array
designated by a destination pointer.  As in C, it treats an object Ptr of
type Pointer as a pointer to the first element of an array, so that for
example, adding 1 to Ptr yields a pointer to the
second element of the array.

The generic allows two styles of usage: one in which the array is
terminated by a special terminator element; and another in which the
programmer needs to keep track of the length.
@end{Intro}

@begin{StaticSem}
The generic library package Interfaces.C.Pointers has the
following declaration:
@begin{Example}
@key(generic)
   @key(type) Index @key(is) (<>);
   @key(type) Element @key(is) @key(private);
   @key(type) Element_Array @key(is) @key(array) (Index @key(range) <>) @key(of) @key(aliased) Element;
   Default_Terminator : Element;
@ChildUnit{Parent=[Interfaces.C],Child=[Pointers],Expanded=[Interfaces.C.Pointers]}
@key(package) Interfaces.C.Pointers @key(is)
   @key[pragma] Preelaborate(Pointers);

@LangDefType{Package=[Interfaces.C.Pointers],Type=[Pointer]}
   @key(type) Pointer @key(is) @key(access) @key(all) Element;

   @key(function) Value(Ref        : @key(in) Pointer;
                  Terminator : @key(in) Element := Default_Terminator)
      @key(return) Element_Array;

   @key(function) Value(Ref    : @key(in) Pointer;
                  Length : @key(in) ptrdiff_t)
      @key(return) Element_Array;


   Pointer_Error : @key(exception);

   @i{-- C-style Pointer arithmetic}

   @key(function) "+" (Left : @key(in) Pointer;   Right : @key(in) ptrdiff_t) @key(return) Pointer;
   @key(function) "+" (Left : @key(in) ptrdiff_t; Right : @key(in) Pointer)   @key(return) Pointer;
   @key(function) "-" (Left : @key(in) Pointer;   Right : @key(in) ptrdiff_t) @key(return) Pointer;
   @key(function) "-" (Left : @key(in) Pointer;   Right : @key(in) Pointer)   @key(return) ptrdiff_t;

   @key(procedure) Increment (Ref : @key(in) @key(out) Pointer);
   @key(procedure) Decrement (Ref : @key(in) @key(out) Pointer);

   @key(pragma) Convention (Intrinsic, "+");
   @key(pragma) Convention (Intrinsic, "-");
   @key(pragma) Convention (Intrinsic, Increment);
   @key(pragma) Convention (Intrinsic, Decrement);

   @key(function) Virtual_Length (Ref        : @key(in) Pointer;
                            Terminator : @key(in) Element := Default_Terminator)
      @key(return) ptrdiff_t;

   @key(procedure) Copy_Terminated_Array (Source     : @key(in) Pointer;
                                    Target     : @key(in) Pointer;
                                    Limit      : @key(in) ptrdiff_t := ptrdiff_t'Last;
                                    Terminator : @key(in) Element :=  Default_Terminator);

   @key(procedure) Copy_Array (Source  : @key(in) Pointer;
                         Target  : @key(in) Pointer;
                         Length  : @key(in) ptrdiff_t);

@key(end) Interfaces.C.Pointers;
@end{Example}

The type Pointer is C-compatible and
corresponds to one use of C's ``Element *''.
An object of type Pointer is interpreted as a pointer to the
initial Element in an Element_Array.
Two styles are supported:
@begin{Itemize}
Explicit termination of an array value with
     Default_Terminator (a special terminator value);

Programmer-managed length, with
     Default_Terminator  treated simply as a data element.
@end{Itemize}
@begin{DescribeCode}
@begin{Example}
@key(function) Value(Ref        : @key(in) Pointer;
               Terminator : @key(in) Element := Default_Terminator)
   @key(return) Element_Array;
@end{Example}

This function returns an Element_Array whose value is the array pointed
  to by Ref, up to and including the first Terminator; the lower bound
  of the array is Index'First.
  Interfaces.C.Strings.Dereference_Error is
  propagated if Ref is @key(null).

@begin{Example}
@key(function) Value(Ref    : @key(in) Pointer;
               Length : @key(in) ptrdiff_t)
   @key(return) Element_Array;
@end{Example}

This function returns an Element_Array comprising the first Length
elements pointed to by Ref.  The exception
Interfaces.C.Strings.Dereference_Error is
propagated if Ref is @key(null).
@end{DescribeCode}

The "+" and "@en@;" functions perform arithmetic on Pointer values, based on
the Size of the array elements.
In each of these functions, Pointer_Error
is propagated if a Pointer parameter is @key(null).
@begin{DescribeCode}
@begin{Example}
@key(procedure) Increment (Ref : @key(in) @key(out) Pointer);
@end{Example}

Equivalent to Ref := Ref+1.

@begin{Example}
@key(procedure) Decrement (Ref : @key(in) @key(out) Pointer);
@end{Example}

Equivalent to Ref := Ref@en@;1.

@begin{Example}
@key(function) Virtual_Length (Ref        : @key(in) Pointer;
                         Terminator : @key(in) Element := Default_Terminator)
   @key(return) ptrdiff_t;
@end{Example}

Returns the number of Elements, up to the one just before the first
Terminator, in Value(Ref, Terminator).

@begin{Example}
@key(procedure) Copy_Terminated_Array (Source     : @key(in) Pointer;
                                 Target     : @key(in) Pointer;
                                 Limit      : @key(in) ptrdiff_t := ptrdiff_t'Last;
                                 Terminator : @key(in) Element := Default_Terminator);
@end{Example}

This procedure copies Value(Source, Terminator) into the array
 pointed to by Target;
it stops either after Terminator has been copied, or the
 number of elements copied is Limit, whichever occurs first.
Dereference_Error is propagated if either Source or Target is @key(null).
@begin{ramification}
It is the programmer's responsibility to ensure that
elements are not copied beyond the logical length of the target array.@end{ramification}
@begin{ImplNote}
  The implementation has to take care to check the Limit
  first.
@end{ImplNote}

@begin{Example}
@key(procedure) Copy_Array (Source  : @key(in) Pointer;
                      Target  : @key(in) Pointer;
                      Length  : @key(in) ptrdiff_t);
@end{Example}

This procedure copies the first Length elements from the array pointed
 to by Source, into the array pointed to by Target.
 Dereference_Error is propagated if either
 Source or Target is @key(null).
@end{DescribeCode}
@end{StaticSem}

@begin{erron}
It is erroneous to dereference a Pointer that does not designate
an aliased Element.
@begin{Discussion}
Such a Pointer could arise via "+", "-", Increment, or
Decrement.@end{discussion}

Execution of Value(Ref, Terminator) is erroneous if
Ref does not designate an aliased Element in an Element_Array
terminated by Terminator.

Execution of Value(Ref, Length) is erroneous if
Ref does not designate an aliased Element in an Element_Array
containing at least Length Elements between the designated Element and
the end of the array, inclusive.

Execution of Virtual_Length(Ref, Terminator) is erroneous if
Ref does not designate an aliased Element in an Element_Array
terminated by Terminator.

Execution of Copy_Terminated_Array(Source, Target, Limit, Terminator)
is erroneous in either of the following situations:
@begin[itemize]
Execution of both Value(Source,Terminator) and
Value(Source,Limit) are erroneous, or

Copying writes past the end of the array containing the Element
designated by Target.
@end[Itemize]

Execution of Copy_Array(Source, Target, Length) is erroneous if either
Value(Source, Length) is erroneous, or copying writes past the end of
the array containing the Element designated by Target.
@end{erron}

@begin{Notes}
To compose a Pointer from an Element_Array, use 'Access on
 the first element.  For example (assuming appropriate instantiations):
@begin{example}
Some_Array   : Element_Array(0..5) ;
Some_Pointer : Pointer := Some_Array(0)'Access;
@end{example}
@end{Notes}

@begin{Examples}
@i{Example of Interfaces.C.Pointers:}
@begin{Example}
@key(with) Interfaces.C.Pointers;
@key(with) Interfaces.C.Strings;
@key(procedure) Test_Pointers @key(is)
   @key(package) C @key(renames) Interfaces.C;
   @key(package) Char_Ptrs @key(is)
      @key(new) C.Pointers (Index              => C.size_t,
                      Element            => C.char,
                      Element_Array      => C.char_array,
                      Default_Terminator => C.nul);

   @key(use) @key(type) Char_Ptrs.Pointer;
   @key(subtype) Char_Star @key(is) Char_Ptrs.Pointer;

   @key(procedure) Strcpy (Target_Ptr, Source_Ptr : Char_Star) @key(is)
      Target_Temp_Ptr : Char_Star := Target_Ptr;
      Source_Temp_Ptr : Char_Star := Source_Ptr;
      Element : C.char;
   @key(begin)
      @key(if) Target_Temp_Ptr = @key(null) @key(or) Source_Temp_Ptr = @key(null) @key(then)
         @key(raise) C.Strings.Dereference_Error;
      @key(end if);

      @key(loop)
         Element             := Source_Temp_Ptr.@key(all);
         Target_Temp_Ptr.@key(all) := Element;
         @key(exit) @key(when) Element = C.nul;
         Char_Ptrs.Increment(Target_Temp_Ptr);
         Char_Ptrs.Increment(Source_Temp_Ptr);
      @key(end) @key(loop);
   @key(end) Strcpy;
@key(begin)
   ...
@key(end) Test_Pointers;
@end{Example}
@end{Examples}

@LabeledClause{Interfacing with COBOL}
@begin{Intro}
@Defn{interface to COBOL}
@Defn{COBOL interface}
The facilities relevant to interfacing with
the COBOL language
 are the package
Interfaces.COBOL
and support for the Import, Export and
Convention pragmas with @i{convention}_@nt{identifier} COBOL.

The COBOL interface package supplies several sets of facilities:
@begin{itemize}
A set of types corresponding to the native
COBOL types of the supported COBOL implementation
(so-called ``internal COBOL representations''),
allowing Ada data to be passed as parameters to COBOL programs

A set of types and constants reflecting external data representations
such as might be found in files or databases, allowing COBOL-generated
data to be read by an Ada program, and Ada-generated data to be read
by COBOL programs

A generic package for converting between an Ada decimal type value and
either an internal or external COBOL representation
@end{itemize}
@end{Intro}

@begin{StaticSem}
The library package Interfaces.COBOL has the following declaration:
@begin{Example}
@ChildUnit{Parent=[Interfaces],Child=[COBOL],Expanded=[Interfaces.COBOL]}
@key(package) Interfaces.COBOL @key(is)
   @key[pragma] Preelaborate(COBOL);

@i{-- Types and operations for internal data representations}

@LangDefType{Package=[Interfaces.COBOL],Type=[Floating]}
   @key(type) Floating      @key(is) @key(digits) @i{implementation-defined};
@LangDefType{Package=[Interfaces.COBOL],Type=[Long_Floating]}
   @key(type) Long_Floating @key(is) @key(digits) @i{implementation-defined};

@LangDefType{Package=[Interfaces.COBOL],Type=[Binary]}
   @key(type) Binary      @key(is) @key(range) @i{implementation-defined};
@LangDefType{Package=[Interfaces.COBOL],Type=[Long_Binary]}
   @key(type) Long_Binary @key(is) @key(range) @i{implementation-defined};

   Max_Digits_Binary      : @key(constant) := @i{implementation-defined};
   Max_Digits_Long_Binary : @key(constant) := @i{implementation-defined};

@LangDefType{Package=[Interfaces.COBOL],Type=[Decimal_Element]}
   @key(type) Decimal_Element  @key(is) @key(mod) @i{implementation-defined};
@LangDefType{Package=[Interfaces.COBOL],Type=[Packed_Decimal]}
   @key(type) Packed_Decimal @key(is) @key(array) (Positive @key(range) <>) @key(of) Decimal_Element;
   @key(pragma) Pack(Packed_Decimal);


@LangDefType{Package=[Interfaces.COBOL],Type=[COBOL_Character]}
   @key(type) COBOL_Character @key(is) @i{implementation-defined character type};

   Ada_To_COBOL : @key(array) (Character) @key(of) COBOL_Character := @i{implementation-defined};

   COBOL_To_Ada : @key(array) (COBOL_Character) @key(of) Character := @i{implementation-defined};

@LangDefType{Package=[Interfaces.COBOL],Type=[Alphanumeric]}
   @key(type) Alphanumeric @key(is) @key(array) (Positive range <>) @key(of) COBOL_Character;
   @key(pragma) Pack(Alphanumeric);

   @key(function) To_COBOL (Item : @key(in) String) @key(return) Alphanumeric;
   @key(function) To_Ada   (Item : @key(in) Alphanumeric) @key(return) String;

   @key(procedure) To_COBOL (Item       : @key(in) String;
                       Target     : @key(out) Alphanumeric;
                       Last       : @key(out) Natural);

   @key(procedure) To_Ada (Item     : @key(in) Alphanumeric;
                     Target   : @key(out) String;
                     Last     : @key(out) Natural);

@LangDefType{Package=[Interfaces.COBOL],Type=[Numeric]}
   @key(type) Numeric @key(is) @key(array) (Positive @key[range] <>) @key(of) COBOL_Character;
   @key(pragma) Pack(Numeric);

@i{-- Formats for COBOL data representations}

@LangDefType{Package=[Interfaces.COBOL],Type=[Display_Format]}
   @key(type) Display_Format @key(is) @key(private);

   Unsigned             : @key(constant) Display_Format;
   Leading_Separate     : @key(constant) Display_Format;
   Trailing_Separate    : @key(constant) Display_Format;
   Leading_Nonseparate  : @key(constant) Display_Format;
   Trailing_Nonseparate : @key(constant) Display_Format;

@LangDefType{Package=[Interfaces.COBOL],Type=[Binary_Format]}
   @key(type) Binary_Format @key(is) @key(private);

   High_Order_First  : @key(constant) Binary_Format;
   Low_Order_First   : @key(constant) Binary_Format;
   Native_Binary     : @key(constant) Binary_Format;

@LangDefType{Package=[Interfaces.COBOL],Type=[Packed_Format]}
   @key(type) Packed_Format @key(is) @key(private);

   Packed_Unsigned   : @key(constant) Packed_Format;
   Packed_Signed     : @key(constant) Packed_Format;


@i{-- Types for external representation of COBOL binary data}

@LangDefType{Package=[Interfaces.COBOL],Type=[Byte]}
   @key(type) Byte @key(is) @key(mod) 2**COBOL_Character'Size;
@LangDefType{Package=[Interfaces.COBOL],Type=[Byte_Array]}
   @key(type) Byte_Array @key(is) @key(array) (Positive @key(range) <>) @key(of) Byte;
   @key(pragma) Pack (Byte_Array);

   Conversion_Error : @key(exception);

   @key(generic)
      @key(type) Num @key(is) @key(delta) <> @key(digits) <>;
   @key(package) Decimal_Conversions @key(is)

      @i{-- Display Formats: data values are represented as Numeric}

      @key(function) Valid (Item   : @key(in) Numeric;
                      Format : @key(in) Display_Format) @key(return) Boolean;

      @key(function) Length (Format : @key(in) Display_Format) @key(return) Natural;


      @key(function) To_Decimal (Item   : @key(in) Numeric;
                           Format : @key(in) Display_Format) @key(return) Num;

      @key(function) To_Display (Item   : @key(in) Num;
                           Format : @key(in) Display_Format) @key(return) Numeric;


      @i{-- Packed Formats: data values are represented as Packed_Decimal}

      @key(function) Valid (Item   : @key(in) Packed_Decimal;
                      Format : @key(in) Packed_Format) @key(return) Boolean;

      @key(function) Length (Format : @key(in) Packed_Format) @key(return) Natural;

      @key(function) To_Decimal (Item   : @key(in) Packed_Decimal;
                           Format : @key(in) Packed_Format) @key(return) Num;

      @key(function) To_Packed (Item   : @key(in) Num;
                          Format : @key(in) Packed_Format) @key(return) Packed_Decimal;


      @i{-- Binary Formats: external data values are represented as Byte_Array}

      @key(function) Valid (Item   : @key(in) Byte_Array;
                      Format : @key(in) Binary_Format) @key(return) Boolean;

      @key(function) Length (Format : @key(in) Binary_Format) @key(return) Natural;
      @key(function) To_Decimal (Item   : @key(in) Byte_Array;
                           Format : @key(in) Binary_Format) @key(return) Num;

      @key(function) To_Binary (Item   : @key(in) Num; 
                        Format : @key(in) Binary_Format) @key(return) Byte_Array;

      @i{-- Internal Binary formats: data values are of type Binary or Long_Binary}

      @key(function) To_Decimal (Item : @key(in) Binary)      @key(return) Num;
      @key(function) To_Decimal (Item : @key(in) Long_Binary) @key(return) Num;

      @key(function) To_Binary      (Item : @key(in) Num)  @key(return) Binary;
      @key(function) To_Long_Binary (Item : @key(in) Num)  @key(return) Long_Binary;

   @key(end) Decimal_Conversions;

@key(private)
   ... -- @i{not specified by the language}
@key(end) Interfaces.COBOL;
@end{Example}
@ImplDef[The types Floating, Long_Floating, Binary, Long_Binary,
Decimal_Element, and COBOL_Character; and the initializations
of the variables Ada_To_COBOL and COBOL_To_Ada, in Interfaces.COBOL]

Each of the types in Interfaces.COBOL is COBOL-compatible.

The types Floating and Long_Floating correspond to the
native types in COBOL for data items with computational usage
implemented by floating point.
The types Binary and Long_Binary correspond to the
native types in COBOL for data items with binary usage,
or with computational usage implemented by binary.

Max_Digits_Binary is the largest number of decimal digits in a
numeric value that is represented as Binary.
Max_Digits_Long_Binary is the largest number of decimal digits in a
numeric value that is represented as Long_Binary.

The type Packed_Decimal corresponds to COBOL's packed-decimal
usage.

The type COBOL_Character
 defines the run-time character set used in the
COBOL implementation.
Ada_To_COBOL and COBOL_To_Ada are the mappings between the Ada and
COBOL run-time character sets.
@begin{reason}
The character mappings are visible variables, since the
 user needs the ability to modify them at run time.
@end{reason}

Type Alphanumeric corresponds to COBOL's alphanumeric data category.

Each of the functions To_COBOL and To_Ada converts its parameter
based on the mappings Ada_To_COBOL and COBOL_To_Ada, respectively.
The length of the result for each is the length of the parameter,
and the lower bound of the result is 1.  Each component of the result
is obtained by applying the relevant mapping to the corresponding
component of the parameter.

Each of the procedures To_COBOL and To_Ada copies converted elements
from Item to Target, using the appropriate mapping (Ada_To_COBOL or
COBOL_To_Ada, respectively).  The index in Target of the last element
assigned is returned in Last (0 if Item is a null array).
@Defn2{Term=[Constraint_Error],Sec=(raised by failure of run-time check)}
If Item'Length exceeds Target'Length, Constraint_Error is propagated.

Type Numeric corresponds to COBOL's numeric data category with
display usage.

The types Display_Format, Binary_Format, and Packed_Format
are used in conversions between Ada decimal type values and
COBOL internal or external data representations.  The value of the
constant Native_Binary is either High_Order_First or Low_Order_First,
depending on the implementation.
@begin{DescribeCode}
@begin{Example}
@key(function) Valid (Item   : @key(in) Numeric;
                Format : @key(in) Display_Format) @key(return) Boolean;
@end{Example}

The function Valid
checks that the
Item parameter has a value consistent with
the value of Format.
If the value of Format is other than
Unsigned, Leading_Separate, and Trailing_Separate,
the effect is implementation defined.  If Format does have one
of these values, the following rules apply:
@begin{itemize}
Format=Unsigned: if Item comprises zero or more leading
space characters followed by one or more decimal digit
characters then Valid returns
 True, else it returns False.

Format=Leading_Separate: if Item  comprises
zero or more leading space characters,
followed by a single occurrence of
the plus or minus sign character, and then one or more
decimal digit characters, then Valid returns True, else it returns False.

Format=Trailing_Separate: if Item comprises zero or more leading space
characters, followed by one or more decimal digit characters and
finally a plus or minus sign character,
then Valid returns True, else it returns False.
@end{itemize}

@begin{Example}
@key(function) Length (Format : @key(in) Display_Format) @key(return) Natural;
@end{Example}

The Length function returns the minimal length of a Numeric value
sufficient to hold any value of type Num when represented as Format.

@begin{Example}
@key(function) To_Decimal (Item   : @key(in) Numeric;
                     Format : @key(in) Display_Format) @key(return) Num;
@end{Example}

Produces a value of type Num corresponding to Item as represented by
Format.
The number of digits after the assumed
radix point in Item is Num'Scale.
 Conversion_Error is propagated if the value
represented by Item is outside the range of Num.
@begin{discussion}
There is no issue of truncation versus rounding, since
the number of decimal places is established by Num'Scale.@end{discussion}

@begin{Example}
@key(function) To_Display (Item   : @key(in) Num;
                     Format : @key(in) Display_Format) @key(return) Numeric;
@end{Example}

This function returns the Numeric value for Item, represented in
accordance with Format.  Conversion_Error is propagated if Num is negative
and Format is Unsigned.

@begin{Example}
@key(function) Valid (Item   : @key(in) Packed_Decimal;
                Format : @key(in) Packed_Format) @key(return) Boolean;
@end{Example}

This function returns True if Item has a value consistent with Format,
and False otherwise.  The rules for the formation of Packed_Decimal
values are implementation defined.

@begin{Example}
@key(function) Length (Format : @key(in) Packed_Format) @key(return) Natural;
@end{Example}

This function returns the minimal length of a Packed_Decimal value
sufficient to hold any value of type Num when represented as Format.

@begin{Example}
@key(function) To_Decimal (Item   : @key(in) Packed_Decimal;
                     Format : @key(in) Packed_Format) @key(return) Num;
@end{Example}

Produces a value of type Num corresponding to Item as represented by
Format.  Num'Scale is the number of digits after the assumed radix point
in Item.  Conversion_Error is propagated if the value represented by Item is
outside the range of Num.

@begin{Example}
@key(function) To_Packed (Item   : @key(in) Num;
                    Format : @key(in) Packed_Format) @key(return) Packed_Decimal;
@end{Example}

This function returns the Packed_Decimal value for Item, represented in
accordance with Format.  Conversion_Error is propagated if Num is negative
and Format is Packed_Unsigned.

@begin{Example}
@key(function) Valid (Item   : @key(in) Byte_Array;
                Format : @key(in) Binary_Format) @key(return) Boolean;
@end{Example}

This function returns True if Item has a value consistent with Format,
and False otherwise.
@begin{ramification}
This function returns False only when the represented
value is outside the range of Num.@end{ramification}

@begin{Example}
@key(function) Length (Format : @key(in) Binary_Format) @key(return) Natural;
@end{Example}

This function returns the minimal length of a Byte_Array value
sufficient to hold any value of type Num when represented as Format.

@begin{Example}
@key(function) To_Decimal (Item   : @key(in) Byte_Array;
                     Format : @key(in) Binary_Format) @key(return) Num;
@end{Example}

Produces a value of type Num corresponding to Item as represented by
Format.  Num'Scale is the number of digits after the assumed radix point
in Item.  Conversion_Error is propagated if the value represented by Item is
outside the range of Num.

@begin{Example}
@key(function) To_Binary (Item   : @key(in) Num;
                    Format : @key(in) Binary_Format) @key(return) Byte_Array;
@end{Example}

This function returns the Byte_Array value for Item, represented in
accordance with Format.

@begin{Example}
@key(function) To_Decimal (Item : @key(in) Binary)      @key(return) Num;

@key(function) To_Decimal (Item : @key(in) Long_Binary) @key(return) Num;
@end{Example}

These functions convert from COBOL binary format to a corresponding
value of the decimal type Num.  Conversion_Error is propagated if Item is
too large for Num.
@begin{Ramification}
There is no rescaling performed on the conversion.  That
is, the returned value in each case is a ``bit copy'' if Num has a
binary radix.  The programmer is responsible for maintaining the correct
scale.@end{ramification}

@begin{Example}
@key(function) To_Binary      (Item : @key(in) Num)  @key(return) Binary;

@key(function) To_Long_Binary (Item : @key(in) Num)  @key(return) Long_Binary;
@end{Example}

These functions convert from Ada decimal to COBOL binary format.
Conversion_Error is propagated if the value of Item is too large to be
represented in the result type.
@begin{discussion}
One style of interface supported for COBOL, similar to
what is provided for C, is the ability to call and pass parameters to an
existing COBOL program.  Thus the interface package supplies types
that can be used in an Ada program as parameters to subprograms whose
bodies will be in COBOL.  These types map to COBOL's alphanumeric and
numeric data categories.

Several types are provided for support of alphanumeric data.
Since COBOL's run-time character
set is not necessarily the same as Ada's, Interfaces.COBOL declares
an implementation-defined character type
COBOL_Character, and mappings
between Character and COBOL_Character.
These mappings are visible variables (rather than, say,
functions or constant arrays),
since in the situation where
COBOL_Character is EBCDIC, the
flexibility of dynamically modifying the mappings is needed.
Corresponding to COBOL's alphanumeric data is the string
 type Alphanumeric.

Numeric data may have either a ``display'' or ``computational'' representation
in COBOL.  On the Ada side, the data is of a decimal fixed point type.
Passing an Ada decimal data item to
a COBOL program requires conversion from the Ada decimal type to some type
that reflects the representation expected on the COBOL side.
@begin{Itemize}
Computational Representation

@NoPrefix@;Floating point representation is modeled by Ada floating point types,
Floating and Long_Floating.  Conversion between these types and Ada decimal
types is obtained directly, since the type name serves as a conversion
function.

@NoPrefix@;Binary representation is modeled by an Ada integer type, Binary, and
possibly other types such as Long_Binary.  Conversion between, say, Binary
and a decimal type is through functions from an instantiation of the
generic package Decimal_Conversions.

@NoPrefix@;Packed decimal representation is modeled by the Ada array type Packed_Decimal.
Conversion between packed decimal and a decimal type is through functions
from an instantiation of the generic package Decimal_Conversions.

Display Representation

@NoPrefix@;Display representation for numeric data
is modeled by the array type Numeric.
Conversion between display representation and a decimal type is through
functions from an instantiation of the generic package Decimal_Conversions.
A parameter to the conversion function indicates the desired interpretation
of the data (e.g., signed leading separate, etc.)
@end{Itemize}

Pragma Convention(COBOL, T) may be applied to a record type T to direct the
compiler to choose a COBOL-compatible representation for objects of the
type.

The package Interfaces.COBOL allows the
Ada programmer to deal with data from files (or databases) created by a
COBOL program.  For data that is alphanumeric, or in display
or packed decimal format, the
approach is the same as for passing parameters (instantiate
Decimal_Conversions to obtain the needed conversion functions).  For binary
data, the external representation is treated as a Byte array, and an
instantiation of Decimal_IO produces a package that declares the needed
conversion functions.  A parameter to the conversion function indicates the
desired interpretation of the data (e.g., high- versus low-order byte
first).
@end{discussion}
@end{DescribeCode}
@end{StaticSem}

@begin{ImplReq}
An implementation shall support pragma Convention with
a COBOL @i[convention]_@nt[identifier] for a COBOL-eligible type
(see @RefSecNum(Interfacing Pragmas)).
@begin{ramification}
An implementation supporting this package shall ensure that if the bounds of
a Packed_Decimal, Alphanumeric, or Numeric variable are static,
then the representation of the
object comprises solely the array components (that is, there is no implicit
run-time ``descriptor''
that is part of the object).
@end{ramification}
@end{ImplReq}

@begin{ImplPerm}
An implementation may
 provide additional constants of the private types
Display_Format, Binary_Format, or Packed_Format.
@begin{Reason}
This is to allow exploitation of other external formats that may
be available in the COBOL implementation.@end{reason}

An implementation may
 provide further floating point and integer types
in Interfaces.COBOL to match additional native COBOL types,
and may also supply corresponding conversion functions in the
generic package Decimal_Conversions.
@end{ImplPerm}

@begin{ImplAdvice}
An Ada implementation should support the following interface
correspondences between Ada and COBOL.
@begin[itemize]
An Ada @key[access] T parameter
is passed as a
``BY REFERENCE'' data item of the COBOL type corresponding
to T.

An Ada @key[in] scalar parameter is passed as a ``BY CONTENT'' data item
of the corresponding COBOL type.

Any other Ada parameter is passed as a
``BY REFERENCE'' data item of the COBOL type corresponding
to the Ada parameter type; for scalars, a local copy is
used if necessary to ensure by-copy semantics.
@end[itemize]
@end{ImplAdvice}

@begin[Notes]
An implementation is not required to support pragma Convention
for access types, nor is it required to support pragma Import, Export
or Convention for functions.
@begin{reason}
COBOL does not have a pointer facility, and a COBOL program
does not return a value.@end{reason}

If an Ada subprogram is exported to COBOL, then a call from COBOL
call may specify
 either ``BY CONTENT'' or ``BY REFERENCE''.
@end[Notes]

@begin{Examples}
@i{Examples of Interfaces.COBOL:}
@begin{Example}
@key(with) Interfaces.COBOL;
@key(procedure) Test_Call @key(is)

   @i{-- Calling a foreign COBOL program}
   @i{-- Assume that a COBOL program PROG has the following declaration}
   @i{--  in its LINKAGE section:}
   @i{--  01 Parameter-Area}
   @i{--     05 NAME   PIC X(20).}
   @i{--     05 SSN    PIC X(9).}
   @i{--     05 SALARY PIC 99999V99 USAGE COMP.}
   @i{-- The effect of PROG is to update SALARY based on some algorithm}

   @key(package) COBOL @key(renames) Interfaces.COBOL;

   @key(type) Salary_Type @key(is) @key(delta) 0.01 @key(digits) 7;

   @key(type) COBOL_Record @key(is)
      @key(record)
         Name   : COBOL.Numeric(1..20);
         SSN    : COBOL.Numeric(1..9);
         Salary : COBOL.Binary;  @i{-- Assume Binary = 32 bits}
      @key(end) @key(record);
   @key(pragma) Convention (COBOL, COBOL_Record);

   @key(procedure) Prog (Item : @key(in) @key(out) COBOL_Record);
   @key(pragma) Import (COBOL, Prog, "PROG");

   @key(package) Salary_Conversions @key(is)
      @key(new) COBOL.Decimal_Conversions(Salary_Type);

   Some_Salary : Salary_Type := 12_345.67;
   Some_Record : COBOL_Record :=
      (Name   => "Johnson, John       ",
       SSN    => "111223333",
       Salary => Salary_Conversions.To_Binary(Some_Salary));

@key(begin)
   Prog (Some_Record);
   ...
@key(end) Test_Call;
@end{Example}

@begin{Example}
@key(with) Interfaces.COBOL;
@key(with) COBOL_Sequential_IO; @i{-- Assumed to be supplied by implementation}
@key(procedure) Test_External_Formats @key(is)

   @i{-- Using data created by a COBOL program}
   @i{-- Assume that a COBOL program has created a sequential file with}
   @i{--  the following record structure, and that we need to}
   @i{--  process the records in an Ada program}
   @i{--  01 EMPLOYEE-RECORD}
   @i{--     05 NAME    PIC X(20).}
   @i{--     05 SSN     PIC X(9).}
   @i{--     05 SALARY  PIC 99999V99 USAGE COMP.}
   @i{--     05 ADJUST  PIC S999V999 SIGN LEADING SEPARATE.}
   @i{-- The COMP data is binary (32 bits), high-order byte first}

   @key(package) COBOL @key(renames) Interfaces.COBOL;

   @key(type) Salary_Type      @key(is) @key(delta) 0.01  @key(digits) 7;
   @key(type) Adjustments_Type @key(is) @key(delta) 0.001 @key(digits) 6;

   @key(type) COBOL_Employee_Record_Type @key(is)  @i{-- External representation}
      @key(record)
         Name    : COBOL.Alphanumeric(1..20);
         SSN     : COBOL.Alphanumeric(1..9);
         Salary  : COBOL.Byte_Array(1..4);
         Adjust  : COBOL.Numeric(1..7);  @i{-- Sign and 6 digits}
      @key(end) @key(record);
   @key(pragma) Convention (COBOL, COBOL_Employee_Record_Type);

   @key(package) COBOL_Employee_IO @key(is)
      @key(new) COBOL_Sequential_IO(COBOL_Employee_Record_Type);
   @key(use) COBOL_Employee_IO;

   COBOL_File : File_Type;

   @key(type) Ada_Employee_Record_Type @key(is)  @i{-- Internal representation}
      @key(record)
         Name    : String(1..20);
         SSN     : String(1..9);
         Salary  : Salary_Type;
         Adjust  : Adjustments_Type;
      @key(end) @key(record);

   COBOL_Record : COBOL_Employee_Record_Type;
   Ada_Record   : Ada_Employee_Record_Type;

   @key(package) Salary_Conversions @key(is)
      @key(new) COBOL.Decimal_Conversions(Salary_Type);
   @key(use) Salary_Conversions;

   @key(package) Adjustments_Conversions @key(is)
      @key(new) COBOL.Decimal_Conversions(Adjustments_Type);
   @key(use) Adjustments_Conversions;

@key(begin)
   Open (COBOL_File, Name => "Some_File");

   @key(loop)
     Read (COBOL_File, COBOL_Record);

     Ada_Record.Name := To_Ada(COBOL_Record.Name);
     Ada_Record.SSN  := To_Ada(COBOL_Record.SSN);
     Ada_Record.Salary :=
        To_Decimal(COBOL_Record.Salary, COBOL.High_Order_First);
     Ada_Record.Adjust :=
        To_Decimal(COBOL_Record.Adjust, COBOL.Leading_Separate);
     ... @i{-- Process Ada_Record}
   @key(end) @key(loop);
@key(exception)
   @key[when] End_Error => ...
@key(end) Test_External_Formats;
@end{Example}
@end{Examples}

@LabeledClause{Interfacing with Fortran}

@begin{Intro}
@Defn{interface to Fortran}
@Defn{Fortran interface}
The facilities relevant to interfacing with
the Fortran language
are the package
Interfaces.Fortran and support for the
Import, Export and Convention pragmas with
@i{convention}_@nt{identifier} Fortran.

The package Interfaces.Fortran defines Ada types whose representations are
identical to the default representations of the Fortran intrinsic types
Integer, Real, Double Precision, Complex, Logical, and Character in a
supported Fortran implementation.  These Ada types can therefore be used to
pass objects between Ada and Fortran programs.
@end{Intro}

@begin{StaticSem}
The library package Interfaces.Fortran has the following declaration:
@begin{Example}
@key[with] Ada.Numerics.Generic_Complex_Types;  @i{-- see @RefSecNum{Complex Types}}
@key[pragma] Elaborate_All(Ada.Numerics.Generic_Complex_Types);
@ChildUnit{Parent=[Interfaces],Child=[Fortran],Expanded=[Interfaces.Fortran]}
@key[package] Interfaces.Fortran @key[is]
   @key[pragma] Pure(Fortran);

   @key[type] Fortran_Integer @key[is] @key[range] @i{implementation-defined};
@Hinge{}

   @key[type] Real             @key[is] @key[digits] @i{implementation-defined};
   @key[type] Double_Precision @key[is] @key[digits] @i{implementation-defined};
@Hinge{}

   @key[type] Logical @key[is] @key[new] Boolean;
@Hinge{}

   @key[package] Single_Precision_Complex_Types @key[is]
      @key[new] Ada.Numerics.Generic_Complex_Types (Real);

   @key[type] Complex @key[is] @key[new] Single_Precision_Complex_Types.Complex;

@key[subtype] Imaginary @key[is] Single_Precision_Complex_Types.Imaginary;
   i : Imaginary @key[renames] Single_Precision_Complex_Types.i;
   j : Imaginary @key[renames] Single_Precision_Complex_Types.j;

@Hinge{}
   @key[type] Character_Set @key[is] @i{implementation-defined character type};

   @key[type] Fortran_Character @key[is] @key[array] (Positive @key[range] <>) @key[of] Character_Set;
   @key[pragma] Pack (Fortran_Character);


@key[function] To_Fortran (Item : @key[in] Character) @key[return] Character_Set;
   @key[function] To_Ada (Item : @key[in] Character_Set) @key[return] Character;

   @key(function) To_Fortran (Item : @key(in) String) @key(return) Fortran_Character;
   @key(function) To_Ada     (Item : @key(in) Fortran_Character) @key(return) String;

   @key(procedure) To_Fortran (Item       : @key(in) String;
                         Target     : @key(out) Fortran_Character;
                         Last       : @key(out) Natural);

   @key(procedure) To_Ada (Item     : @key(in) Fortran_Character;
                     Target   : @key(out) String;
                     Last     : @key(out) Natural);

@key[end] Interfaces.Fortran;
@end{Example}
@begin{Ramification}
   The means by which the Complex type is provided in Interfaces.Fortran
   creates a dependence of Interfaces.Fortran on Numerics.Generic_Complex_Types
   (see @RefSecNum{Complex Types}).
   This dependence is intentional and unavoidable,
   if the Fortran-compatible Complex type is to be useful in Ada code
   without duplicating facilities defined elsewhere.
@end{Ramification}

The types
   Fortran_Integer, Real, Double_Precision, Logical,
   Complex, and Fortran_Character are Fortran-compatible.

The To_Fortran and To_Ada functions map between the
Ada  type Character and the Fortran type Character_Set,
and also between the Ada  type String and the Fortran type
Fortran_Character.
 The To_Fortran and To_Ada procedures
 have analogous effects to the string conversion subprograms
found in Interfaces.COBOL.
@end{StaticSem}

@begin[ImplReq]
An implementation shall support @nt[pragma] Convention
with a Fortran @i[convention]_@nt[identifier] for a Fortran-eligible
type (see @RefSecNum(Interfacing Pragmas)).
@end[ImplReq]

@begin{ImplPerm}
An implementation may add additional declarations to the Fortran interface
packages.  For example, the Fortran interface package for an implementation of
Fortran 77 (ANSI X3.9-1978) that defines types like Integer*@i{n}, Real*@i{n},
Logical*@i{n}, and Complex*@i{n} may contain the declarations of types named
Integer_Star_@i{n}, Real_Star_@i{n}, Logical_Star_@i{n}, and
Complex_Star_@i{n}.  (This convention should
 not apply to Character*@i{n}, for
which the Ada analog is the constrained array subtype Fortran_Character
(1..@i{n}).)  Similarly, the Fortran interface package for an implementation of
Fortran 90 that provides multiple @i{kinds} of intrinsic types, e.g. Integer
(Kind=@i{n}), Real (Kind=@i{n}), Logical (Kind=@i{n}), Complex (Kind=@i{n}),
and Character (Kind=@i{n}), may contain the declarations of types
with the recommended names
Integer_Kind_@i{n}, Real_Kind_@i{n}, Logical_Kind_@i{n}, Complex_Kind_@i{n},
and Character_Kind_@i{n}.
@begin[discussion]
Implementations may add auxiliary declarations as needed to assist in the
declarations of additional Fortran-compatible types.  For example, if a double
precision complex type is defined, then Numerics.Generic_Complex_Types may be
instantiated for the double precision type.  Similarly, if a wide character
type is defined to match a Fortran 90 wide character type (accessible in
Fortran 90 with the Kind modifier), then an auxiliary character set may be
declared to serve as its component type.
@end[discussion]
@end{ImplPerm}

@begin{ImplAdvice}
An Ada implementation should support the following interface
correspondences
between Ada and Fortran:
@begin[itemize]
An Ada procedure corresponds to
a Fortran subroutine.

An Ada function corresponds to a Fortran function.

An Ada parameter of an elementary, array, or
record type T is passed as
 a  T@-(F) argument to a Fortran procedure,
where T@-(F) is the Fortran type corresponding to the
Ada type T, and where the INTENT attribute of the corresponding
dummy argument matches the Ada formal parameter mode;
 the Fortran
implementation's parameter passing conventions are used.
For elementary types, a local copy is used if necessary to ensure
by-copy semantics.

An Ada parameter of an access-to-subprogram type
is passed as a reference to a Fortran
procedure whose interface corresponds to the designated subprogram's
specification.
@end[itemize]
@end{ImplAdvice}

@begin[Notes]
An object of a Fortran-compatible record type,
declared in a library package or subprogram,
can correspond to a Fortran common
block; the type also corresponds to
a Fortran ``derived type''.
@end[Notes]
@begin{Examples}
@i{Example of Interfaces.Fortran:}
@begin{Example}
@key[with] Interfaces.Fortran;
@key[use] Interfaces.Fortran;
@key[procedure] Ada_Application @key[is]

   @key[type] Fortran_Matrix @key[is] @key[array] (Integer @key[range] <>,
                                 Integer @key[range] <>) @key[of] Double_Precision;
   @key[pragma] Convention (Fortran, Fortran_Matrix);    @i{-- stored in Fortran's}
                                                   @i{-- column-major order}
   @key[procedure] Invert (Rank : @key[in] Fortran_Integer; X : @key[in] @key[out] Fortran_Matrix);
   @key[pragma] Import (Fortran, Invert);                @i{-- a Fortran subroutine}

   Rank      : @key[constant] Fortran_Integer := 100;
   My_Matrix : Fortran_Matrix (1 .. Rank, 1 .. Rank);
@Hinge{}

@key[begin]

   ...
   My_Matrix := ...;
   ...
   Invert (Rank, My_Matrix);
   ...

@key[end] Ada_Application;
@end{Example}
@end{Examples}
