@Part(frontmatter, root="asis.msm")
@comment{$Source: e:\\cvsroot/ARM/ASIS/concepts.mss,v $}
@comment{$Revision: 1.11 $ $Date: 2009/05/16 03:55:40 $}


@LabeledSection{ASIS technical concepts}


@LabeledClause{Ada compilation environment}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
ASIS is an interface between an Ada environment as defined by
@Chg{Version=[2],New=[the Ada Standard (ISO/IEC 8652:1995(E) and later
documents)], Old=[ISO/IEC 8652:1995 (the Ada Reference Manual)]} and any tool
requiring information from this environment, as shown in Figure 2.

@PictureAlone(Alignment=[Center], Border=[None],
         Height=[427], Width=[456],
         Name=[ada_env.png],
         Descr=[ASIS as interface to Ada compilation environment])
@Comment{Original size:  Height=[483], Width=[515]}

@b{Figure 2 @em ASIS as interface to Ada compilation environment}

@LabeledSubClause{Ada environment}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@Chg{Version=[2],New=[The Ada Standard],Old=[ISO/IEC 8652:1995]},
10.1.4(1) provides a notion for this compilation
@i{environment} as:@Defn{compilation environment} @ldquote@;Each compilation
unit submitted to the compiler is compiled in the context of an environment
declarative_part (or simply environment), which is a conceptual
declarative_part that forms the outermost declarative region of the context of
any compilation. At run time, an environment forms the declarative_part of the
body of the environment task of a partition.@rdquote


@LabeledSubClause{ASIS notion of the Ada compilation environment}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[The],Old=[However, the]} mechanisms for creating an
environment and for adding and replacing compilation units within an environment
are implementation-defined. @Chg{Version=[2],New=[In some implementations,
environments are represented by a],Old=[Some implementor environments create and
maintain]} persistent @Chg{Version=[2],New=[database,],Old=[databases]} while
@Chg{Version=[2],New=[in ],Old=[]}others @Chg{Version=[2],New=[they
are],Old=[do]} not. Consequently, ASIS requires the user of the interface (i.e.,
ASIS application) to establish the compilation environment. This is done through
the context.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
The @i{context}@Defn{context} @Chg{Version=[2],New=[is a view of an
Ada environment, and ],Old=[]}defines a set of compilation units and
configuration pragmas @Chg{Version=[2],New=[to be ],Old=[]}processed by an
ASIS application. ASIS provides any information from a context by treating
this set as if its elements make up an environment declarative
part@Chg{Version=[2],New=[],Old=[ by modeling some view (most likely @en one
of the views of the underlying Ada implementation) on the environment. Context
is a view of an Ada environment]}. ASIS requires an application to identify
@Chg{Version=[2],New=[the],Old=[that]} view of the environment
@Chg{Version=[2],New=[to be provided by the context ],Old=[]}using
the procedure Asis.Ada_Environments.Associate, as
shown in Figure 3. ASIS may process several different contexts at a time.

@PictureAlone(Alignment=[Center], Border=[None],
         Height=[282],
         Width=[507],
         Name=[asis_cont.png],
         Descr=[Application interface to ASIS Context])
@Comment{Original size:  Height=[319], Width=[573]}

@b{Figure 3 @em Application interface to ASIS Context}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
A context may have one or more Compilation_Units. ASIS has defined
Compilation_Unit as an ASIS private type. This type has values which denote an
Ada compilation unit@Defn{Compilation unit} or configuration
pragma@Defn{Configuration pragma} from the environment. Compilation_Unit also
is an abstraction, which represents information about some physical object from
the @ldquote@;external world@rdquote@;. This physical object is treated by the
underlying Ada implementation as the corresponding Ada compilation unit or as a
result of compiling a configuration pragma. An ASIS @i{compilation unit} includes
the notion of some implementation-defined way to associate the corresponding
ASIS object with some physical external object. This is necessary to support
ASIS queries such as Time_Of_Last_Update and Text_Name which have no relation
to the @Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}-defined
notion of an Ada compilation unit.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
To facilitate the use of context, implementations may support the use of
@i{containers} which are logical collections of ASIS compilation
units.@Defn{Container} For example, some containers can hold compilation units
@Chg{Version=[2],New=[that declare],Old=[which include]} Ada predefined types;
another container can hold implementation-defined packages. Containers provide
an implementation-defined way of grouping the compilation units accessible for
an ASIS application through the ASIS queries.


@LabeledSubClause{Illegal / inconsistent units in the compilation Environment}

Ada Implementation permissions allow for illegal and inconsistent units to be
in the environment. Because the contents of the Ada environment are
Ada-implementation-defined, the ASIS context can contain illegal compilation
units. The use of ASIS can result in the exception ASIS_Failed being raised if
the Ada environment includes such units.


@LabeledClause{ASIS queries}


ASIS queries are provided in the form of structural (syntactic) and semantic
queries to the Ada compilation environment.


@LabeledSubClause{Structural queries}

@leading@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@i{Structural queries}@Defn{Structural queries}@Defn2{Term=[Queries],Sec=[Structural]}
are those ASIS queries which provide the top-down
decomposition and reverse bottom-up composition of the compilation unit
according to its syntax structure. @Chg{Version=[2],New=[Structural queries
are the primary component of the syntactic
subsystem@Defn{syntactic subsystem}@Defn2{Term=[subsystem],Sec=[syntactic]}, and are only found in
that subsystem. ],Old=[]}These structural queries are further characterized as:
@begin{Itemize}
"Black-box" queries are those ASIS queries which produce information about
compilation units.

"White-box" queries are those ASIS queries which produce information about
lexical elements of compilation units.
@end{Itemize}

@LabeledSubClause{Semantic queries}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@i{Semantic queries} are those ASIS queries
which expression semantic properties (that is, the meaning) of constructs
in the compilation unit.]}

@leading@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Semantic queries],Old=[@i{Semantic queries}]}@Defn{Semantic queries}@Defn2{Term=[Queries],Sec=[Semantic]}
@Chg{Version=[2],New=[in the syntactic subsystem],Old=[are those ASIS queries which]}
express semantic properties of ASIS Elements in
terms of other Elements. There are three kinds of semantic queries in
@Chg{Version=[2],New=[the syntactic subsystem of ],Old=[]}ASIS:
@begin{Itemize}
Semantic queries about Elements,

Semantic queries about Compilation Units, and

Semantic queries about Dependence Order.
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[In addition, semantic queries are provided by
the semantic subsystem
subsystem@Defn{semantic subsystem}@Defn2{Term=[subsystem],Sec=[semantic]}(see
section @RefSecNum{ASIS Semantic Subsystem})
which defines a largely self-contained set of packages defining the
semantics of the program in terms of Views and Declarations of
various kinds of semantic entities, such as Objects and Subtypes.
The semantic subsystem is linked to the structural queries through
Element_Denoting_View, and from the structural queries through
Corresponding_View, Corresponding_Subtype_View, and
Corresponding_View_Declaration.]}

@LabeledSubClause{General ASIS query processing}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1],ARef=[SI99-0047-1]}
Both structural (syntactic) and semantic queries are facilitated through the
notion of ASIS elements and their kinds. Most ASIS
queries @Chg{Version=[2],New=[(outside the semantic subsystem) ],Old=[]}provide
for the processing of specific constructs
@Chg{Version=[2],New=[and lists thereof ],Old=[]}with respect to the
Ada @Chg{Version=[2],New=[Standard],Old=[Reference Manual and
the processing of their lists]}.


@LabeledSubSubClause{Elements and element kinds}

@leading@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
The base object in ASIS is the Asis.Element.@Defn{Element} @i{Element} is a
common abstraction used to @Chg{Version=[2],New=[represent syntactic
constructs],Old=[denote the syntax]} (both explicit and implicit)
@Chg{Version=[2],New=[occurring within],Old=[of]}
ASIS compilation units. Elements correspond to nodes of a hierarchical tree
representation of an Ada program. Most elements of the tree have child
elements. These children can appear as single elements (possibly with children
themselves) or as a list of elements (also possibly with children). As an
example, @Chg{Version=[2],New=[consider],Old=[think of]} an Ada object
declaration having three sub-parts or children:

@begin{Itemize}
A list of identifiers,

A reference to a subtype (subtype indication),

An initialization expression (possibly absent)
@end{Itemize}

@leading@;Thus, the declaration:
@begin{Example}
A, B : Latitude := 0.0;
@end{Example}
has a corresponding tree as in Figure 4.

@PictureAlone(Alignment=[Center], Border=[None],
         Height=[271],
         Width=[497],
         Name=[syn_tree.png],
         Descr=[Syntactic tree representation of an Ada object declaration])
@Comment{Original size:  Height=[306], Width=[562]}

@b{Figure 4 @em Syntactic tree representation of an Ada object declaration}

ASIS Elements are either @i{explicit elements}@Defn{Explicit elements}@Defn2{Term=[Element],Sec=[Explicit]},
representing a language construct
that appears explicitly in the program text for the compilation unit, or
@i{implicit elements}@Defn{Implicit elements}@Defn2{Term=[Element],Sec=[Implicit]},
representing a language construct that does not exist in the
program text for the compilation unit, but could occur at a given place in the
program text as a consequence of the semantics of another construct (e.g., an
implicit declaration, a generic instantiation).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
ASIS provides the ability to visit elements of the tree and ask questions of
each element. One key hierarchy of questions begins with: @ldquote@;What kind
of element
do I have?@rquote Elements of the highest level of the ASIS hierarchy are
classified into kinds. The following table identifies the high level hierarchy
of kinds, the primary ASIS package to support processing of those kinds, and
the references for those kinds in the
Ada @Chg{Version=[2],New=[Standard],Old=[Reference Manual]}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@table{Columns=[3],Alignment=[Allleft],FirstColWidth=[1],LastColWidth=[2],
NoBreak=[F],Border=[F],SmallSize=[F],Caption=[],Headers=[],
Body=[A_Defining_Name@\Asis.Definitions@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  3, 6
A_Declaration@\Asis.Declarations@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  3, 5, 6, 7, 8, 9, 10, 11, 12
A_Definition@\Asis.Definitions@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  3, 7, 9, 12
An_Expression@\Asis.Expressions@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  2, 4
A_Statement@\Asis.Statements@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  5, 6, 9, 11, 13
A_Path@\Asis.Statements@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  5, 9
A_Clause@\Asis.Clauses@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  8, 10, 13
An_Association@\Asis.Expressions@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  2, 3, 4, 6, 12
An_Exception_Handler@\Asis.Statements@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  11.2@last
A_Pragma@\Asis.Elements@\@Chg{Version=[2],New=[Ada Standard],Old=[Reference Manual]}  2, 10, 11, 13, B, G, H, I, L]}

The function Asis.Elements.Element_Kind classifies any element into one of
these kinds. Once the client knows that an element is a declaration, for
example, it can further classify the element as to what kind of declaration it
is with the Asis.Elements.Declaration_Kind function. This leads to case
structures like the following.

@begin{Examples}
@leading@;Case statement to classify elements:
@begin{Example}
@key[case] Asis.Elements.Element_Kind (My_Element) @key[is]            -- @examcom{@RefSecNum{function Element_Kind}}
   @key[when] Asis.A_Declaration =>                              -- @examcom{ @RefSecNum{type Element_Kinds}}
      @key[case] Asis.Elements.Declaration_Kind (My_Element) @key[is]  -- @examcom{@RefSecNum{function Declaration_Kind}}
         @key[when] Asis.A_Variable_Declaration =>               -- @examcom{ @RefSecNum{type Declaration_Kinds}}
            { statement }
         @key[when others] =>
            @key[null];
      @key[end case];
   @key[when] Asis.A_Statement =>                                -- @examcom{ @RefSecNum{type Element_Kinds}}
      @key[case] Asis.Elements.Statement_Kind (My_Element) @key[is]    -- @examcom{@RefSecNum{function Statement_Kind}}
         @key[when] Asis.A_Block_Statement =>                    -- @examcom{ @RefSecNum{type Statement_Kinds}}
            @i[{ statement }]
         @key[when others] =>
            @key[null];
      @key[end case];
   @key[when others] =>
      @key[null];
@key[end case];
@end{Example}

In this example, variable declarations and block statements will presumably be
processed further. All other element kinds are ignored by falling into the null
@b{when others} alternative.
@end{Examples}


@LabeledSubSubClause{Processing specific constructs}

@leading@;Once this level of classification is determined, ASIS provides a set
of functions for processing a specific statement, declaration, or other element
kinds. The following functions are available for processing object
declarations:

@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@key[function] Names (Declaration : Asis.Declaration)                       -- @examcom{@RefSecNum{function Names}}
      @key[return] Asis.Defining_Name_List;
@key[function] @Chg{Version=[2],New=[Object_Declaration_Subtype],Old=[Object_Declaration_View]} (Declaration : Asis.Declaration)     -- @examcom{@Chg{Version=[2],New=[@RefSecNum{function Object_Declaration_Subtype}],Old=[@RefSecNum{function Object_Declaration_View}]}}
      @key[return] Asis.Definition;
@key[function] Initialization_Expression (Declaration : Asis.Declaration)   -- @examcom{@RefSecNum{function Initialization_Expression}}
      @key[return] Asis.Expression;
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
These functions correspond to each of the three parts of an object declaration.
Once it is classified into its kind, each element can be processed further with
the provided functions. Some functions @ldquote@;traverse@rdquote@Defn{Traverse}
to other child elements such
as the @Chg{Version=[2],New=[Object_Declaration_Subtype],Old=[Object_Declaration_View]}
and Initialization_Expression functions above.
Since the kind of element that is returned for each of these functions is
already known, processing can continue directly with the functions in the
Asis.Definitions package (Section @RefSecNum{package Asis.Definitions})
that accept @Chg{Version=[2],New=[A_Subtype_Indication],Old=[A_Type_Definition]}
elements or the Asis.Expressions package (Section @RefSecNum{package Asis.Expressions}) that
accept An_Expression elements.
The exception ASIS_Inappropriate_Element is raised whenever an ASIS function is
passed an element it is not intended to process. Parameter names and subtype
names in the function specifications indicate what kind of input element is
expected and what kind of element is returned.


@LabeledSubSubClause{Element list processing}

Lists of elements are processed with a loop iteration scheme in the following
manner:

@begin{Examples}
@leading@;Loop iteration scheme

@begin{Example}
   List : constant Asis.Element_List :=                  -- @examcom{@RefSecNum{type Element_List}}
       <@i{ASIS function returning a list}>;
   An_Element : Asis.Element;                            -- @examcom{@RefSecNum{type Element}}
begin
   for I in List'Range loop
       An_Element := List (I);
       Process (An_Element);
   end loop;
@end{Example}
@end{Examples}

Functions that return Element_List types generally indicate what type of
elements are in the list they return. Thus, processing can sometimes continue
with specific calls without first asking what type of element is being
processed.


@LabeledSubSubClause{Operations that apply to all elements}

Figure 5 depicts operations which, in general, apply to all elements.

@PictureAlone(Alignment=[Center], Border=[None],
         Height=[412],
         Width=[499],
         Name=[ele_op.png],
         Descr=[Operations on elements])
@Comment{Original size:  Height=[466], Width=[564]}

@b{Figure 5 @em Operations on elements}

@leading@;The ASIS packages provide some general interfaces that operate on all
nodes, such as:

@begin{Itemize}
Asis.Elements.Element_Kind (@RefSecNum{function Element_Kind}) @en Returns the
Element_Kind for the element. Once the Element_Kind is known, the Element can
be decomposed into its component elements.

Asis.Elements.Enclosing_Element (@RefSecNum{function Enclosing_Element}) @en
Returns the element that contains the current element (one element
up in the tree hierarchy).

Asis.Elements.Enclosing_Compilation_Unit (@RefSecNum{function Enclosing_Compilation_Unit})
@en Returns the Compilation_Unit that contains the given element.

Asis.Text.First_Line_Number (@RefSecNum{function First_Line_Number}) @en
Returns the first line number in which the text of the element resides.

Asis.Text.Last_Line_Number (@RefSecNum{function Last_Line_Number}) @en Returns
the last line number in which the text of the element resides.

Asis.Text.Element_Span (@RefSecNum{function Element_Span}) @en Returns the span
for the element.

Asis.Text.Lines (@RefSecNum{function Lines (element)}) @en Returns a list of lines for
the element.

Asis.Text.Element_Image (@RefSecNum{function Element_Image}) @en Returns the
program text image of any element.

Asis.Elements.Hash (@RefSecNum{function Hash}) @en Returns a convenient name
for an object of type Asis.Element.

Asis.Ids.Create_Id (@RefSecNum{function Create_Id}) @en Returns a unique Id
value corresponding to this element.

Asis.Elements.Is_Nil (@RefSecNum{function Is_Nil (element)}) @en Determines
whether an element is nil. Some functions return a Nil_Element when a potential
element does not exist in the program. This is true for the
Initialization_Expression function above when no initial value is present in
the declaration.

@end{Itemize}


@LabeledSubSubClause{Semantic references}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
References from one part of an Ada program to another can be traversed with
functions whose name begins with @ldquote@;Corresponding_@rdquote@;, such as:
Corresponding_@!Children, Corresponding_@!Declaration, Corresponding_Body,
@Chg{Version=[2],New=[Corresponding_@!Expression_Type, ],Old=[]}Corresponding_@!Type_Declaration,
Corresponding_Type, Corresponding_@!Body_Stub,
Corresponding_@!Name_Definition, Corresponding_@!Name_Declaration,
Corresponding_@!Loop_Exited, Corresponding_@!Entry, etc. If an element references
another element, the user can traverse to the referenced element with
@Chg{Version=[2],New=[such a],Old=[this]} function. A Nil_Element is returned if
no definition traversal is possible.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[The most fundamental such function is
Corresponding_Name_Definition. This function traverses from a Name
to the corresponding defining name, represented by an element of kind
A_Defining_Name. From this point a traversal using the Enclosing_Element
function can be used to arrive at the declaration of the corresponding program
entity. Corresponding_@!Name_Declaration combines these two traversals into a
single operation. Corresponding_@!Expression_Type traverses from an Expression to
the declaration of its type.],Old=[A slightly more precise statement of the
operation of this function is that identifier references point to identifier
definitions. Traversal arrives at the identifier definition where the
Enclosing_Element function is used to arrive at the complete element
declaration.]}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
For example, the user can traverse to the @Chg{Version=[2],New=[declaration of
the type for an object named in an expression],Old=[type declaration referenced
in an object declaration]} with the Corresponding_Expression_Type function as
shown in Figure 6. Figure 6 also depicts links using the semantic query
Corresponding_Name_Declaration. (Note: the thin lined-arrows depict syntactic
queries while the thick-lined arrows depict semantic queries.)

@PictureAlone(Alignment=[Center], Border=[None],
         Height=[557],
         Width=[511],
         Name=[sem_ref.png],
         Descr=[Semantic reference using corresponding queries])
@Comment{Original size:  Height=[629], Width=[577]}

@b{Figure 6 @em Semantic reference using corresponding queries}


@LabeledClause{ASIS package architecture}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Figure 7 depicts the package architecture for the ASIS
interface@Chg{Version=[2],New=[ (not including the semantic subsystem)],Old=[]}. A tool or
application using ASIS has visibility to the entire declarative region of
package Asis including all child packages. To get a better understanding of how
these packages are used, see the examples in
@RefSecNum{Asis Application Examples}.

@PictureAlone(Alignment=[Center], Border=[None],
         Height=[534],
         Width=[477],
         Name=[asis_ref.png],
         Descr=[ASIS package architecture])
@Comment{Original size:  Height=[603], Width=[539]}


@b{Figure 7 @em ASIS package architecture}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@b{package ASIS} @en Package ASIS, together with its children, provides the
interface between an Ada environment (as defined by @Chg{Version=[2], New=[the
Ada Standard], Old=[ISO/IEC 8652:1995]}) and any
tool requiring information from it. Valuable semantic and syntactic information
is made available via queries through child packages of package ASIS.

@leading@;Package Asis contains common types and subtypes used for the ASIS
interface and its child packages. Important common types include (see Section
@RefSecNum{package ASIS}):
@begin{Itemize}
Type Context@Defn{Context} helps identify the compilation units considered to
be analyzable as part of the Ada compilation environment.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Type Element@Defn{Elememt} is an abstraction of
@Chg{Version=[2],New=[constructs],Old=[entities]} within a logical Ada syntax
tree.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Element Kinds are a set of enumeration types @Chg{Version=[2],New=[that
characterize the constructs of],Old=[providing a mapping to the]} Ada syntax.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Type Compilation_Unit@Defn{Compilation_Unit} is an abstraction
@Chg{Version=[2],New=[that represents an],Old=[for]} Ada compilation
@Chg{Version=[2],New=[unit],Old=[units]}.

Unit Kinds are a set of enumeration types describing the various kinds of
compilation units.

Type Traverse_Control@Defn{Traverse_Control} provides a mechanism to control
iterative traversals of a logical syntax tree.

Type Program_Text@Defn{Program_Text} provides an abstraction for the program
text for a program's source code.
@end{Itemize}

@leading@;Package Asis also encapsulates implementor-specific declarations,
which are made available to ASIS and its client applications in an
implementor-independent manner. Package ASIS is the root of the ASIS interface.
All other packages are child packages of package Asis. These packages are:

@begin{Itemize}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0030-1]}
@b{Asis.Ada_Environments}@Defn{Ada_Environments} @en This child package
encapsulates a set of queries that map physical Ada compilation and program
execution environments to logical ASIS environments. An ASIS Context is
associated with some set of Ada compilation units maintained by an underlying
Ada implementation. After this association has been made, this set of units is
considered to be part of the compile-time Ada environment, which forms the
outermost context of any compilation, as specified in section 10.1.4 of the Ada
@Chg{Version=[2],New=[Standard],Old=[Reference manual]}. This same environment
context provides the implicit outermost anonymous task during program execution.
If an Ada implementation supports the notion of a program library or
@ldquote@;library@rdquote as specified in section 10(2) of the Ada
@Chg{Version=[2],New=[Standard],Old=[Reference Manual]}, then an ASIS Context
value can be mapped onto one or more implementor libraries represented by
Containers. More than one context may be manipulated at a time. Important
interfaces include: Associate, Dissociate, Open, and Close (see Section
@RefSecNum{package Asis.Ada_Environments}). The type Container and its
supporting functions are provided in the child package
@b{Asis.Ada_Environments.Containers} (see Section
@RefSecNum{package Asis.Ada_Environments.Containers}).

@b{Asis.Implementation}@Defn{Implementation} @en This child package provides operations to
initialize and finalize the ASIS interface. It also provides queries for the
error status of the ASIS implementation (see Section
@RefSecNum{package Asis.Implementation}). Its child package
@b{Asis.Implementation.Permissions} provides queries to determine options used
by an implementation (see Section
@RefSecNum{package Asis.Implementation.Permissions}).

@b{Asis.Compilation_Units}@Defn{Compilation_Units} @en This child package
encapsulates a set of queries that implement the ASIS Compilation_Unit
abstraction. It defines queries that deal with Compilation_Units and the gateway
queries between Compilation_Units, Elements, and Ada_Environments. More than one
compilation unit may be manipulated at one time (see Section
@RefSecNum{package Asis.Compilation_Units}). The child package
@b{Asis.Compilation_Units.Times}@Defn{Compilation_Units.Times} encapsulates the
time related functions used within ASIS (see
Section @RefSecNum{package Asis.Compilation_Units.Times}). A second child
package, @b{Asis.Compilation_Units.Relations}@Defn{Compilation_Units.Relations}
encapsulates the semantic relationship concepts used in ASIS. Relation queries
provide references to compilation units that are related, in some specific
fashion, to one or more given compilation units (see Section
@RefSecNum{package Asis.Compilation_Units.Relations}).

@b{Asis.Iterator}@Defn{Iterator} @en This child package encapsulates the Traverse_Element
mechanism to perform an iterative traversal of a logical syntax tree. During
the traversal, ASIS can analyze the various elements present and provide
application processing via generic procedures instantiated by the application
using queries to decompose ASIS elements into the logical semantic tree of the
Ada program (see Section @RefSecNum{package Asis.Iterator}). This key mechanism
is the heart of most ASIS tools; examples of its use are provided in
@RefSecNum{Asis Application Examples}.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@b{Asis.Elements}@Defn{Elements} @en This child package encapsulates a set of
queries that operate on all elements and some queries specific to A_Pragma
elements. @Chg{Version=[2],New=[The element kinds],Old=[Element_Kinds]},
defined in package Asis@Chg{Version=[2],New=[,],Old=[]} are defined
as@Chg{Version=[2],New=[ a set of],Old=[]} enumeration
types describing the various kinds of elements. ASIS offers a hierarchical
classification of elements. At the highest level, the Element_Kinds type
provides literals that define "kinds" or classes into which all non-nil elements
are grouped. Element_Kinds are: Not_An_Element, A_Pragma, A_Defining_Name,
A_Declaration, A_Definition, An_Expression, An_Association, A_Statement, A_Path,
A_Clause, and An_Exception_Handler. Elements in each of the Element_Kinds
classes, with the exception of An_Exception_Handler, can be further classified
by a subordinate kind at the next level in the hierarchy (see Section
@RefSecNum{package Asis.Elements}).

@b{Asis.Clauses}@Defn{Clauses} @en This child package encapsulates a set of
queries that operate on the A_Clause element (see
Section @RefSecNum{package Asis.Clauses}).

@b{Asis.Declarations}@Defn{Declarations} @en This child package encapsulates a
set of queries that operate on A_Defining_Name and A_Declaration elements (see
Section @RefSecNum{package Asis.Declarations}).

@b{Asis.Definitions}@Defn{Definitions} @en This child package encapsulates a set
of queries that operate on A_Definition elements (see
Section @RefSecNum{package Asis.Definitions}).

@b{Asis.Expressions}@Defn{Expressions} @en This child package encapsulates a set
of queries that operate on An_Expression and An_Association elements (see Section
@RefSecNum{package Asis.Expressions}).

@b{Asis.Statements}@Defn{Statements} @en This child package encapsulates a set
of queries that operate on A_Statement, A_Path, and An_Exception_Handler
elements (see Section @RefSecNum{package Asis.Statements}).

@b{Asis.Text}@Defn{Text} @en This child package encapsulates a set of operations
to access the text of ASIS Elements. This text is represented as logical
@i{lines} from the source code of the external representation of a compilation
unit. Type Line is defined to support program text operations (see Section
@RefSecNum{package Asis.Text}). It assumes no knowledge of the existence,
location, or form of the program text.

@b{Asis.Errors}@Defn{Errors} @en This child package provides an enumeration type
identifying the error kinds used for the ASIS interface (see Section
@RefSecNum{package Asis.Errors}).

@b{Asis.Exceptions}@Defn{Exceptions} @en This child package identifies all
defined ASIS exceptions (see Section @RefSecNum{package Asis.Exceptions}).

@b{Asis.Ids}@Defn{Ids} @en This child package encapsulates a set of operations
and queries that implement the ASIS Id abstraction. An Id is a way to identify a
particular element (i.e., a unique reference to an Element) which is efficient
and persistent as long as the environment is not recompiled (see Section
@RefSecNum{package Asis.Ids}).

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0035-1]}
@b{Asis.Data_Decomposition}@Defn{Data_Decomposition} @en This optional child
package encapsulates a set
of operations to decompose data values using the ASIS type information and a
portable data stream, representing a data value of that type (see Section
@RefSecNum{package Asis.Data_Decomposition (optional)}).@Chg{Version=[2],New=[],
Old=[Its child package
@b{Asis.Data_Decomposition.Portable_Transfer} provides support for logging and
delogging of application data using ASIS. Internal packages of
Asis.Data_Decomposition.Portable_Transfer include:
Portable_Constrained_Subtype, Portable_Unconstrained_Record_Type,
Portable_Array_Type_1 (for one dimensional arrays), Portable_Array_Type_2 (for
two dimensional arrays), and Portable_Array_Type_3 (for three dimensional
arrays) (see Section
@RefSecNum{Obsolescent package Asis.Data_Decomposition.Portable_Transfer}).]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Views}@Defn{Views} @en This child package
provides the basis of the semantic subsystem. A view represents a semantic
entity in an Ada program, whether or not the entity has a syntactic
representation. This package
includes mechanisms for finding the associated element (if any). (see Section
@RefSecNum{ASIS Semantic Subsystem}).]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Declarations.Views}@Defn{Declarations.Views}
@en This child package provides a path to move from a syntactic declaration
(an element) to a semantic one (a view).]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Definitions.Views}@Defn{Definitions.Views}
@en This child package provides a path to move from a syntactic definition
(an element) to a semantic one (a view).]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Expressions.Views}@Defn{Expressions.Views}
@en This child package provides a path to move from a syntactic expression
(an element, which can be a value or object) to a semantic one (a view).]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Program_Units}@Defn{Program_Units} @en This
child package provides a semantic representation of program units, including
ones that don't exist explicitly, like inherited subprograms and entities
declared by generic instances. It also provides a set of queries on this
semantic representation.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Subtype_Views}@Defn{Subtype_Views} @en This
child package provides queries on semantic views of (all) subtypes.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Subtype_Views.Elementary}@Defn{Subtype_Views.Elementary}
@en This child package provides queries specifically
for semantic views of elementary subtypes.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Subtype_Views.Composite}@Defn{Subtype_Views.Composite}
@en This child package provides queries specifically for semantic
views of composite subtypes.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Object_Views}@Defn{Object_Views} @en This
child package provides queries on semantic views of values and objects.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Object_Views.Access_Views}@Defn{Object_Views
.Access_Views} @en This child package provides queries on semantic views of
values and objects of access types.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Profiles}@Defn{Profiles} @en This child
package provides the definition of and queries on subprogram profiles.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Callable_Views}@Defn{Callable_Views} @en
This child package provides queries on semantic views of entities that can be
called (subprograms and entries).]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Package_Views}@Defn{Package_Views} @en This
child package provides queries on semantic views of packages units.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Generic_Views}@Defn{Generic_Views} @en This
child package provides queries on semantic views of generic units.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Exception_Views}@Defn{Exception_Views} @en
This child package provides queries on semantic views of exceptions.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0047-1]}
@ChgAdded{Version=[2],Text=[@b{Asis.Statement_Views}@Defn{Statement_Views} @en
This child package provides queries on semantic views of statements.]}

@end{Itemize}


@LabeledClause{Application use}


The ASIS Interface is provided through child packages of package ASIS. Complete
executable examples of application use are provided in
@RefSecNum{Asis Application Examples}. This section
discusses the establishment of ASIS context, required sequencing of calls,
erroneous applications, and usage rules.


@LabeledSubClause{Establishing ASIS context}

An application using ASIS has a context clause for package ASIS and the child
packages needed by the application.

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
An application using all @Chg{Version=[2],New=[non-obsolescent ],Old=[]}child
packages includes the following in its context clause:

@begin{Example}
@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[@key[with]],Old=[]}Asis, Asis.Errors,
  Asis.Compilation_Units, Asis.Compilation_Units.Times,
  Asis.Compilation_Units.Relations, Asis.Ada_Environments, Asis.Implementation,
  Asis.Exceptions, Asis.Elements, Asis.Iterator, Asis.Declarations,
  Asis.Expressions, Asis.Clauses, Asis.Definitions, Asis.Statements, Asis.Text,
  Asis.Ids, Asis.Data_Decomposition@Chg{Version=[2],New=[, Asis.Views,
  Asis.Program_Units, Asis.Subtype_Views, Asis.Subtype_Views.Elementary,
  Asis.Subtype_Views.Composite, Asis.Object_Views, Asis.Object_Views.Access_Views,
  Asis.Profiles, Asis.Callable_Views, Asis.Package_Views,
  Asis.Generic_Views, Asis.Exception_Views, Asis.Statement_Views,
  Asis.Declarations.Views, Asis.Definitions.Views,
  Asis.Expressions.Views;],Old=[; and Asis.Data_Decomposition.Portable_Transfer.]}
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
Only these packages should be referenced (@key[with]ed) by
a @Chg{Version=[2],New=[newly written ],Old=[]}portable
application. @Chg{Version=[2],New=[Obsolescent packages should not be
used in new applications; other],Old=[Other]} packages, which may be present in
a specific ASIS implementation, are not part of this International Standard.


@LabeledSubClause{Required sequencing of calls}


@leading@;An ASIS application shall use the following required sequencing of
calls:

@begin{Example}
a)  Asis.Implementation.Initialize;         -- @examcom{Initialize the ASIS interface}
b)  Asis.Ada_Environments.Associate(..);    -- @examcom{Name an Ada Environment}
c)  Asis.Ada_Environments.Open(..);         -- @examcom{Access an Ada Environment}
d)  Use the various ASIS queries.           -- @examcom{Fetch a unit, its attributes,}
                                            -- @examcom{get its Unit_Declaration element,}
                                            -- @examcom{traverse its elements, etc.}
e)  Asis.Ada_Environments.Close(..);        -- @examcom{Drop access to an Ada Environment}
f)  Asis.Ada_Environments.Dissociate(..);   -- @examcom{Release the Ada Environment name}
g)  Asis.Implementation.Finalize;           -- @examcom{Release all resources}
@end{Example}

These calls may be used in a loop. More than one element may be manipulated at
one time. (The exact number is subject to implementation/target specific
limitations). An element, obtained from a compilation unit, can continue to be
manipulated while the Context, from which the element's compilation unit was
obtained, remains open.


@LabeledSubClause{Notional ASIS application}

@leading@;ASIS Applications may take on many forms. This section is intended to
present a notional ASIS Application using the example of a simple restrictions
checker. A restrictions checker is intended to visit every element in an ASIS
context to determine if a violation of a safety-critical check has been made.
The ASIS context could include all compilation units in the Ada application.
Such a restrictions checker might contain a large number of restrictions to
check. The example restrictions checker looks for violations of two
safety-critical guidelines:

a) Short circuit operators are always used
(i.e., @key[or else] and @key[and then] are used and @key[or] and @key[and]
are not used).@*
b) Tasks are declared at the library level.

A procedure, named Process_Element, is created which contains calls to
procedures Check_Short_Circuit and Check_Library_Level_Task, which performs a
restrictions check for each of our safety-critical guidelines above.

@begin{Example}
@key[procedure] Process_Element (Elem       : @key[in] Asis.Element;               -- @examcom{@RefSecNum{type Element}}
                           Control    : @key[in out] Asis.Traverse_Control;  -- @examcom{@RefSecNum{type Traverse_Control}}
                           Dummy      : @key[in out] Boolean) @key[is]

@key[begin]

   Check_Short_Circuit (Elem);
   Check_Library_Level_Task (Elem);

   -- @examcom{Additional guidelines can be checked here.}

@key[end] Process_Element;
@end{Example}

This procedure will process each element in the Context as controlled by an
instantiation to Traverse_Element. The body of the Check_Short_Circuit and
Check_Library_Level_Task identify the processing to be performed on each
Element.

The Check_Short_Circuit procedure is passed the current Element to be
evaluated. The Operator_Kinds function identifies the operator kind. If the
Element happens to be An_And_Operator or An_Or_Operator, then a violation
exists and must be reported by identifying the line number of the violation.
Otherwise, there is no processing for this Element.

@begin{Example}
@key[procedure] Check_Short_Circuit (Elem : @key[in] Asis.Element) @key[is]             -- @examcom{ @RefSecNum{type Element}}
   Op_Kind : Asis.Operator_Kinds :=                                   -- @examcom{ @RefSecNum{type Operator_Kinds}}
                 Asis.Elements.Operator_Kind (Elem);                  -- @examcom{@RefSecNum{function Operator_Kind}}

@key[begin]
   @key[case] Op_Kind @key[is]

      @key[when] Asis.An_And_Operator =>                                    -- @examcom{ @RefSecNum{type Operator_Kinds}}
         Put_Line ("Violation of Short Circuit Operator guideline:");
         Put ("-- Use of AND Operator at line ");
         Put (Asis.Text.Line_Number'Wide_Image                        -- @examcom{@RefSecNum{subtypes Line_Number and Line_Number_Positive}}
                      (Asis.Text.First_Line_Number (Elem)));          -- @examcom{@RefSecNum{function First_Line_Number}}
         New_Line;

      @key[when] Asis.An_Or_Operator =>                                     -- @examcom{ @RefSecNum{type Operator_Kinds}}
         Put_Line ("Violation of Short Circuit Operator guideline:");
         Put ("-- Use of OR Operator at line ");
         Put (Asis.Text.Line_Number'Wide_Image                        -- @examcom{@RefSecNum{subtypes Line_Number and Line_Number_Positive}}
                (Asis.Text.First_Line_Number (Elem)));                -- @examcom{@RefSecNum{function First_Line_Number}}
         New_Line;

      @key[when others] =>
         @key[null];
   @key[end case];

@key[end] Check_Short_Circuit;
@end{Example}

The Check_Library_Level_Task checks to see if the Element parameter is a
Declaration_Kind of A_Task_Type_Declaration, A_Protected_Type_Declaration,
A_Single_Task_Declaration, or A_Single_Protected_Declaration. If the Element is
such a declaration, then a test Is_Library_Level is performed. If the task is
not at the Library level, then a violation is reported along with its line
number.

@begin{Example}
@key[procedure] Check_Library_Level_Task (Elem : Asis.Element) @key[is]           -- @examcom{ @RefSecNum{type Element}}
@key[begin]
   @key[case] Asis.Elements.Declaration_Kind (Elem) @key[is]                      -- @examcom{@RefSecNum{function Declaration_Kind}}

      @key[when] Asis.A_Task_Type_Declaration |                             -- @examcom{ @RefSecNum{type Declaration_Kinds}}
            Asis.A_Protected_Type_Declaration |                       -- @examcom{ @RefSecNum{type Declaration_Kinds}}
            Asis.A_Single_Task_Declaration |                          -- @examcom{ @RefSecNum{type Declaration_Kinds}}
            Asis.A_Single_Protected_Declaration =>                    -- @examcom{ @RefSecNum{type Declaration_Kinds}}

          @key[if not] Is_Library_Level
             (Asis.Elements.Enclosing_Compilation_Unit(Elem)) @key[then]    -- @examcom{@RefSecNum{function Enclosing_Compilation_Unit}}
             Put_Line ("Violation of Tasking guideline:");
             Put ("-- @examcom{Non-Library Level Task at Line:");}
             Put (Asis.Text.Line_Number'Wide_Image                    -- @examcom{@RefSecNum{subtypes Line_Number and Line_Number_Positive}}
                 (Asis.Text.First_Line_Number (Elem)));               -- @examcom{@RefSecNum{function First_Line_Number}}
             New_Line;
          @key[end if];

       @key[when others] =>
          @key[null];
    @key[end case];
@key[end] Check_Library_Level_Task;
@end{Example}

The function Is_Library_Level returns true when the Unit_Class of the
Compilation_Unit is A_Public_Declaration.

@begin{Example}
@key[function] Is_Library_Level (CU : Asis.Compilation_Unit)               -- @examcom{ @RefSecNum{type Compilation_Unit}}
   @key[return] Boolean @key[is]
@key[begin]

   @key[case] Asis.Compilation_Units.Unit_Class (CU) @key[is]                    -- @examcom{@RefSecNum{function Unit_Class}}
      @key[when] Asis.A_Public_Declaration =>                              -- @examcom{ @RefSecNum{type Unit_Classes}}
          @key[return] True;
      @key[when others] =>
          @key[return] False;
   @key[end case];

@key[end] Is_Library_Level;
@end{Example}

So far the procedure Process_Element has been created to check restrictions on
an Element in a Compilation_Unit. The next step is to traverse all the Elements
in a Compilation_Unit with this check. The following package, called
Check_Compilation_Unit, does this with its procedure Find_Violations.
Find_Violations prints the name of the Unit_Kind and name of each
Compilation_Unit as it checks each element in the Compilation_Unit for
restrictions using the procedure Check. Procedure Check is an instantiation of
ASIS's Traverse_Element with Process_Element, containing the restriction
checks. Traverse_Element provides a traversal of each element in a
Compilation_Unit's logical syntax tree. The generic is instantiated with a
state, a pre-operation, and a post-operation. In this example, Process_Element
is the pre-operation which is executed when we land on each Element in the
logical syntax tree. In this example, no processing is needed as we leave the
Element, so the third generic parameter is the procedure No_Op. The state is
not needed by the application. The package Check_Compilation_Unit provides the
procedure Find_Violations for the selected Compilation_Unit. It traverses the
logical syntax tree, finding and reporting the short circuit violations and
task library level violations.

@begin{Example}
@key[with] Asis;
@key[package] Check_Compilation_Unit @key[is]

   @key[procedure] Find_Violations (CU : @key[in] Asis.Compilation_Unit);         -- @examcom{ @RefSecNum{type Compilation_Unit}}

@key[end] Check_Compilation_Unit;

@key[with] Asis; @key[with] Asis.Elements; @key[with] Asis.Iterator; @key[with] Asis.Text;
@key[with] Ada.Wide_Text_Io; @key[use] Ada.Wide_Text_Io;
@key[package body] Check_Compilation_Unit @key[is]

   @key[procedure] Process_Element (Elem    : @key[in] Asis.Element;              -- @examcom{ @RefSecNum{type Element}}
                              Control : @key[in out] Asis.Traverse_Control; -- @examcom{ @RefSecNum{type Traverse_Control}}
                              Dummy   : @key[in out] Boolean);

   @key[procedure] No_Op (Elem    : @key[in] Asis.Element;                        -- @examcom{ @RefSecNum{type Element}}
                    Control : @key[in out] Asis.Traverse_Control;           -- @examcom{ @RefSecNum{type Traverse_Control}}
                    Dummy   : @key[in out] Boolean);

   @key[procedure] Check @key[is new] Asis.Iterator.Traverse_Element              -- @examcom{@RefSecNum{procedure Traverse_Element}}
                              (Boolean, Process_Element, No_Op);

   @key[procedure] Find_Violations (CU : Asis.Compilation_Unit) @key[is]          -- @examcom{ @RefSecNum{type Compilation_Unit}}
      Control : Asis.Traverse_Control := Asis.Continue;               -- @examcom{ @RefSecNum{type Traverse_Control}}
      Dummy   : Boolean;
   @key[begin]
      Put_Line ("Processing " &
         Asis.Unit_Kinds'Wide_Image                                   -- @examcom{ @RefSecNum{type Unit_Kinds}}
            (Asis.Compilation_Units.Unit_Kind (CU))                   -- @examcom{@RefSecNum{function Unit_Kind}}
         & ": " &  (Asis.Compilation_Units.Unit_Full_Name (CU)));     -- @examcom{@RefSecNum{function Unit_Full_Name}}
      Check (Asis.Elements.Unit_Declaration (CU), Control, Dummy);
   @key[end] Check_Compilation_Unit;
@end{Example}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
The ASIS application is almost complete. A main
@Chg{Version=[2],New=[subprogram],Old=[program]} is needed @Chg{Version=[2],New=[that],Old=[which]}
contains the required sequencing of calls to initialize the ASIS interface,
name the Ada environment, access the Ada environment, loop through all
Compilation_Units in the ASIS Context with the Find_Violations procedure, and
@Chg{Version=[2],New=[to close/release],Old=[closing/releasing]} all ASIS
resources. The Compilation_Units in the Context are placed into the Unit_List.
This is achieved with the following main program, called My_Application.

@begin{Example}
@key[with] Asis;                                                             -- @examcom{ @RefSecNum{package Asis}}
@key[with] Asis.Implementation;                                              -- @examcom{ @RefSecNum{package Asis.Implementation}}
@key[with] Asis.Ada_Environments;                                            -- @examcom{ @RefSecNum{package Asis.Ada_Environments}}
@key[with] Asis.Compilation_Units;                                           -- @examcom{@RefSecNum{package Asis.Compilation_Units}}
@key[with] Check_Compilation_Unit;

@key[procedure] My_Application @key[is]
   My_Context : Asis.Context;                                          -- @examcom{ @RefSecNum{type Context}}

@key[begin]
   Asis.Implementation.Initialize;                                     -- @examcom{ @RefSecNum{procedure Initialize}}
   Asis.Ada_Environments.Associate (My_Context, "My Context");         -- @examcom{ @RefSecNum{procedure Associate}}
   Asis.Ada_Environments.Open (My_Context);                            -- @examcom{ @RefSecNum{procedure Open}}

   @key[declare]
      Unit_List :  Asis.Compilation_Unit_List :=                       -- @examcom{ @RefSecNum{type Compilation_Unit_List}}
         Asis.Compilation_Units.Compilation_Units (My_Context);        -- @examcom{@RefSecNum{function Compilation_Units (context)}}
   @key[begin]
      @key[for] I @key[in] Unit_List'Range @key[loop]
         @key[case] Asis.Compilation_Units.Unit_Origin (Unit_List (I)) @key[is]    -- @examcom{@RefSecNum{function Unit_Origin}}
            @key[when] Asis.An_Application_Unit =>                           -- @examcom{ @RefSecNum{type Unit_Origins}}
               Check_Compilation_Unit.Find_Violations (Unit_List (I));
            @key[when others] => @key[null];
         @key[end case];
      @key[end loop];
   @key[end];

   Asis.Ada_Environments.Close (My_Context);                           -- @examcom{ @RefSecNum{procedure Close}}
   Asis.Ada_Environments.Dissociate (My_Context);                      -- @examcom{ @RefSecNum{procedure Dissociate}}
   Asis.Implementation.Finalize;                                       -- @examcom{ @RefSecNum{procedure Finalize}}
@key[end] My_Application;
@end{Example}


@LabeledSubClause{Erroneous applications}

@leading@;An ASIS application is @i{erroneous} if:@Defn2{Term=[Erroneous],Sec=[ASIS Application]}
@begin{Itemize}
It uses any ASIS query, other than those exported by Asis.Implementation, while
Asis.Implementation.Is_Initialized = False.

It attempts to use a Context before opening it (exceptions to this are
Asis.Ada_Environments: Associate, Dissociate, and the Context query functions;
these are the only ones that shall be used before opening a Context).

It attempts to use a Context after closing it (exceptions to this are
Asis.Ada_Environments: Associate, Dissociate, and the Context query functions;
these are the only ones that shall be used after closing a Context).

It attempts to Dissociate or Associate an open Context.

It attempts to manipulate a Compilation_Unit whose Context has been closed.

It attempts to obtain Compilation_Unit information from an unopened Context.

It attempts to manipulate an Element from a Closed Context.
@end{Itemize}


@LabeledSubClause{Usage rules}


@LabeledSubSubClause{General usage rules}

The following are general usage rules:
@begin{Itemize}
All queries returning list values always return lists with a 'First of one (1).

All queries with a Context parameter of mode IN raise
ASIS_Inappropriate_Context if the Context value is not open. The Status is set
to Value_Error.@Defn{ASIS_Inappropriate_Context}

All queries with an Element or Line parameter attempt to detect the use of
invalid values and raise ASIS_Inappropriate_Element in response. The Status is
set to Value_Error. (An invalid value is one where the associated Context
variable has been closed.) Not all ASIS implementations are able to detect the
use of invalid values. An application that depends upon the success/failure of
this invalid value detection is not portable.@Defn{ASIS_Inappropriate_Element}

All queries other than simple Boolean or enumeration value queries, raise
ASIS_Inappropriate_Element if passed an Element that is not appropriate to the
query. The commentary for each query indicates the appropriate element
kinds.@Defn{ASIS_Inappropriate_Element}

It is generally inappropriate to mix elements of distinct subtypes (e.g.,
Passing a statement to a query expecting a declaration is inappropriate). It is
also inappropriate to mix kinds of elements within a subtype when a query is
expecting a specific kind. (i.e., Passing a type declaration to a query
expecting a procedure declaration is inappropriate).

Any query may raise ASIS_Failed with a Status of Obsolete_Reference_Error if
the argument or result is itself, or is part of, an inconsistent compilation
unit.@Defn{Obsolete_Reference_Error}

Any Asis.Text query may raise ASIS_Failed with a Status of Text_Error if the
program text cannot be located or retrieved for any reason such as renaming,
deletion, corruption, or moving of the text or Ada environment.@Defn{ASIS_Failed}
@end{Itemize}

@LabeledSubSubClause{Rules for processing queries for illegal/inconsistent context}

@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
The following provides @Chg{Version=[2],New=[],Old=[some ]}general rules to
identify processing when some inconsistent subset of Compilation Units is
processed during semantic query processing. Processing of inconsistent unit sets
is @Chg{Version=[2],New=[to],Old=[in]} a certain extent
implementation-dependent; @Chg{Version=[2],New=[nevertheless, ],Old=[]}the
following @Chg{Version=[2],New=[rules],Old=[in general should]} apply:

@leading@b{Semantic queries about elements}:

Semantic queries across compilation boundaries may raise an exception if the
units are inconsistent.

@leading@b{Semantic queries about compilation units}:
@begin{Itemize}
@b{Corresponding_Children}: If the declaration of a child is inconsistent with the
argument of the query, neither declaration nor body is returned. If the
declaration of a child is consistent with the argument, but the body is not,
the declaration is returned, and for the body, the result of the
Corresponding_Body query applied to the declaration is returned.

@b{Corresponding_Parent_Declaration}: If a parent is inconsistent with a child
passed as the argument, A_Nonexistent_Declaration shall be
returned.@Defn{A_Nonexistent_Declaration}

@b{Corresponding_Declaration}: If the declaration of an argument Element is
inconsistent with the argument, A_Nonexistent_Declaration shall be returned.
(For a unit A_Procedure_Body or A_Function_Body kind the solution may be in any
case to return Nil_Compilation_Unit if the unit is of
A_Public_Declaration_And_Body kind).

@b{Corresponding_Body}: If the argument Element requires a body to be presented to
make up a complete partition containing this Element, but the Context does not
contain the corresponding body, or the body contained in the Context is
inconsistent with the argument Element, A_Nonexistent_Body shall be
returned.@Defn{A_Nonexistent_Body}

@b{Subunits}: If a subunit is absent or if it is inconsistent with the argument
Element, A_Nonexistent_Body shall be returned for it.@Defn{A_Nonexistent_Body}

@b{Corresponding_Subunit_Parent_Body}: If the corresponding body does not exist in
the Context, or if it exists, but is inconsistent with the argument Element,
then A_Nonexistent_Body shall be returned.@Defn{A_Nonexistent_Body}

@end{Itemize}

@leading@b{Semantic queries about semantic dependence order}:

The Semantic_Dependence_Order query should never raise an exception when
processing inconsistent unit (sub)sets. This query is the only means for an
application to know if a given unit is consistent with (some of) its supporters
(dependents), and therefore the related semantic processing can give valuable
results for this unit.

@ChgDeleted{Version=[1],Text=[The remaining clauses in this International
Standard are presented as a compilable interface, documented with Ada
comments.]}

@LabeledAddedSubSubClause{Version=[2],Name=[Processing instantiations]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}@ChgNote{Moved from section 15}
@ChgAdded{Version=[2],Text=[Instantiations can always be analyzed in terms of
the generic actual parameters supplied with the instantiation. A generic
instance is a copy of the generic unit, and while there is no explicit (textual)
specification in the program text, an implicit specification and body, if there
is one, with the generic actual parameters is implied.]}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[To analyze the implicit instance
specification or body of a generic instantiation:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Use Corresponding_Declaration to return the
implicit expanded specification of an instantiation.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Use Corresponding_Body to return the implicit body
of an instantiation.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Then analyze the specification or body with any
appropriate queries.]}
@end{Itemize}

@ChgRef{Version=[2],Kind=[Added],ARef=[SI99-0037-1]}
@ChgAdded{Version=[2],Type=[Leading],Text=[To analyze the explicit generic
specification or body referenced by a generic instantiation:]}
@begin{Itemize}
@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Use Generic_Unit_Name to obtain the name of the generic unit.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Then use Corresponding_Name_Declaration to get to the generic declaration.]}

@ChgRef{Version=[2],Kind=[Added]}
@ChgAdded{Version=[2],Text=[Then use Corresponding_Body to get to the body of the generic declaration.]}
@end{Itemize}
