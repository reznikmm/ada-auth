@Part(examples, Root="asis.msm")

@Comment{$Date: 2010/03/12 06:02:39 $}

@comment{$Source: e:\\cvsroot/ARM/ASIS/examples.mss,v $}
@comment{$Revision: 1.6 $}

@LabeledInformativeAnnex{ASIS application examples}

@Chgnote{Improved title - SI-47}
@LabeledRevisedClause{Version=[2],New=[An ASIS application to traverse a compilation unit],Old=[Use to traverse compilation unit]}

The following example of an ASIS tool prompts the user for the name of
an Ada package specification, traverses that compilation unit, and prints
all explicit declarations with their kind.@Chg{Version=[1],New=[],Old=[ ASIS Application Example:]}

@begin{Example}
@key[with] Asis;                                                           -- @examcom{ @RefSecNum{package Asis}}
@key[with] Asis.Errors;                                                    -- @examcom{ @RefSecNum{package Asis.Errors}}
@key[with] Asis.Exceptions;                                                -- @examcom{ @RefSecNum{package Asis.Exceptions}}
@key[with] Asis.Implementation;                                            -- @examcom{ @RefSecNum{package Asis.Implementation}}
@key[with] Asis.Ada_Environments;                                          -- @examcom{ @RefSecNum{package Asis.Ada_Environments}}
@key[with] Asis.Compilation_Units;                                         -- @examcom{@RefSecNum{package Asis.Compilation_Units}}
@key[with] Asis.Elements;                                                  -- @examcom{@RefSecNum{package Asis.Elements}}
@key[with] Asis.Iterator;                                                  -- @examcom{@RefSecNum{package Asis.Iterator}}
@key[with] Asis.Declarations;                                              -- @examcom{@RefSecNum{package Asis.Declarations}}
@key[with] Ada.Wide_Text_Io; @key[use] Ada.Wide_Text_Io;

@key[procedure] ASIS_Application_Example @key[is]

   My_Context                 : Asis.Context;                        -- @examcom{ @RefSecNum{type Context}}
   My_Unit                    : Asis.Compilation_Unit;               -- @examcom{ @RefSecNum{type Compilation_Unit}}
   Unit_Name                  : Wide_String ( 1 .. 100 );
   Unit_Name_Length           : Natural;

   @key[procedure] Report_Declarations (Unit : @key[in] Asis.Compilation_Unit) @key[is] -- @examcom{@RefSecNum{type Compilation_Unit}}

      My_Element              : Asis.Element;                        -- @examcom{ @RefSecNum{type Element}}
      My_Control              : Asis.Traverse_Control                -- @examcom{ @RefSecNum{type Traverse_Control}}
                                  := Asis.Continue;
      My_State                : Boolean := True;

      @key[procedure] Process_Element
                   (Elem      : @key[in]     Asis.Element;                 -- @examcom{ @RefSecNum{type Element}}
                    Control   : @key[in out] Asis.Traverse_Control;        -- @examcom{ @RefSecNum{type Traverse_Control}}
                    State     : @key[in out] Boolean);

      @key[procedure] No_Op
                   (Elem      : @key[in]     Asis.Element;                 -- @examcom{ @RefSecNum{type Element}}
                    Control   : @key[in out] Asis.Traverse_Control;        -- @examcom{ @RefSecNum{type Traverse_Control}}
                    State     : @key[in out] Boolean);

      @key[procedure] Find_and_Print_Declarations @key[is new]
                    Asis.Iterator.Traverse_Element                   -- @examcom{@RefSecNum{procedure Traverse_Element}}
                       (Boolean, Process_Element, No_Op);

      @key[procedure] No_Op
                   (Elem      : @key[in]     Asis.Element;                 -- @examcom{ @RefSecNum{type Element}}
                    Control   : @key[in out] Asis.Traverse_Control;        -- @examcom{ @RefSecNum{type Traverse_Control}}
                    State     : @key[in out] Boolean) @key[is]
      @key[begin]
         @key[null];
      @key[end] No_Op;

      @key[procedure] Process_Element
                   (Elem      : @key[in]     Asis.Element;                 -- @examcom{ @RefSecNum{type Element}}
                    Control   : @key[in out] Asis.Traverse_Control;        -- @examcom{ @RefSecNum{type Traverse_Control}}
                    State     : @key[in out] Boolean) @key[is]

         @key[package] Kind_Io @key[is new] Ada.Wide_Text_Io.Enumeration_Io
                            (Asis.Declaration_Kinds);                -- @examcom{ @RefSecNum{type Declaration_Kinds}}

         Decl_Kind : Asis.Declaration_Kinds :=                       -- @examcom{ @RefSecNum{type Declaration_Kinds}}
             Asis.Elements.Declaration_Kind (Elem);                  -- @examcom{@RefSecNum{function Declaration_Kind}}

      @key[begin] -- @examcom{Process_Element}

         @key[case] Decl_Kind @key[is]

            @key[when] Asis.Not_A_Declaration => @key[null];                     -- @examcom{ @RefSecNum{type Declaration_Kinds}}

            @key[when others] =>

                @key[if not] Asis."="                                      -- @examcom{ @RefSecNum{type Declaration_Origins}}
                          (Asis.Elements.Declaration_Origin (Elem),  -- @examcom{@RefSecNum{function Declaration_Origin}}
                           Asis.An_Explicit_Declaration) @key[then]        -- @examcom{ @RefSecNum{type Declaration_Origins}}
                   @key[return];
                @key[end if];

                @key[declare]

                   Name_List : Asis.Defining_Name_List               -- @examcom{ @RefSecNum{subtypes of Element and Element_List}}
                          := Asis.Declarations.Names (Elem);         -- @examcom{@RefSecNum{function Names}}

                @key[begin]
                   @key[for] I @key[in] Name_List'Range @key[loop]
                       Put (Asis.Declarations.Defining_Name_Image
                           (Name_List (I)));                         -- @examcom{@RefSecNum{function Defining_Name_Image}}
                       Put (" (is kind) ");
                       Kind_Io.Put (Decl_Kind);
                       New_Line;
                   @key[end loop];
                @key[end];

         @key[end case];

      @key[end] Process_Element;

   @key[begin] -- @examcom{Report_Declarations}
      My_Element   := Asis.Elements.Unit_Declaration (Unit);         -- @examcom{@RefSecNum{function Unit_Declaration}}
      Find_and_Print_Declarations (My_Element, My_Control, My_State);
   @key[end] Report_Declarations;

@key[begin] -- @examcom{ASIS_Application_Example}

   Asis.Implementation.Initialize;                                   -- @examcom{ @RefSecNum{procedure Initialize}}
   Asis.Ada_Environments.Associate(My_Context, "My Context");        -- @examcom{ @RefSecNum{procedure Associate}}
   Asis.Ada_Environments.Open (My_Context);                          -- @examcom{ @RefSecNum{procedure Open}}

   Put_Line ("Type the name of an Ada package specification");
   Get_Line (Unit_Name, Unit_Name_Length);

   My_Unit := Asis.Compilation_Units.Library_Unit_Declaration        -- @examcom{@RefSecNum{function Library_Unit_Declaration}}
                  ( Unit_Name ( 1 .. Unit_Name_Length), My_Context );

   @key[if] Asis.Compilation_Units.Is_Nil (My_Unit)                        -- @examcom{@RefSecNum{function Is_Nil (unit)}}
   @key[then]
      Put ("Context does not contain the requested unit: ");
      Put (Unit_Name ( 1 .. Unit_Name_Length));
      New_Line;
   @key[else]
      Put ("Context contains the requested unit: ");
      Put (Unit_Name ( 1 .. Unit_Name_Length));
      New_Line;
      Report_Declarations ( My_Unit );
      New_Line;
   @key[end if];

   Asis.Ada_Environments.Close (My_Context);                         -- @examcom{ @RefSecNum{procedure Close}}
   Asis.Ada_Environments.Dissociate (My_Context);                    -- @examcom{ @RefSecNum{procedure Dissociate}}
   Asis.Implementation.Finalize;                                     -- @examcom{ @RefSecNum{procedure Finalize}}


@key[exception]

   @key[when]   Asis.Exceptions.ASIS_Inappropriate_Context                 -- @examcom{ @RefSecNum{package Asis.Exceptions}}@Chg{Version=[2],New=[],Old=[
        | Asis.Exceptions.ASIS_Inappropriate_Container               -- @examcom{ @RefSecNum{package Asis.Exceptions}}]}
        | Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit        -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Inappropriate_Element                 -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Inappropriate_Line                    -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Inappropriate_Line_Number             -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Failed                                -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        =>

        Put (Asis.Implementation.Diagnosis);                         -- @examcom{ @RefSecNum{function Diagnosis}}
        New_Line;
        Put ("Status Value is ");
        Put (Asis.Errors.Error_Kinds'Wide_Image                      -- @examcom{ @RefSecNum{type Error_Kinds}}
                        (Asis.Implementation.Status));               -- @examcom{ @RefSecNum{function Status}}
        New_Line;

   @key[when others] =>

        Put_Line ("ASIS Application failed because of non-ASIS reasons");

@key[end] ASIS_Application_Example;
@end{Example}

@leading@;@Chg{Version=[1],New=[Sample input for the ],Old=[]}ASIS Application Example
@Chg{Version=[1],New=[is the ],Old=[using ]}following package specification named
asis_test:

@begin{Example}
@key[package] asis_test @key[is]

   @key[type] T @key[is] ( A, B, C);

   S : integer := T'BASE'SIZE;

@key[end] asis_test;
@end{Example}


@leading@;Result of executing ASIS Application Example:

@begin{Example}
Type the name of an Ada package specification
asis_test
Context contains the requested unit: asis_test
asis_test (is kind) A_PACKAGE_DECLARATION
T (is kind) AN_ORDINARY_TYPE_DECLARATION
A (is kind) AN_ENUMERATION_LITERAL_SPECIFICATION
B (is kind) AN_ENUMERATION_LITERAL_SPECIFICATION
C (is kind) AN_ENUMERATION_LITERAL_SPECIFICATION
S (is kind) A_VARIABLE_DECLARATION
@end{Example}


@Chgnote{Improved title - SI-47}
@LabeledRevisedClause{Version=[2],New=[An ASIS application to build a call tree],Old=[Use to build call tree]}

This example prints call tree information (i.e., a list of all procedure,
function, and entry calls made within a compilation unit) for each compilation
unit in the context. The output format is of the form:

@begin{Example}
<Calling_Compilation_Unit> (calls) <Called_Program_Unit> at line <Line_Number>
@end{Example}

where <Calling_Compilation_Unit> is the Expanded Name of the Unit making the
call; <Called_Program_Unit> is the name of the program unit being called; and
<Line_Number> is the first line number of the call in the source file.


@begin{Example}
@key[with] Asis;                                                             -- @examcom{ @RefSecNum{package Asis}}
@key[with] Asis.Errors;                                                      -- @examcom{ @RefSecNum{package Asis.Errors}}
@key[with] Asis.Exceptions;                                                  -- @examcom{ @RefSecNum{package Asis.Exceptions}}
@key[with] Asis.Implementation;                                              -- @examcom{ @RefSecNum{package Asis.Implementation}}
@key[with] Asis.Ada_Environments;                                            -- @examcom{ @RefSecNum{package Asis.Ada_Environments}}
@key[with] Asis.Compilation_Units;                                           -- @examcom{@RefSecNum{package Asis.Compilation_Units}}
@key[with] Asis.Elements;                                                    -- @examcom{@RefSecNum{package Asis.Elements}}
@key[with] Asis.Iterator;                                                    -- @examcom{@RefSecNum{package Asis.Iterator}}
@key[with] Asis.Declarations;                                                -- @examcom{@RefSecNum{package Asis.Declarations}}
@key[with] Asis.Expressions;                                                 -- @examcom{@RefSecNum{package Asis.Expressions}}
@key[with] Asis.Statements;                                                  -- @examcom{@RefSecNum{package Asis.Statements}}
@key[with] Asis.Text;                                                        -- @examcom{@RefSecNum{package Asis.Text}}

@key[with] Ada.Wide_Text_Io; @key[use] Ada.Wide_Text_Io;

@key[procedure] ASIS_Call_Tree_Example @key[is]

   My_Context              : Asis.Context;                             -- @examcom{ @RefSecNum{type Context}}

   @key[procedure] No_Op
                (Elem      : @key[in]     Asis.Element;                      -- @examcom{ @RefSecNum{type Element}}
                 Control   : @key[in out] Asis.Traverse_Control;             -- @examcom{ @RefSecNum{type Traverse_Control}}
                 State     : @key[in out] Boolean);

   @key[procedure] Report_Calls
                (An_Element: @key[in]     Asis.Element;                      -- @examcom{ @RefSecNum{type Element}}
                 Control   : @key[in out] Asis.Traverse_Control;             -- @examcom{ @RefSecNum{type Traverse_Control}}
                 State     : @key[in out] Boolean);

   @key[procedure] Print_Call_Tree @key[is new]
                 Asis.Iterator.Traverse_Element                        -- @examcom{@RefSecNum{procedure Traverse_Element}}
                    (Boolean, Report_Calls, No_Op);

   @key[procedure] No_Op
                (Elem      : @key[in]     Asis.Element;                      -- @examcom{ @RefSecNum{type Element}}
                 Control   : @key[in out] Asis.Traverse_Control;             -- @examcom{ @RefSecNum{type Traverse_Control}}
                 State     : @key[in out] Boolean) @key[is]
   @key[begin]
      @key[null];
   @key[end] No_Op;

   @key[procedure] Output_Call (Caller : Asis.Element;                       -- @examcom{ @RefSecNum{type Element}}
                          Callee : Asis.Declaration) @key[is]                -- @examcom{ @RefSecNum{subtypes of Element and Element_List}}

      Calling_Cu   : Asis.Compilation_Unit;                            -- @examcom{ @RefSecNum{type Compilation_Unit}}
      Calling_Unit : Asis.Declaration;                                 -- @examcom{ @RefSecNum{subtypes of Element and Element_List}}


   @key[begin] -- @examcom{Output_Call}

      Calling_Cu := Asis.Elements.Enclosing_Compilation_Unit (Caller); -- @examcom{@RefSecNum{function Enclosing_Compilation_Unit}}

      @key[if] Asis.Compilation_Units.Is_Nil (Calling_Cu) @key[then]               -- @examcom{@RefSecNum{function Is_Nil (unit)}}
         Put ("An_Unknown_Unit");
      @key[else]
         Put (Asis.Compilation_Units.Unit_Full_Name (Calling_Cu));     -- @examcom{@RefSecNum{function Unit_Full_Name}}
      @key[end] if;

      Put (" (calls) ");
      Put (Asis.Declarations.Defining_Name_Image                       -- @examcom{@RefSecNum{function Defining_Name_Image}}
                       (Asis.Declarations.Names (Callee) (1)));        -- @examcom{@RefSecNum{function Names}}
      Put (" at line ");
      Put (Asis.Text.Line_Number'Wide_Image                            -- @examcom{@RefSecNum{subtypes Line_Number and Line_Number_Positive}}
                    (Asis.Text.First_Line_Number (Caller)));           -- @examcom{@RefSecNum{function First_Line_Number}}
      New_Line;

   @key[end] Output_Call;

   @key[procedure] Report_Calls (An_Element : @key[in] Asis.Element;               -- @examcom{ @RefSecNum{type Element}}
                           Control    : @key[in out] Asis.Traverse_Control;  -- @examcom{ @RefSecNum{type Traverse_Control}}
                           State      : @key[in out] Boolean) @key[is]

      Callee : Asis.Declaration;                                       -- @examcom{ @RefSecNum{subtypes of Element and Element_List}}

   @key[begin] -- @examcom{Report_Calls}

      case Asis.Elements.Element_Kind (An_Element) @key[is]                  -- @examcom{@RefSecNum{function Element_Kind}}

         @key[when] Asis.An_Expression =>                                    -- @examcom{ @RefSecNum{type Element_Kinds}}
            @key[case] Asis.Elements.Expression_Kind (An_Element) @key[is]         -- @examcom{@RefSecNum{function Expression_Kind}}
               @key[when] Asis.A_Function_Call =>                            -- @examcom{ @RefSecNum{type Expression_Kinds}}

                  Callee := Asis.Expressions.Corresponding_Called_Function
                                     (An_Element);                     -- @examcom{@RefSecNum{function Corresponding_Called_Function}}

                  @key[if not] Asis.Elements.Is_Nil (Callee) @key[then]            -- @examcom{@RefSecNum{function Is_Nil (element)}}
                     Output_Call (An_Element, Callee);
                  @key[end if];

               @key[when others] =>
                  @key[null];
            @key[end case];

         @key[when] Asis.A_Statement =>                                      -- @examcom{ @RefSecNum{type Element_Kinds}}
            case Asis.Elements.Statement_Kind (An_Element) @key[is]          -- @examcom{@RefSecNum{function Statement_Kind}}

               @key[when] Asis.A_Procedure_Call_Statement |                  -- @examcom{ @RefSecNum{type Statement_Kinds}}
                    Asis.An_Entry_Call_Statement =>                    -- @examcom{ @RefSecNum{type Statement_Kinds}}

                  Callee := Asis.Statements.Corresponding_Called_Entity
                                 (An_Element);                         -- @examcom{@RefSecNum{function Corresponding_Called_Entity}}

                  @key[if not] Asis.Elements.Is_Nil (Callee) @key[then]            -- @examcom{@RefSecNum{function Is_Nil (element)}}
                     Output_Call (An_Element, Callee);
                  @key[end if];

               @key[when others] =>
                  @key[null];
            @key[end case];

         @key[when others] =>
            @key[null];

      @key[end case];

   @key[end] Report_Calls;

   @key[procedure] Process_Units (Unit_List : @key[in] Asis.Compilation_Unit_List) @key[is] -- @examcom{@RefSecNum{type Compilation_Unit_List}}

      Control : Asis.Traverse_Control := Asis.Continue;                -- @examcom{ @RefSecNum{type Traverse_Control}}
      State   : Boolean := True;

   @key[begin]

      @key[for] I @key[in] Unit_List'Range @key[loop]

         case Asis.Compilation_Units.Unit_Origin (Unit_List (I)) @key[is]    -- @examcom{@RefSecNum{function Unit_Origin}}
            @key[when] Asis.An_Application_Unit =>                           -- @examcom{ @RefSecNum{type Unit_Origins}}
               New_Line;
               Put_Line ("Processing Unit: " &
                                 Asis.Compilation_Units.Unit_Full_Name
                                    (Unit_List (I)));                  -- @examcom{@RefSecNum{function Unit_Full_Name}}
               Print_Call_Tree (Asis.Elements.Unit_Declaration         -- @examcom{@RefSecNum{function Unit_Declaration}}
                                    (Unit_List (I)), Control, State);
            @key[when others] =>
               @key[null];
         @key[end case];

      @key[end loop];

   @key[end] Process_Units;

@key[begin] -- @examcom{ASIS_Call_Tree_Example}

   Asis.Implementation.Initialize;                                     -- @examcom{ @RefSecNum{procedure Initialize}}
   Asis.Ada_Environments.Associate(My_Context, "My Context");          -- @examcom{ @RefSecNum{procedure Associate}}
   Asis.Ada_Environments.Open (My_Context);                            -- @examcom{ @RefSecNum{procedure Open}}

   Process_Units (Asis.Compilation_Units.Compilation_Units (My_Context)); -- @examcom{@RefSecNum{function Compilation_Units (context)}}

   Asis.Ada_Environments.Close (My_Context);                           -- @examcom{ @RefSecNum{procedure Close}}
   Asis.Ada_Environments.Dissociate (My_Context);                      -- @examcom{ @RefSecNum{procedure Dissociate}}
   Asis.Implementation.Finalize;                                       -- @examcom{ @RefSecNum{procedure Finalize}}

@key[exception]

   @key[when] Asis.Exceptions.ASIS_Inappropriate_Context                     -- @examcom{ @RefSecNum{package Asis.Exceptions}}@Chg{Version=[2],New=[],Old=[
        | Asis.Exceptions.ASIS_Inappropriate_Container                 -- @examcom{ @RefSecNum{package Asis.Exceptions}}]}
        | Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit          -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Inappropriate_Element                   -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Inappropriate_Line                      -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Inappropriate_Line_Number               -- @examcom{ @RefSecNum{package Asis.Exceptions}}
        | Asis.Exceptions.ASIS_Failed                                  -- @examcom{ @RefSecNum{package Asis.Exceptions}}
      =>

      Put (Asis.Implementation.Diagnosis);                             -- @examcom{ @RefSecNum{function Diagnosis}}
      New_Line;
      Put ("Status Value is ");
      Put (Asis.Errors.Error_Kinds'Wide_Image                          -- @examcom{ @RefSecNum{type Error_Kinds}}
                      (Asis.Implementation.Status));                   -- @examcom{ @RefSecNum{function Status}}
      New_Line;

   @key[when others] =>

      Put_Line ("Asis Application failed because of non-ASIS reasons");

@key[end] ASIS_Call_Tree_Example;
@end{Example}


@leading@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Consider the],Old=[The ASIS_Call_Tree_Example is
demonstrated using a]} context containing the
following compilation units:

@begin{Example}
@key[package] P @key[is]

   @key[procedure] P1;
   @key[procedure] P2;
   @key[procedure] P3(X : integer);
   @key[function] F1 @key[return] integer;

@key[end];

@key[package body] P @key[is]

   @key[procedure] P1 @key[is] @key[separate];
   @key[procedure] P2 @key[is] @key[separate];
   @key[procedure] P3(X : integer) @key[is] @key[separate];
   @key[function] F1 @key[return] integer @key[is] @key[separate];

@key[begin]
   P1;
@key[end];

@key[separate] (P)
@key[function] F1 @key[return] integer @key[is]
@key[begin]
   return 0;
@key[end];

@key[separate] (P)
@key[procedure] P1 @key[is]
   x : integer := F1;
@key[begin]
   P2;
   P3(x);
@key[end];

@key[separate] (P)
@key[procedure] P2 @key[is]
@key[begin]
   P3(F1);
@key[end];

@key[separate] (P)
@key[procedure] P3(X : integer) @key[is]
@key[begin]
   @key[null];
@key[end];
@end{Example}


@leading@ChgRef{Version=[2],Kind=[Revised],ARef=[SI99-0047-1]}
@Chg{Version=[2],New=[Applying],Old=[Given a context containing the
above set of compilation units, the sample
output resulting from the execution of]} ASIS_Call_Tree_Example
@Chg{Version=[2],New=[to the context given above yields the
following output],Old=[is]}:

@begin{Example}
Processing Unit: P

Processing Unit: P
P (calls) P1 at line  9

Processing Unit: P.F1

Processing Unit: P.P1
P.P1 (calls) F1 at line  3
P.P1 (calls) P2 at line  5
P.P1 (calls) P3 at line  6

Processing Unit: P.P2
P.P2 (calls) P3 at line  4
P.P2 (calls) F1 at line  4

Processing Unit: P.P3
@end{Example}

@ChgNote{From SI99-0060-1}
@LabeledAddedClause{Version=[2],Name=[An ASIS semantic subsystem application to display dispatching calls]}

@ChgRef{Version=[2],Kind=[AddedNormal],ARef=[SI99-0060-1]}
@ChgAdded{Version=[2],Text=[This example adds Dispatching_Call processing to the
previous example, via the ASIS semantic subsystem queries. Assuming that that
example is updated with an appropriate stub for and call to procedure
Process_Dispatching_Call; the following example will output additional
information relating to dispatching calls.]}

@begin{Example}
@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[with] Asis.Views;
@key[with] Asis.Expressions.Views;
@key[with] Asis.Callable_Views;
@key[with] Asis.Subtype_Views;
@key[with] Ada.Wide_Text_Io; @key[use] Ada.Wide_Text_Io;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[separate] (Check_Compilation_Unit)
@key[procedure] Process_Dispatching_Call (A_Call : @key[in] Asis.Element) @key[is]
   Call_Name : Asis.Name;
@key[begin]]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[if] Asis.Statements.Is_Dispatching_Call(A_Call) @key[then]                   -- @examcom{@refsecnum{function Is_Dispatching_Call}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      -- @examcom{Get the element representing the name of the call}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key[case] Asis.Elements.Element_Kind (A_Call) @key[is]
         @key[when] Asis.An_Expression =>
                  Call_Name := Asis.Expressions.Prefix(A_Call);          -- @examcom{@refsecnum{function Prefix}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         @key[when] Asis.A_Statement =>
                  Call_Name:= Asis.Statements.Called_Name(A_Call);       -- @examcom{@refsecnum{function Called_Name}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         @key[when others] => @key[null];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key[end case];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key[declare]
         -- @examcom{Create the callable_view from the call_name}
         Call_View : @key[constant] Asis.Callable_Views.Callable_View'Class
            := Asis.Callable_Views.Callable_View
               (Asis.Expressions.Views.Corresponding_View (Call_Name));  -- @examcom{@refsecnum{function Corresponding_View}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         -- @examcom{Via a semantic query, determine the controlling type}
         Controlling_Type : @key[constant] Asis.Subtype_Views.Subtype_View'Class
            := Call_View.Associated_Tagged_Type;                         -- @examcom{@refsecnum{Primitive operations}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         -- @examcom{Determine the declaration of the callee and the controlling type}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         Callee_Decl : @key[constant] Asis.Views.Declarative_Regions.View_Declaration'Class
            := Call_View.Declaration;                                    -- @examcom{@refsecnum{Views and Declarations}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         Type_Decl : @key[constant] Asis.Views.Declarative_Regions.View_Declaration'Class
            := Controlling_Type.Declaration;                             -- @examcom{@refsecnum{Views and Declarations}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         -- @examcom{Get the Asis Element representing the declaration of the callee}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         Callee_Declaration_Element : Asis.Declaration :=
             Callee_Decl.Declaration;                                    -- @examcom{@refsecnum{function Element_Denoting_View}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         -- @examcom{Get the Asis Element representing the declaration of the controlling}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         Controlling_Type_Declaration_Element : Asis.Declaration :=
             Type_Decl.Declaration;                                      -- @examcom{@refsecnum{function Element_Denoting_View}}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         -- @examcom{Output the dispatching call info}]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      begin]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[         Put ("Dispatching Call to ");
         Put ( Callee_Decl.Expanded_Name );                              -- @examcom{@refsecnum{Nested Declarative Regions}}
         Put ( Asis.Views.Declarative_Regions.Expanded_Name(Callee_Decl));
         Put (" with controlling tagged type of ");
         Put_Line ( Asis.Views.Declarative_Regions.Expanded_Name(Type_Decl));]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[      @key[end];]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[   @key[end] if;]}

@ChgRef{Version=[2],Kind=[AddedNormal]}
@ChgAdded{Version=[2],Text=[@key[end] Process_Dispatching_Call;]}


@end{Example}

