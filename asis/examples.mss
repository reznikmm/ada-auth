@Part(examples, Root="asis.msm")

@Comment{$Date: 2006/09/28 05:11:59 $}

@comment{$Source: e:\\cvsroot/ARM/ASIS/examples.mss,v $}
@comment{$Revision: 1.1 $}

@LabeledInformativeAnnex{ASIS application examples}

@LabeledClause{Use to traverse compilation unit}

The following example of an ASIS tool prompts the user for the name of
an Ada package specification, traverses that compilation unit, and prints
all explicit declarations with their kind.@Chg{Version=[1],New=[],Old=[ ASIS Application Example:]}

@begin{Example}
@key[with] Asis;                                                           --  @RefSecNum{package Asis}
@key[with] Asis.Errors;                                                    --  @RefSecNum{package Asis.Errors}
@key[with] Asis.Exceptions;                                                --  @RefSecNum{package Asis.Exceptions}
@key[with] Asis.Implementation;                                            --  @RefSecNum{package Asis.Implementation}
@key[with] Asis.Ada_Environments;                                          --  @RefSecNum{package Asis.Ada_Environments}
@key[with] Asis.Compilation_Units;                                         -- @RefSecNum{package Asis.Compilation_Units}
@key[with] Asis.Elements;                                                  -- @RefSecNum{package Asis.Elements}
@key[with] Asis.Iterator;                                                  -- @RefSecNum{package Asis.Iterator}
@key[with] Asis.Declarations;                                              -- @RefSecNum{package Asis.Declarations}
@key[with] Ada.Wide_Text_Io; @key[use] Ada.Wide_Text_Io;

@key[procedure] ASIS_Application_Example @key[is]

   My_Context                 : Asis.Context;                        --  @RefSecNum{type Context}
   My_Unit                    : Asis.Compilation_Unit;               --  @RefSecNum{type Compilation_Unit}
   Unit_Name                  : Wide_String ( 1 .. 100 );
   Unit_Name_Length           : Natural;

   @key[procedure] Report_Declarations (Unit : @key[in] Asis.Compilation_Unit) @key[is] -- @RefSecNum{type Compilation_Unit}

      My_Element              : Asis.Element;                        --  @RefSecNum{type Element}
      My_Control              : Asis.Traverse_Control                --  @RefSecNum{type Traverse_Control}
                                  := Asis.Continue;
      My_State                : Boolean := True;

      @key[procedure] Process_Element
                   (Elem      : @key[in]     Asis.Element;                 --  @RefSecNum{type Element}
                    Control   : @key[in out] Asis.Traverse_Control;        --  @RefSecNum{type Traverse_Control}
                    State     : @key[in out] Boolean);

      @key[procedure] No_Op
                   (Elem      : @key[in]     Asis.Element;                 --  @RefSecNum{type Element}
                    Control   : @key[in out] Asis.Traverse_Control;        --  @RefSecNum{type Traverse_Control}
                    State     : @key[in out] Boolean);

      @key[procedure] Find_and_Print_Declarations @key[is new]
                    Asis.Iterator.Traverse_Element                   -- @RefSecNum{procedure Traverse_Element}
                       (Boolean, Process_Element, No_Op);

      @key[procedure] No_Op
                   (Elem      : @key[in]     Asis.Element;                 --  @RefSecNum{type Element}
                    Control   : @key[in out] Asis.Traverse_Control;        --  @RefSecNum{type Traverse_Control}
                    State     : @key[in out] Boolean) @key[is]
      @key[begin]
         @key[null];
      @key[end] No_Op;

      @key[procedure] Process_Element
                   (Elem      : @key[in]     Asis.Element;                 --  @RefSecNum{type Element}
                    Control   : @key[in out] Asis.Traverse_Control;        --  @RefSecNum{type Traverse_Control}
                    State     : @key[in out] Boolean) @key[is]

         @key[package] Kind_Io @key[is new] Ada.Wide_Text_Io.Enumeration_Io
                            (Asis.Declaration_Kinds);                --  @RefSecNum{type Declaration_Kinds}

         Decl_Kind : Asis.Declaration_Kinds :=                       --  @RefSecNum{type Declaration_Kinds}
             Asis.Elements.Declaration_Kind (Elem);                  -- @RefSecNum{function Declaration_Kind}

      @key[begin] -- Process_Element

         @key[case] Decl_Kind @key[is]

            @key[when] Asis.Not_A_Declaration => @key[null];                     --  @RefSecNum{type Declaration_Kinds}

            @key[when others] =>

                @key[if not] Asis."="                                      --  @RefSecNum{type Declaration_Origins}
                          (Asis.Elements.Declaration_Origin (Elem),  -- @RefSecNum{function Declaration_Origin}
                           Asis.An_Explicit_Declaration) @key[then]        --  @RefSecNum{type Declaration_Origins}
                   @key[return];
                @key[end if];

                @key[declare]

                   Name_List : Asis.Defining_Name_List               --  @RefSecNum{subtypes of Element and Element_List}
                          := Asis.Declarations.Names (Elem);         -- @RefSecNum{function Names}

                @key[begin]
                   @key[for] I @key[in] Name_List'Range @key[loop]
                       Put (Asis.Declarations.Defining_Name_Image
                           (Name_List (I)));                         -- @RefSecNum{function Defining_Name_Image}
                       Put (" (is kind) ");
                       Kind_Io.Put (Decl_Kind);
                       New_Line;
                   @key[end loop];
                @key[end];

         @key[end case];

      @key[end] Process_Element;

   @key[begin] -- Report_Declarations
      My_Element   := Asis.Elements.Unit_Declaration ( Unit );       -- @RefSecNum{function Unit_Declaration}
      Find_and_Print_Declarations (My_Element, My_Control, My_State);
   @key[end] Report_Declarations;

@key[begin] -- ASIS_Application_Example

   Asis.Implementation.Initialize;                                   --  @RefSecNum{procedure Initialize}
   Asis.Ada_Environments.Associate(My_Context, "My Context");        --  @RefSecNum{procedure Associate}
   Asis.Ada_Environments.Open (My_Context);                          --  @RefSecNum{procedure Open}

   Put_Line ("Type the name of an Ada package specification");
   Get_Line (Unit_Name, Unit_Name_Length);

   My_Unit := Asis.Compilation_Units.Library_Unit_Declaration        -- @RefSecNum{function Library_Unit_Declaration}
                  ( Unit_Name ( 1 .. Unit_Name_Length), My_Context );

   @key[if] Asis.Compilation_Units.Is_Nil (My_Unit)                        -- @RefSecNum{function Is_Nil (unit)}
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

   Asis.Ada_Environments.Close (My_Context);                         --  @RefSecNum{procedure Close}
   Asis.Ada_Environments.Dissociate (My_Context);                    --  @RefSecNum{procedure Dissociate}
   Asis.Implementation.Finalize;                                     --  @RefSecNum{procedure Finalize}


@key[exception]

   @key[when]   Asis.Exceptions.ASIS_Inappropriate_Context                 --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Container               --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit        --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Element                 --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Line                    --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Line_Number             --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Failed                                --  @RefSecNum{package Asis.Exceptions}
        =>

        Put (Asis.Implementation.Diagnosis);                         --  @RefSecNum{function Diagnosis}
        New_Line;
        Put ("Status Value is ");
        Put (Asis.Errors.Error_Kinds'Wide_Image                      --  @RefSecNum{type Error_Kinds}
                        (Asis.Implementation.Status));               --  @RefSecNum{function Status}
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


@LabeledClause{Use to build call tree}

This example prints call tree information (i.e., a list of all procedure,
function, and entry calls made within a compilation unit) for each compilation
unit in the context. The output format is of the form:

<Calling_Compilation_Unit> (calls) <Called_Program_Unit> at line <Line_Number>
where:

<Calling_Compilation_Unit>  is the Expanded Name of the Unit making the call
<Called_Program_Unit>       is the name of the program unit being called
<Line_Number>               is the first line number of the call in the source file


@begin{Example}
@key[with] Asis;                                                             --  @RefSecNum{package Asis}
@key[with] Asis.Errors;                                                      --  @RefSecNum{package Asis.Errors}
@key[with] Asis.Exceptions;                                                  --  @RefSecNum{package Asis.Exceptions}
@key[with] Asis.Implementation;                                              --  @RefSecNum{package Asis.Implementation}
@key[with] Asis.Ada_Environments;                                            --  @RefSecNum{package Asis.Ada_Environments}
@key[with] Asis.Compilation_Units;                                           -- @RefSecNum{package Asis.Compilation_Units}
@key[with] Asis.Elements;                                                    -- @RefSecNum{package Asis.Elements}
@key[with] Asis.Iterator;                                                    -- @RefSecNum{package Asis.Iterator}
@key[with] Asis.Declarations;                                                -- @RefSecNum{package Asis.Declarations}
@key[with] Asis.Expressions;                                                 -- @RefSecNum{package Asis.Expressions}
@key[with] Asis.Statements;                                                  -- @RefSecNum{package Asis.Statements}
@key[with] Asis.Text;                                                        -- @RefSecNum{package Asis.Text}

@key[with] Ada.Wide_Text_Io; @key[use] Ada.Wide_Text_Io;

@key[procedure] ASIS_Call_Tree_Example @key[is]

   My_Context              : Asis.Context;                             --  @RefSecNum{type Context}

   @key[procedure] No_Op
                (Elem      : @key[in]     Asis.Element;                      --  @RefSecNum{type Element}
                 Control   : @key[in out] Asis.Traverse_Control;             --  @RefSecNum{type Traverse_Control}
                 State     : @key[in out] Boolean);

   @key[procedure] Report_Calls
                (An_Element: @key[in]     Asis.Element;                      --  @RefSecNum{type Element}
                 Control   : @key[in out] Asis.Traverse_Control;             --  @RefSecNum{type Traverse_Control}
                 State     : @key[in out] Boolean);

   @key[procedure] Print_Call_Tree @key[is new]
                 Asis.Iterator.Traverse_Element                        -- @RefSecNum{procedure Traverse_Element}
                    (Boolean, Report_Calls, No_Op);

   @key[procedure] No_Op
                (Elem      : @key[in]     Asis.Element;                      --  @RefSecNum{type Element}
                 Control   : @key[in out] Asis.Traverse_Control;             --  @RefSecNum{type Traverse_Control}
                 State     : @key[in out] Boolean) @key[is]
   @key[begin]
      @key[null];
   @key[end] No_Op;

   @key[procedure] Output_Call (Caller : Asis.Element;                       --  @RefSecNum{type Element}
                          Callee : Asis.Declaration) @key[is]                --  @RefSecNum{subtypes of Element and Element_List}

      Calling_Cu   : Asis.Compilation_Unit;                            --  @RefSecNum{type Compilation_Unit}
      Calling_Unit : Asis.Declaration;                                 --  @RefSecNum{subtypes of Element and Element_List}


   @key[begin] -- Output_Call

      Calling_Cu := Asis.Elements.Enclosing_Compilation_Unit (Caller); -- @RefSecNum{function Enclosing_Compilation_Unit}

      @key[if] Asis.Compilation_Units.Is_Nil (Calling_Cu) @key[then]               -- @RefSecNum{function Is_Nil (unit)}
         Put ("An_Unknown_Unit");
      @key[else]
         Put (Asis.Compilation_Units.Unit_Full_Name (Calling_Cu));     -- @RefSecNum{function Unit_Full_Name}
      @key[end] if;

      Put (" (calls) ");
      Put (Asis.Declarations.Defining_Name_Image                       -- @RefSecNum{function Defining_Name_Image}
                       (Asis.Declarations.Names (Callee) (1)));        -- @RefSecNum{function Names}
      Put (" at line ");
      Put (Asis.Text.Line_Number'Wide_Image                            -- @RefSecNum{type Line_Number}
                    (Asis.Text.First_Line_Number (Caller)));           -- @RefSecNum{function First_Line_Number}
      New_Line;

   @key[end] Output_Call;

   @key[procedure] Report_Calls (An_Element : @key[in] Asis.Element;               --  @RefSecNum{type Element}
                           Control    : @key[in out] Asis.Traverse_Control;  --  @RefSecNum{type Traverse_Control}
                           State      : @key[in out] Boolean) @key[is]

      Callee : Asis.Declaration;                                       --  @RefSecNum{subtypes of Element and Element_List}

   @key[begin] -- Report_Calls

      case Asis.Elements.Element_Kind (An_Element) @key[is]                  -- @RefSecNum{function Element_Kind}

         @key[when] Asis.An_Expression =>                                    --  @RefSecNum{type Element_Kinds}
            @key[case] Asis.Elements.Expression_Kind (An_Element) @key[is]         -- @RefSecNum{function Expression_Kind}
               @key[when] Asis.A_Function_Call =>                            --  @RefSecNum{type Expression_Kinds}

                  Callee := Asis.Expressions.Corresponding_Called_Function
                                     (An_Element);                     -- @RefSecNum{function Corresponding_Called_Function}

                  @key[if not] Asis.Elements.Is_Nil (Callee) @key[then]            -- @RefSecNum{function Is_Nil (element)}
                     Output_Call (An_Element, Callee);
                  @key[end if];

               @key[when others] =>
                  @key[null];
            @key[end case];

         @key[when] Asis.A_Statement =>                                      --  @RefSecNum{type Element_Kinds}
            case Asis.Elements.Statement_Kind (An_Element) @key[is]          -- @RefSecNum{function Statement_Kind}

               @key[when] Asis.A_Procedure_Call_Statement |                  --  @RefSecNum{type Statement_Kinds}
                    Asis.An_Entry_Call_Statement =>                    --  @RefSecNum{type Statement_Kinds}

                  Callee := Asis.Statements.Corresponding_Called_Entity
                                 (An_Element);                         -- @RefSecNum{function Corresponding_Called_Entity}

                  @key[if not] Asis.Elements.Is_Nil (Callee) @key[then]            -- @RefSecNum{function Is_Nil (element)}
                     Output_Call (An_Element, Callee);
                  @key[end if];

               @key[when others] =>
                  @key[null];
            @key[end case];

         @key[when others] =>
            @key[null];

      @key[end case];

   @key[end] Report_Calls;

   @key[procedure] Process_Units (Unit_List : @key[in] Asis.Compilation_Unit_List) @key[is] -- @RefSecNum{type Compilation_Unit_List}

      Control : Asis.Traverse_Control := Asis.Continue;                --  @RefSecNum{type Traverse_Control}
      State   : Boolean := True;

   @key[begin]

      @key[for] I @key[in] Unit_List'Range @key[loop]

         case Asis.Compilation_Units.Unit_Origin (Unit_List (I)) @key[is]    -- @RefSecNum{function Unit_Origin}
            @key[when] Asis.An_Application_Unit =>                           --  @RefSecNum{type Unit_Origins}
               New_Line;
               Put_Line ("Processing Unit: " &
                                 Asis.Compilation_Units.Unit_Full_Name
                                    (Unit_List (I)));                  -- @RefSecNum{function Unit_Full_Name}
               Print_Call_Tree (Asis.Elements.Unit_Declaration         -- @RefSecNum{function Unit_Declaration}
                                    (Unit_List (I)), Control, State);
            @key[when others] =>
               @key[null];
         @key[end case];

      @key[end loop];

   @key[end] Process_Units;

@key[begin] -- ASIS_Call_Tree_Example

   Asis.Implementation.Initialize;                                     --  @RefSecNum{procedure Initialize}
   Asis.Ada_Environments.Associate(My_Context, "My Context");          --  @RefSecNum{procedure Associate}
   Asis.Ada_Environments.Open (My_Context);                            --  @RefSecNum{procedure Open}

   Process_Units (Asis.Compilation_Units.Compilation_Units (My_Context)); -- @RefSecNum{function Compilation_Units (context)}

   Asis.Ada_Environments.Close (My_Context);                           --  @RefSecNum{procedure Close}
   Asis.Ada_Environments.Dissociate (My_Context);                      --  @RefSecNum{procedure Dissociate}
   Asis.Implementation.Finalize;                                       --  @RefSecNum{procedure Finalize}

@key[exception]

   @key[when] Asis.Exceptions.ASIS_Inappropriate_Context                     --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Container                 --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit          --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Element                   --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Line                      --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Inappropriate_Line_Number               --  @RefSecNum{package Asis.Exceptions}
        | Asis.Exceptions.ASIS_Failed                                  --  @RefSecNum{package Asis.Exceptions}
      =>

      Put (Asis.Implementation.Diagnosis);                             --  @RefSecNum{function Diagnosis}
      New_Line;
      Put ("Status Value is ");
      Put (Asis.Errors.Error_Kinds'Wide_Image                          --  @RefSecNum{type Error_Kinds}
                      (Asis.Implementation.Status));                   --  @RefSecNum{function Status}
      New_Line;

   @key[when others] =>

      Put_Line ("Asis Application failed because of non-ASIS reasons");

@key[end] ASIS_Call_Tree_Example;
@end{Example}


@leading@;The ASIS_Call_Tree_Example is demonstrated using a context containing the
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


@leading@;Given a context containing the above set of compilation units, the sample
output resulting from the execution of ASIS_Call_Tree_Example is:

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



