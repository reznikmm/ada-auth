with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Wide_Fixed;

package body ARM_ReST is

   function "+" (Item : String) return Wide_String renames
     Ada.Characters.Handling.To_Wide_String;

   function Make_Clause_Anchor_Name (Clause : String) return String;

   procedure Print
     (Self : in out ReST_Output_Type;
      Text : Wide_String);
   --  Add Text to current paragraph
   procedure Print_New_Line (Self : in out ReST_Output_Type);

   ------------------
   -- AI_Reference --
   ------------------

   procedure AI_Reference
     (Output_Object : in out ReST_Output_Type; Text : in String;
      AI_Number     : in     String)
   is
   begin
      raise Program_Error with "Unimplemented procedure AI_Reference";
   end AI_Reference;

   ---------------------
   -- Category_Header --
   ---------------------

   procedure Category_Header
     (Output_Object : in out ReST_Output_Type; Header_Text : String)
   is
   begin
      if not Output_Object.Is_Valid then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Not valid object");
      end if;
      if Output_Object.Is_In_Paragraph then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Header in paragraph");
      end if;
      Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
      Ada.Wide_Text_IO.Put (Output_Object.Output_File, ".. rubric:: ");
      Ada.Wide_Text_IO.Put_Line (Output_Object.Output_File, +Header_Text);
      Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
      Output_Object.Prev_Indent := 0;
   end Category_Header;

   -------------------
   -- Clause_Header --
   -------------------

   procedure Clause_Header
     (Output_Object : in out ReST_Output_Type; Header_Text : in String;
      Level : in     ARM_Contents.Level_Type; Clause_Number : in String;
      Top_Level_Subdivision_Name : in     ARM_Output
        .Top_Level_Subdivision_Name_Kind;
      No_Page_Break : in Boolean := False)
   is
      use Ada.Strings.Wide_Fixed;
      Full_Name : String := Clause_Number & " " & Header_Text;
   begin
      if not Output_Object.Is_Valid then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Not valid object");
      end if;
      if Output_Object.Is_In_Paragraph then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Header in paragraph");
      end if;
      if Ada.Wide_Text_IO.Is_Open (Output_Object.Output_File) then
         Ada.Wide_Text_IO.Close (Output_Object.Output_File);
      end if;
      -- Create a new file for this section:
      Ada.Wide_Text_IO.Create
        (Output_Object.Output_File, Ada.Wide_Text_IO.Out_File,
         Ada.Strings.Unbounded.To_String (Output_Object.Output_Prefix) &
           "-" & Make_Clause_Anchor_Name (Clause_Number) & ".rst",
         Form => "WCEM=8");
      Ada.Wide_Text_IO.Put_Line
        (Output_Object.Output_File, ".. role:: paranum");
      Ada.Wide_Text_IO.Put_Line (Output_Object.Output_File, ".. role:: swiss");
      Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
      Ada.Wide_Text_IO.Put (Output_Object.Output_File, ".. _clause");
      Ada.Wide_Text_IO.Put (Output_Object.Output_File, +Clause_Number);
      Ada.Wide_Text_IO.Put_Line (Output_Object.Output_File, ":");

      Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);

      case Level is
         when ARM_Contents.Unnumbered_Section
            =>
            Ada.Wide_Text_IO.Put_Line
              (Output_Object.Output_File,
               +Header_Text);
            Ada.Wide_Text_IO.Put_Line
              (Output_Object.Output_File,
               Header_Text'Length * '=');
         when ARM_Contents.Section
              | ARM_Contents.Clause
              | ARM_Contents.Subclause
            =>
            Ada.Wide_Text_IO.Put_Line
              (Output_Object.Output_File,
               +Full_Name);
            Ada.Wide_Text_IO.Put_Line
              (Output_Object.Output_File,
               Full_Name'Length * '=');
         when others =>
            raise Program_Error;
      end case;
   end Clause_Header;

   ----------------------
   -- Clause_Reference --
   ----------------------

   procedure Clause_Reference
     (Output_Object : in out ReST_Output_Type;
      Text          : in String;
      Clause_Number : in     String)
   is
      --  No format inside references in ReST. So reset format temporary
      Format : ARM_Output.Format_Type := Output_Object.Requested;
   begin
      if Clause_Number = "X.X" then
         -- Link to a dead clause, just output the text (presumably this
         -- is deleted).
         Output_Object.Ordinary_Text (Text);
      else
         Output_Object.Requested := ARM_Output.NORMAL_FORMAT;
         Output_Object.Print ("\ :ref:`");
         Output_Object.Print (+Text);
         Output_Object.Print ("<clause");
         Output_Object.Print (+Clause_Number);
         Output_Object.Print (">`\ ");
         Output_Object.Requested := Format;
      end if;
   end Clause_Reference;

   -----------
   -- Close --
   -----------

   procedure Close (Output_Object : in out ReST_Output_Type) is
   begin
      raise Program_Error with "Unimplemented procedure Close";
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (Output_Object : in out ReST_Output_Type; File_Prefix : in String;
      Output_Path   : in     String; Title : in String := "")
   is
   begin
      Output_Object.Output_Prefix :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (Output_Path & File_Prefix);
      Output_Object.Is_Valid := True;
   end Create;

   ------------------
   -- DR_Reference --
   ------------------

   procedure DR_Reference
     (Output_Object : in out ReST_Output_Type; Text : in String;
      DR_Number     : in     String)
   is
   begin
      raise Program_Error with "Unimplemented procedure DR_Reference";
   end DR_Reference;

   -------------------
   -- End_Hang_Item --
   -------------------

   procedure End_Hang_Item (Output_Object : in out ReST_Output_Type) is
      use Ada.Strings.Wide_Fixed;
   begin
      --  Close any format
      Output_Object.Requested := ARM_Output.NORMAL_FORMAT;
      Output_Object.Print ("");
      Output_Object.Print_New_Line;
      Output_Object.Print_New_Line;
   end End_Hang_Item;

   -------------------
   -- End_Paragraph --
   -------------------

   procedure End_Paragraph (Output_Object : in out ReST_Output_Type) is
      use Ada.Strings.Wide_Fixed;
      use type ARM_Output.Paragraph_Indent_Type;
      First_Line : Boolean := True;
   begin
      --  Close any format
      Output_Object.Requested := ARM_Output.NORMAL_FORMAT;
      Output_Object.Print ("");

      if Output_Object.Table.Columns = 0 then
         if Output_Object.Indent > Output_Object.Prev_Indent then
            --  Add an empty paragraph after rubric if first para has indent
            Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
            Ada.Wide_Text_IO.Put
              (Output_Object.Output_File,
               2 * Natural (Output_Object.Prev_Indent) * ' ');
            Ada.Wide_Text_IO.Put_Line (Output_Object.Output_File, "\ ");
         end if;

         --  An extra new line after a possible Index_Target
         Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);

         for Line of Output_Object.Paragraph loop
            if Output_Object.Line_Block then
               Ada.Wide_Text_IO.Put (Output_Object.Output_File, "  | ");
            end if;

            if not First_Line and Output_Object.Indent > 0 then
               Ada.Wide_Text_IO.Put
                 (Output_Object.Output_File,
                  2 * Natural (Output_Object.Indent) * ' ');
            end if;

            Ada.Wide_Text_IO.Put_Line
              (Output_Object.Output_File,
               Ada.Strings.Wide_Unbounded.To_Wide_String (Line));

            First_Line := False;
         end loop;

         Output_Object.Paragraph.Clear;
      end if;

      Output_Object.Is_In_Paragraph := False;
      Output_Object.Prev_Indent := Output_Object.Indent;
   end End_Paragraph;

   ----------------
   -- Hard_Space --
   ----------------

   procedure Hard_Space (Output_Object : in out ReST_Output_Type) is
      NBSP : Wide_Character := Wide_Character'Val (160);
   begin
      Output_Object.Print ((1 => NBSP));
   end Hard_Space;

   ----------------------
   -- Index_Line_Break --
   ----------------------

   procedure Index_Line_Break
     (Output_Object        : in out ReST_Output_Type;
      Clear_Keep_with_Next : in     Boolean)
   is
   begin
      raise Program_Error with "Unimplemented procedure Index_Line_Break";
   end Index_Line_Break;

   ---------------------
   -- Index_Reference --
   ---------------------

   procedure Index_Reference
     (Output_Object : in out ReST_Output_Type; Text : in String;
      Index_Key     : in     Natural; Clause_Number : in String)
   is
   begin
      raise Program_Error with "Unimplemented procedure Index_Reference";
   end Index_Reference;

   ------------------
   -- Index_Target --
   ------------------

   procedure Index_Target
     (Output_Object : in out ReST_Output_Type; Index_Key : in Natural)
   is
      use Ada.Strings.Wide_Fixed;
      use type ARM_Output.Paragraph_Indent_Type;
      Target : Wide_String := Natural'Wide_Image (Index_Key);
   begin
      if Output_Object.Indent > Output_Object.Prev_Indent then
         --  Add an empty paragraph after rubric if first para has indent
         Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
         Ada.Wide_Text_IO.Put
           (Output_Object.Output_File,
            2 * Natural (Output_Object.Prev_Indent) * ' ');
         Ada.Wide_Text_IO.Put_Line (Output_Object.Output_File, "\ ");
      end if;

      Output_Object.Prev_Indent := Output_Object.Indent;
      Target (1) := 'i';  -- FIXME
      Ada.Wide_Text_IO.Put
        (Output_Object.Output_File,
         2 * Natural (Output_Object.Indent) * ' ');
      Ada.Wide_Text_IO.Put (Output_Object.Output_File, ".. _");
      Ada.Wide_Text_IO.Put (Output_Object.Output_File, Target);
      Ada.Wide_Text_IO.Put_Line (Output_Object.Output_File, ":");
   end Index_Target;

   ----------------
   -- Line_Break --
   ----------------

   procedure Line_Break (Output_Object : in out ReST_Output_Type) is
      use Ada.Strings.Wide_Fixed;
      use all type ARM_Output.Paragraph_Style_Type;
      use all type ARM_Output.Paragraph_Indent_Type;
      NBSP : Wide_Character := Wide_Character'Val (133);
   begin
      --  Close any format
      Output_Object.Requested := ARM_Output.NORMAL_FORMAT;
      Output_Object.Print ("");
      Output_Object.Line_Block := True;
      Output_Object.Print_New_Line;
   end Line_Break;

   ----------------
   -- Local_Link --
   ----------------

   procedure Local_Link
     (Output_Object : in out ReST_Output_Type;
      Text          : in String;
      Target        : in String;
      Clause_Number : in String)
   is
      --  No format inside references in ReST. So reset format temporary
      Format : ARM_Output.Format_Type := Output_Object.Requested;
   begin
      Output_Object.Requested := ARM_Output.NORMAL_FORMAT;
      Output_Object.Print ("\ :ref:`");
      Output_Object.Print (+Text);
      Output_Object.Print ("<");
      Output_Object.Print (+Clause_Number);
      Output_Object.Print ("_");
      Output_Object.Print (+Target);
      Output_Object.Print (">`\ ");
      Output_Object.Requested := Format;
   end Local_Link;

   --------------------
   -- Local_Link_End --
   --------------------

   procedure Local_Link_End
     (Output_Object : in out ReST_Output_Type; Target : in String;
      Clause_Number : in     String)
   is
   begin
      raise Program_Error with "Unimplemented procedure Local_Link_End";
   end Local_Link_End;

   ----------------------
   -- Local_Link_Start --
   ----------------------

   procedure Local_Link_Start
     (Output_Object : in out ReST_Output_Type; Target : in String;
      Clause_Number : in     String)
   is
   begin
      raise Program_Error with "Unimplemented procedure Local_Link_Start";
   end Local_Link_Start;

   ------------------
   -- Local_Target --
   ------------------

   procedure Local_Target
     (Output_Object : in out ReST_Output_Type; Text : in String;
      Target        : in     String)
   is
   begin
      raise Program_Error with "Unimplemented procedure Local_Target";
   end Local_Target;

   -----------------------------
   -- Make_Clause_Anchor_Name --
   -----------------------------

   function Make_Clause_Anchor_Name (Clause : String) return String is
   begin
      if Clause'Length > 7 and then
        Clause (Clause'First .. Clause'First + 5) = "Annex "
      then
         return (1 => Clause (Clause'First + 6));
      else
         return Clause;
      end if;
   end Make_Clause_Anchor_Name;

   ----------------
   -- New_Column --
   ----------------

   procedure New_Column (Output_Object : in out ReST_Output_Type) is
   begin
      Output_Object.Print_New_Line;
      Output_Object.Last_Column := Output_Object.Last_Column + 1;
      Output_Object.Table.Start (Output_Object.Last_Column) :=
        Output_Object.Paragraph.Last_Index;
   end New_Column;

   --------------
   -- New_Page --
   --------------

   procedure New_Page
     (Output_Object : in out ReST_Output_Type;
      Kind          :        ARM_Output.Page_Kind_Type := ARM_Output.Any_Page)
   is
   begin
      null;
   end New_Page;

   ------------------------
   -- Ordinary_Character --
   ------------------------

   procedure Ordinary_Character
     (Output_Object : in out ReST_Output_Type; Char : in Character)
   is
   begin
      if not Output_Object.Is_Valid then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Not valid object");
      end if;
      if not Output_Object.Is_In_Paragraph then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Not in paragraph");
      end if;

      if Char in '+' | '*' | '|' then
         Output_Object.Print ((1 => '\'));
      end if;

      if Character'Pos (Char) in 0 .. 127 | 169 | 228 | 229 then
         Output_Object.Print
           ((1 => Wide_Character'Val (Character'Pos (Char))));
      else
         raise Program_Error;
      end if;
   end Ordinary_Character;

   -------------------
   -- Ordinary_Text --
   -------------------

   procedure Ordinary_Text
     (Output_Object : in out ReST_Output_Type; Text : in String)
   is
   begin
      for Char of Text loop
         Output_Object.Ordinary_Character (Char);
      end loop;
   end Ordinary_Text;

   -----------
   -- Print --
   -----------

   procedure Print
     (Self : in out ReST_Output_Type;
      Text : Wide_String)
   is

      procedure Append (Text : Wide_String) is
      begin
         Ada.Strings.Wide_Unbounded.Append
           (Self.Paragraph (Self.Paragraph.Last_Index), Text);
      end Append;

      procedure Change_Format
        (Format : ARM_Output.Format_Type;
         On     : Boolean)
      is
         use all type ARM_Output.Font_Family_Type;
         use all type ARM_Output.Location_Type;
      begin
         if On then
            if Format.Bold then
               Append ("\ :strong:`");
            elsif Format.Italic then
               Append ("\ :emphasis:`");
               --  elsif Format.Size then
            elsif Format.Location = Subscript then
               Append ("\ :subscript:`");
            elsif Format.Location = Superscript then
               Append ("\ :superscript:`");
            elsif Format.Font = Swiss then
               Append ("\ :swiss:`");
            elsif Format.Font /= Default then
               raise Program_Error;
            end if;
         else
            if Format.Bold or Format.Italic
              or Format.Location in Subscript | Superscript
              or Format.Font = Swiss
            then
               Append ("`\ ");
            end if;
         end if;
      end Change_Format;

      use type ARM_Output.Format_Type;

   begin
      if Self.Requested /= Self.Actual then
         if Self.Actual /= ARM_Output.NORMAL_FORMAT then
            Change_Format (Self.Actual, On => False);
         end if;

         if Self.Requested /= ARM_Output.NORMAL_FORMAT then
            Change_Format (Self.Requested, On => True);
         end if;

         Self.Actual := Self.Requested;
      end if;

      Append (Text);
   end Print;

   --------------------
   -- Print_New_Line --
   --------------------

   procedure Print_New_Line (Self : in out ReST_Output_Type) is
   begin
      Self.Paragraph.Append
        (Ada.Strings.Wide_Unbounded.Null_Unbounded_Wide_String);
   end Print_New_Line;

   -------------
   -- Picture --
   -------------

   procedure Picture
     (Output_Object : in out ReST_Output_Type; Name : in String;
      Descr         : in String; Alignment : in ARM_Output.Picture_Alignment;
      Height, Width : in     Natural; Border : in ARM_Output.Border_Kind)
   is
   begin
      raise Program_Error with "Unimplemented procedure Picture";
   end Picture;

   ---------------------------
   -- Revised_Clause_Header --
   ---------------------------

   procedure Revised_Clause_Header
     (Output_Object : in out ReST_Output_Type; New_Header_Text : in String;
      Old_Header_Text : in     String; Level : in ARM_Contents.Level_Type;
      Clause_Number : in String; Version : in ARM_Contents.Change_Version_Type;
      Old_Version                : in     ARM_Contents.Change_Version_Type;
      Top_Level_Subdivision_Name : in     ARM_Output
        .Top_Level_Subdivision_Name_Kind;
      No_Page_Break : in Boolean := False)
   is
   begin
      raise Program_Error with "Unimplemented procedure Revised_Clause_Header";
   end Revised_Clause_Header;

   -------------
   -- Section --
   -------------

   procedure Section
     (Output_Object : in out ReST_Output_Type; Section_Title : in String;
      Section_Name  : in     String)
   is
   begin
      if not Output_Object.Is_Valid then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Not valid object");
      end if;
      if Output_Object.Is_In_Paragraph then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Section in paragraph");
      end if;
      if Ada.Wide_Text_IO.Is_Open (Output_Object.Output_File) then
         Ada.Wide_Text_IO.Close (Output_Object.Output_File);
      end if;
      -- Create a new file for this section:
      Ada.Wide_Text_IO.Create
        (Output_Object.Output_File, Ada.Wide_Text_IO.Out_File,
         Ada.Strings.Unbounded.To_String (Output_Object.Output_Prefix) &
           "-" & Section_Name & ".rst",
         Form => "WCEM=8");
      Ada.Wide_Text_IO.Put_Line
        (Output_Object.Output_File, ".. role:: paranum");
      Ada.Wide_Text_IO.Put_Line (Output_Object.Output_File, ".. role:: swiss");
      Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);

      Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);

   end Section;

   --------------------
   -- Separator_Line --
   --------------------

   procedure Separator_Line
     (Output_Object : in out ReST_Output_Type; Is_Thin : Boolean := True)
   is
   begin
      Output_Object.Print_New_Line;
      Output_Object.Print ("----");
   end Separator_Line;

   -----------------
   -- Set_Columns --
   -----------------

   procedure Set_Columns
     (Output_Object     : in out ReST_Output_Type;
      Number_of_Columns : in     ARM_Output.Column_Count)
   is
   begin
      if Number_of_Columns = 1 then
         declare
            use Ada.Strings.Wide_Fixed;
            T      : Table_Offsets renames Output_Object.Table;
            Length : Column_Offset := (1 .. T.Columns => 1);
            Value  : Natural;
         begin
            for J in 1 .. T.Columns loop
               for K in 1 .. T.Start (2) - T.Start (1) loop
                  Value := Ada.Strings.Wide_Unbounded.Length
                    (Output_Object.Paragraph (T.Start (J) + K - 1));
                  if Length (J) < Value then
                     Length (J) := Value;
                  end if;
               end loop;
            end loop;

            for J in 1 .. T.Columns loop
               if J > 1 then
                  Ada.Wide_Text_IO.Put (Output_Object.Output_File, ' ');
               end if;

               Ada.Wide_Text_IO.Put
                 (Output_Object.Output_File, Length (J) * '=');
            end loop;

            Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);

            for K in 1 .. T.Start (2) - T.Start (1) loop
               for J in 1 .. T.Columns loop
                  Value := Ada.Strings.Wide_Unbounded.Length
                    (Output_Object.Paragraph (T.Start (J) + K - 1));

                  Ada.Wide_Text_IO.Put
                    (Output_Object.Output_File,
                     Ada.Strings.Wide_Unbounded.To_Wide_String
                       (Output_Object.Paragraph (T.Start (J) + K - 1)));

                  Ada.Wide_Text_IO.Put
                    (Output_Object.Output_File, (Length (J) - Value + 1) * ' ');
               end loop;
               Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
            end loop;

            for J in 1 .. T.Columns loop
               if J > 1 then
                  Ada.Wide_Text_IO.Put (Output_Object.Output_File, ' ');
               end if;

               Ada.Wide_Text_IO.Put
                 (Output_Object.Output_File, Length (J) * '=');
            end loop;

            Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
            Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);

            Output_Object.Paragraph.Clear;
         end;
      else
         Output_Object.Print_New_Line;
         Output_Object.Table :=
           (Columns => Number_of_Columns,
            Start   => (1 => Output_Object.Paragraph.Last_Index, others => <>));
         Output_Object.Last_Column := 1;
      end if;
   end Set_Columns;

   -----------------------
   -- Soft_Hyphen_Break --
   -----------------------

   procedure Soft_Hyphen_Break (Output_Object : in out ReST_Output_Type) is
   begin
      raise Program_Error with "Unimplemented procedure Soft_Hyphen_Break";
   end Soft_Hyphen_Break;

   ---------------------
   -- Soft_Line_Break --
   ---------------------

   procedure Soft_Line_Break (Output_Object : in out ReST_Output_Type) is
   begin
      raise Program_Error with "Unimplemented procedure Soft_Line_Break";
   end Soft_Line_Break;

   -----------------------
   -- Special_Character --
   -----------------------

   procedure Special_Character
     (Output_Object : in out ReST_Output_Type;
      Char          : in     ARM_Output.Special_Character_Type)
   is
   begin
      case Char is
         when ARM_Output.Left_Double_Quote =>
            Output_Object.Print ((1 => Wide_Character'Val (8220)));
         when ARM_Output.Right_Double_Quote =>
            Output_Object.Print ((1 => Wide_Character'Val (8221)));
         when ARM_Output.EM_Dash =>
            Output_Object.Print ((1 => Wide_Character'Val (8212)));
         when ARM_Output.EN_Dash =>
            Output_Object.Print ((1 => Wide_Character'Val (8211)));
         when others =>
            raise Program_Error with "Unimplemented procedure Special_Character";
      end case;
   end Special_Character;

   ---------------------
   -- Start_Paragraph --
   ---------------------

   procedure Start_Paragraph
     (Output_Object : in out ReST_Output_Type;
      Style         : in     ARM_Output.Paragraph_Style_Type;
      Indent        : in ARM_Output.Paragraph_Indent_Type; Number : in String;
      No_Prefix     : in     Boolean                       := False;
      Tab_Stops     : in     ARM_Output.Tab_Info := ARM_Output.NO_TABS;
      No_Breaks : in Boolean := False; Keep_with_Next : in Boolean := False;
      Space_After   : in     ARM_Output.Space_After_Type := ARM_Output.Normal;
      Justification : in ARM_Output.Justification_Type := ARM_Output.Default)
   is
      use Ada.Strings.Wide_Fixed;
      use all type ARM_Output.Paragraph_Style_Type;
      use type ARM_Output.Paragraph_Indent_Type;
   begin
      Output_Object.Line_Block := False;

      if Style = Wide_Hanging then
         Output_Object.Print_New_Line;
         Output_Object.Print (2 * Natural (Indent) * ' ');
         Output_Object.Print (".. rubric:: ");
         Output_Object.Print (":paranum:`");
         Output_Object.Print (+Number);
         Output_Object.Print ("`\ ");
         Output_Object.Prev_Indent := Indent;
      elsif Style = Small_Examples and Indent = 2 then
         Output_Object.Line_Block := True;
         Output_Object.Print_New_Line;
      elsif Style = Small_Header and Indent = 1 then
         Output_Object.Print_New_Line;
         Output_Object.Print (".. note::");
         Output_Object.Print ("  ");
      elsif Style = Small and Indent = 1 then
         Output_Object.Print_New_Line;
         Output_Object.Print ("  ");
      elsif Style in Bulleted | Nested_Bulleted and Indent in 1 .. 2 then
         Output_Object.Print_New_Line;
         Output_Object.Print (2 * Natural (Indent) * ' ');
         Output_Object.Print ("* ");

         if Number /= "" then
            Output_Object.Print (":paranum:`");
            Output_Object.Print (+Number);
            Output_Object.Print ("`\ ");
         end if;
      elsif Style in Normal | Wide_Above
        and Indent in 0 .. 2
      then
         Output_Object.Print_New_Line;

         Output_Object.Print (2 * Natural (Indent) * ' ');

         if Number /= "" then
            Output_Object.Print (":paranum:`");
            Output_Object.Print (+Number);
            Output_Object.Print ("`\ ");
         end if;

      elsif Indent /= 0
        or (Style in ARM_Output.Prefixed_Style_Subtype and No_Prefix)
      then
         raise Program_Error with "Unimplemented procedure Start_Paragraph";
      elsif Style = Title and Number = "" then
         Output_Object.Print_New_Line;
         Output_Object.Print (2 * Natural (Indent) * ' ');
         Output_Object.Print (".. rubric:: ");
         Output_Object.Prev_Indent := Indent;
      else
         raise Program_Error with "Unimplemented procedure Start_Paragraph";
      end if;

      Output_Object.Is_In_Paragraph := True;
      Output_Object.Style := Style;
      Output_Object.Indent := Indent;

      Ada.Wide_Text_IO.New_Line (Output_Object.Output_File);
   end Start_Paragraph;

   -----------------
   -- Start_Table --
   -----------------

   procedure Start_Table
     (Output_Object      : in out ReST_Output_Type;
      Columns            : in     ARM_Output.Column_Count;
      First_Column_Width : in     ARM_Output.Column_Count;
      Last_Column_Width  : in     ARM_Output.Column_Count;
      Alignment          : in     ARM_Output.Column_Text_Alignment;
      No_Page_Break      : in     Boolean; Has_Border : in Boolean;
      Small_Text_Size    : in     Boolean;
      Header_Kind        : in     ARM_Output.Header_Kind_Type)
   is
   begin
      raise Program_Error with "Unimplemented procedure Start_Table";
   end Start_Table;

   ---------
   -- Tab --
   ---------

   procedure Tab (Output_Object : in out ReST_Output_Type) is
   begin
      null;  --  FIXME?
   end Tab;

   ------------------
   -- Table_Marker --
   ------------------

   procedure Table_Marker
     (Output_Object : in out ReST_Output_Type;
      Marker        : in     ARM_Output.Table_Marker_Type)
   is
   begin
      raise Program_Error with "Unimplemented procedure Table_Marker";
   end Table_Marker;

   -----------------
   -- Text_Format --
   -----------------

   procedure Text_Format
     (Output_Object : in out ReST_Output_Type;
      Format        : in     ARM_Output.Format_Type)
   is
      Value : ARM_Output.Format_Type := Format;
   begin
      --  We don't use these properties for now:
      Value.Size := 0;
      Value.Color := ARM_Output.Default;
      Value.Change := ARM_Output.None;
      Value.Version := '0';
      Value.Added_Version := '0';

      if not Output_Object.Is_Valid then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Not valid object");
      end if;
      if not Output_Object.Is_In_Paragraph then
         Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity,
                                         "Not in paragraph");
      end if;

      Output_Object.Requested := Value;
   end Text_Format;

   ----------------
   -- TOC_Marker --
   ----------------

   procedure TOC_Marker
     (Output_Object : in out ReST_Output_Type; For_Start : in Boolean)
   is
   begin
      raise Program_Error with "Unimplemented procedure TOC_Marker";
   end TOC_Marker;

   -----------------------
   -- Unicode_Character --
   -----------------------

   procedure Unicode_Character
     (Output_Object : in out ReST_Output_Type;
      Char          : in     ARM_Output.Unicode_Type)
   is
   begin
      raise Program_Error with "Unimplemented procedure Unicode_Character";
   end Unicode_Character;

   --------------
   -- URL_Link --
   --------------

   procedure URL_Link
     (Output_Object : in out ReST_Output_Type; Text : in String;
      URL           : in     String)
   is
   begin
      raise Program_Error with "Unimplemented procedure URL_Link";
   end URL_Link;

end ARM_ReST;
