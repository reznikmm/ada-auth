with Ada.Exceptions;
with Ada.Strings.Fixed;
package body ARM_Texinfo is

   --  Copyright (C) 2003, 2007, 2010 Stephen Leake.  All Rights Reserved.
   --  E-Mail: stephen_leake@acm.org
   --
   --  This library is free software; you can redistribute it and/or
   --  modify it under terms of the GNU General Public License as
   --  published by the Free Software Foundation; either version 3, or (at
   --  your option) any later version. This library is distributed in the
   --  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
   --  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   --  PURPOSE. See the GNU General Public License for more details. You
   --  should have received a copy of the GNU General Public License
   --  distributed with this program; see file gnu-3-0.txt. If not,
   --  see <http://www.gnu.org/licenses/>.
   --
   --  As a special exception, if other files instantiate generics from
   --  this unit, or you link this unit with other files to produce an
   --  executable, this  unit  does not  by itself cause  the resulting
   --  executable to be covered by the GNU General Public License. This
   --  exception does not however invalidate any other reasons why the
   --  executable file  might be covered by the  GNU Public License.

   -- ---------------------------------------
   --
   -- Edit History:
   --
   -- Ancient  - S L - Developed package as add-on to Arm_Form.
   -- 10/19/11 - RLB - Integrated outside-developed package into Arm_Form.
   --                  Commented out/replaced Ada 2005 features (this is
   --		       Ada 95 code). Updated for a few other changes since
   --		       the last update.
   -- 10/25/11 - RLB - Added old insertion version to Revised_Clause_Header.


   use Ada.Text_IO;

   Indentation : constant := 5;

   --  VERSION: This is fragile; it changes with each version of the manual.
   Index_Clause      : constant String    := "0.5";
   Index_Clause_Name : constant String    := "Index";
   Index_Clause_Next : constant String    := "operators";
   Operators_Clause  : constant String    := "operators";
   Last_Index_Clause : constant Character := 'Y';

   ----------
   --  local subprograms

   procedure Check_Not_In_Paragraph (Output_Object : in Texinfo_Output_Type)
   is begin
      if Output_Object.In_Paragraph then
         Ada.Exceptions.Raise_Exception
           (ARM_Output.Not_Valid_Error'Identity,
            "In paragraph");
      end if;
   end Check_Not_In_Paragraph;

   procedure Check_Valid (Output_Object : in Texinfo_Output_Type)
   is begin
      if not Output_Object.Is_Valid then
         Ada.Exceptions.Raise_Exception
           (ARM_Output.Not_Valid_Error'Identity,
            "Not valid object");
      end if;
   end Check_Valid;

   procedure Unexpected_State (Output_Object : in Texinfo_Output_Type)
   is begin
      Ada.Exceptions.Raise_Exception
        (ARM_Output.Not_Valid_Error'Identity,
         "Unexpected state: " & State_Type'Image (Output_Object.State));
   end Unexpected_State;

   procedure Escape_Put
     (Output_Object  : in Texinfo_Output_Type;
      Char           : in Character;
      Preserve_Space : in Boolean             := False)
   is begin
      --  Escape special chars
      if Char = '@' then
         Put (Output_Object.File, "@@");
      elsif Char = '{' then
         Put (Output_Object.File, "@{");
      elsif Char = '}' then
         Put (Output_Object.File, "@}");
      elsif Char = ''' then
         --  Avoid makeinfo converting '' into "
         Put (Output_Object.File, "'@w{}");
      elsif Char = '`' then
         --  Avoid makeinfo converting `` into "
         Put (Output_Object.File, "`@w{}");
      elsif Char = '-' then
         Put (Output_Object.File, "@minus{}");
      elsif Char = ' ' and Preserve_Space then
         --  Don't allow collapsing spaces
         Put (Output_Object.File, "@w{ }");
      elsif Char = '\' then
         --  This confuses texi2dvi if not escaped.
         Put (Output_Object.File, "@code{\}");
      else
         Put (Output_Object.File, Char);
      end if;
   end Escape_Put;

   procedure Escape_Put
     (Output_Object  : in Texinfo_Output_Type;
      Text           : in String;
      Preserve_Space : in Boolean             := False)
   is begin
      for I in Text'Range loop
         Escape_Put (Output_Object, Text (I), Preserve_Space);
      end loop;
   end Escape_Put;

   procedure End_Title_Page (Output_Object : in out Texinfo_Output_Type)
   is
      use ARM_Contents;

      procedure Put_Top_Menu_Item
        (Title         : in     Title_Type;
         Level         : in     Level_Type;
         Clause_Number : in     Clause_Number_Type;
         Version       : in     ARM_Contents.Change_Version_Type;
         Quit          :    out Boolean)
      is
         pragma Unreferenced (Version); --  we are only concerned with version 2
         First_Part : String (1 .. 14); --  Get all Titles aligned.
      begin
         Quit := False;

         case Level is
         when Section | Normative_Annex | Informative_Annex | Plain_Annex =>
            Ada.Strings.Fixed.Move
              (Source =>
                 "* " &
                 Make_Clause_Number (Level, Clause_Number) &
                 " ::",
               Target => First_Part);

            Put_Line (Output_Object.File, First_Part & Title);

         when Unnumbered_Section | Clause | Subclause | Subsubclause =>
            null;

	 when ARM_Contents.Dead_Clause  =>
	    raise Program_Error; -- No headers for dead clauses.

         end case;
      end Put_Top_Menu_Item;

      procedure Put_Top_Menu is new For_Each (Put_Top_Menu_Item);
   begin

      New_Line (Output_Object.File); --  Terminate unneeded "@center"

      Put_Line (Output_Object.File, "@dircategory GNU Ada tools");
      Put_Line (Output_Object.File, "@direntry");
      Put_Line (Output_Object.File, "* Ada Reference Manual: (arm2005).");
      Put_Line (Output_Object.File, "* Annotated ARM: (aarm2005).");
      Put_Line (Output_Object.File, "@end direntry");

      Put_Line (Output_Object.File, "@menu");
      Put_Line (Output_Object.File, "* Front Matter:: Copyright, Foreword, etc."); --  Not a section in ARM sources
      Put_Top_Menu;
      Put_Line (Output_Object.File, "* Index ::    Index"); --  Not in ARM sources
      Put_Line (Output_Object.File, "@end menu");

      -- @node current, next, prev, up
      Put_Line (Output_Object.File, "@node Front Matter, 0.1, Top, Top");
      Put_Line (Output_Object.File, "@chapter Front Matter");
   end End_Title_Page;

   procedure Get_Clause_Section
     (Clause_String  : in     String;
      Section_Number :    out ARM_Contents.Section_Number_Type;
      Clause_Integer :    out Natural)
   is
      --  This is a partial inverse of ARM_Contents.Make_Clause_Number.
      --
      --  Clause_String has "section.clause.subclause", possibly no subclause.
      --
      --  "section" can be a number, a letter "N", or "Annex N", where
      --
      --  'N' = Character'Val (Character'Pos('A') + (Section_Number - ANNEX_START)

      Section_Dot : constant Natural := Ada.Strings.Fixed.Index (Source => Clause_String, Pattern => ".");

      Clause_Dot : constant Natural := Ada.Strings.Fixed.Index
        (Source => Clause_String (Section_Dot + 1 .. Clause_String'Last),
         Pattern => ".");

      use type ARM_Contents.Section_Number_Type;
   begin
      if Section_Dot = 8 then
         --  Section is "Annex N"
         Section_Number := ARM_Contents.ANNEX_START +
           Character'Pos (Clause_String (Clause_String'First + 6)) - Character'Pos ('A');
      elsif Character'Pos (Clause_String (Clause_String'First)) >= Character'Pos ('A') then
         --  Section is letter.
         Section_Number := ARM_Contents.ANNEX_START +
           Character'Pos (Clause_String (Clause_String'First)) - Character'Pos ('A');
      else
         Section_Number := ARM_Contents.Section_Number_Type'Value
           (Clause_String (Clause_String'First .. Section_Dot - 1));
      end if;

      if Clause_Dot = 0 then
         Clause_Integer := Natural'Value
           (Clause_String (Section_Dot + 1 .. Clause_String'Last));
      else
         Clause_Integer := Natural'Value
           (Clause_String (Section_Dot + 1 .. Clause_Dot - 1));
      end if;
   end Get_Clause_Section;

   procedure Add_To_Column_Item (Output_Object : in out Texinfo_Output_Type; Text : in String)
   is begin
      if Output_Object.Column_Text (Output_Object.Current_Column) = null or else
        Output_Object.Column_Text (Output_Object.Current_Column).Row /= Output_Object.Current_Row
      then
         --  Start a new row.
         Output_Object.Column_Text (Output_Object.Current_Column) :=
           new Column_Text_Item_Type'
           (Text   => (others => ' '),
            Length => 0,
            Row    => Output_Object.Current_Row,
            Next   => Output_Object.Column_Text (Output_Object.Current_Column));
      end if;

      if Output_Object.Column_Text (Output_Object.Current_Column).Length + Text'Length >
        Output_Object.Column_Text (Output_Object.Current_Column).Text'Length
      then
         Ada.Exceptions.Raise_Exception
           (ARM_Output.Not_Valid_Error'Identity,
            "Column item full, but more text: " &
              Output_Object.Column_Text (Output_Object.Current_Column).Text
              (1 .. Output_Object.Column_Text (Output_Object.Current_Column).Length));
      else
         declare
            Current_Text : Column_Text_Item_Type renames Output_Object.Column_Text (Output_Object.Current_Column).all;
         begin
            Current_Text.Text (Current_Text.Length + 1 .. Current_Text.Length + Text'Length) := Text;

            Current_Text.Length := Current_Text.Length + Text'Length;

            if Output_Object.Column_Widths (Output_Object.Current_Column) < Current_Text.Length then
               Output_Object.Column_Widths (Output_Object.Current_Column) := Current_Text.Length;
            end if;
         end;
      end if;
   end Add_To_Column_Item;

   procedure Pad_Columns (Output_Object : in out Texinfo_Output_Type)
   --  Ensure that all columns have the same number of (possibly
   --  empty) rows, for table headers.
   is
      Item          : Column_Text_Ptr;
      First_New_Row : Natural;
   begin
      for Col in 1 .. Output_Object.Column_Count loop
         Item := Output_Object.Column_Text (Col);
         if Item = null then
            First_New_Row := 1;
         else
            First_New_Row := Item.Row + 1;
         end if;

         for I in First_New_Row .. Output_Object.Max_Row loop
            Output_Object.Column_Text (Col) :=
              new Column_Text_Item_Type'
              (Text   => (others => ' '),
               Length => 1,
               Row    => I,
               Next   => Output_Object.Column_Text (Col));
         end loop;
      end loop;
   end Pad_Columns;

   procedure Output_Column_Widths (Output_Object : in out Texinfo_Output_Type)
   is begin
      New_Line (Output_Object.File);
      Put (Output_Object.File, "@multitable ");
      for I in 1 .. Output_Object.Column_Count loop
         Put
           (Output_Object.File,
            " {" &
              String'(1 .. Output_Object.Column_Widths (I) => 'w') &
              "}");
      end loop;
   end Output_Column_Widths;

   procedure Output_Columns (Output_Object : in out Texinfo_Output_Type)
   is
      Row  : Natural         := 1;
      Item : Column_Text_Ptr;
      Temp : Column_Text_Ptr;
   begin
      Rows :
      loop
         New_Line (Output_Object.File);
         Put (Output_Object.File, "@item ");

         --  For all columns, output the items for this row. Note that
         --  the last row is at the front of each column list; the
         --  first row is at the end. We delete the rows as we output
         --  them, so the one we want is always at the end of the
         --  column list.
         Columns :
         for Col in 1 .. Output_Object.Column_Count loop
            Item := Output_Object.Column_Text (Col);

            if Item = null then
               --  Previously finished column
               null;

            elsif Item.Next = null then
               --  This is the last item in the column.
               if Item.Row /= Row then
                  --  This column is empty for this row.
                  Item := null;
               else
                  --  Output Item, and mark that we're done outputing
                  --  this column.
                  Output_Object.Column_Text (Col) := null;
               end if;
            else
               --  Find first item for this row in the column.
               while Item.Next /= null and then Item.Next.Row /= Row loop
                  Item := Item.Next;
               end loop;

               --  Output Item.Next, take it out of list.
               Temp      := Item;
               Item      := Item.Next;
               Temp.Next := null;
            end if;

            if Item /= null then
               --  Output the item
               Escape_Put (Output_Object, Item.Text (1 .. Item.Length), Preserve_Space => True);
               Free (Item);

               if Col /= Output_Object.Column_Count then
                  Put (Output_Object.File, " @tab ");
               end if;

            else
               --  This column is empty for this row
               if Col < Output_Object.Column_Count then
                  Put (Output_Object.File, " @tab ");
               end if;
            end if;
         end loop Columns;

         if Output_Object.Column_Text = Column_Text_Ptrs_Type'(others => null) then
            --  We've output everything.
            exit Rows;
         end if;

         --  End the row:
         Row := Row + 1;
      end loop Rows;
   end Output_Columns;

   procedure Index_Menu (Output_Object : in out Texinfo_Output_Type)
   is begin
      Put_Line (Output_Object.File, "@menu");
      Put_Line (Output_Object.File, "* operators::");
      Put_Line (Output_Object.File, "* A::");
      Put_Line (Output_Object.File, "* B::");
      Put_Line (Output_Object.File, "* C::");
      Put_Line (Output_Object.File, "* D::");
      Put_Line (Output_Object.File, "* E::");
      Put_Line (Output_Object.File, "* F::");
      Put_Line (Output_Object.File, "* G::");
      Put_Line (Output_Object.File, "* H::");
      Put_Line (Output_Object.File, "* I::");
      Put_Line (Output_Object.File, "* J::");
      Put_Line (Output_Object.File, "* K::");
      Put_Line (Output_Object.File, "* L::");
      Put_Line (Output_Object.File, "* M::");
      Put_Line (Output_Object.File, "* N::");
      Put_Line (Output_Object.File, "* O::");
      Put_Line (Output_Object.File, "* P::");
      Put_Line (Output_Object.File, "* Q::");
      Put_Line (Output_Object.File, "* R::");
      Put_Line (Output_Object.File, "* S::");
      Put_Line (Output_Object.File, "* T::");
      Put_Line (Output_Object.File, "* U::");
      Put_Line (Output_Object.File, "* V::");
      Put_Line (Output_Object.File, "* W::");
      Put_Line (Output_Object.File, "* X::");
      Put_Line (Output_Object.File, "* Y::");
      --  Put_Line (Output_Object.File, "* Z::"); --  VERSION: No entries in Z
      Put_Line (Output_Object.File, "@end menu");

      --  @node current, next, prev, up
      Put_Line
        (Output_Object.File,
         "@node " & Operators_Clause &
           ", A, " & Index_Clause_Name &
           ", " & Index_Clause_Name);

      Put_Line (Output_Object.File, "@section operators");
   end Index_Menu;

   ----------
   --  Public subprograms. Alphabetical order

   procedure AI_Reference
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      AI_Number     : in     String)
   is begin
      Ordinary_Text (Output_Object, AI_Number & Text);
   end AI_Reference;

   procedure Category_Header
     (Output_Object : in out Texinfo_Output_Type;
      Header_Text   :        String)
   is begin
      Check_Not_In_Paragraph (Output_Object);

      --  Can't be in a multi-column setting.
      --
      --  Don't use @heading; that causes a weird underline in info,
      --  that isn't centered!
      Put_Line (Output_Object.File, "@center @emph{" & Header_Text & "}");
      New_Line (Output_Object.File, 2);
   end Category_Header;

   procedure Clause_Header
     (Output_Object : in out Texinfo_Output_Type;
      Header_Text   : in     String;
      Level         : in     ARM_Contents.Level_Type;
      Clause_Number : in     String;
      No_Page_Break : in     Boolean                 := False)
   is
      pragma Unreferenced (No_Page_Break);
      Title : constant String := Clause_Number & " " & Header_Text;

      use ARM_Contents;

      Section_Number : Section_Number_Type;
      Clause_Integer : Natural;

      procedure Put_Clause_Menu_Item
        (Item_Title         : in     Title_Type;
         Item_Level         : in     Level_Type;
         Item_Clause_Number : in     Clause_Number_Type;
         Version            : in     ARM_Contents.Change_Version_Type;
         Quit               :    out Boolean)
      is
         pragma Unreferenced (Version); --  only version 2
         First_Part : String (1 .. 14); --  Get all Titles aligned.
      begin
         Quit := False;

         case Item_Level is
         when Section | Unnumbered_Section | Normative_Annex | Informative_Annex | Plain_Annex | Subclause | Subsubclause =>
            --  We are doing Clause here
            null;

         when Clause  =>
            if Item_Clause_Number.Section < Section_Number then
               null;

            elsif Item_Clause_Number.Section = Section_Number then
               Ada.Strings.Fixed.Move
                 (Source =>
                    "* " &
                    Make_Clause_Number (Item_Level, Item_Clause_Number) &
                    " ::",
                  Target => First_Part);

               Put_Line (Output_Object.File, First_Part & Item_Title);
            else
               Quit := True;
            end if;
         when Dead_Clause =>
            raise Program_Error; -- No dead clauses should be output.
         end case;
      end Put_Clause_Menu_Item;

      procedure Put_Clause_Menu is new For_Each (Put_Clause_Menu_Item);

      procedure Put_Subclause_Menu_Item
        (Item_Title         : in     Title_Type;
         Item_Level         : in     Level_Type;
         Item_Clause_Number : in     Clause_Number_Type;
         Version            : in     ARM_Contents.Change_Version_Type;
         Quit               :    out Boolean)
      is
         pragma Unreferenced (Version); --  only version 2
         First_Part : String (1 .. 14); --  Get all Titles aligned.
      begin
         Quit := False;

         case Item_Level is
            when Section | Unnumbered_Section | Normative_Annex | Informative_Annex | Plain_Annex | Clause | Subsubclause =>
               --  We are doing Subclause here
               null;

         when Subclause  =>
            if Item_Clause_Number.Section < Section_Number then
               null;

            elsif Item_Clause_Number.Section = Section_Number then
               if Item_Clause_Number.Clause < Clause_Integer then
                  null;

               elsif Item_Clause_Number.Clause = Clause_Integer then
                  Ada.Strings.Fixed.Move
                    (Source =>
                       "* " &
                       Make_Clause_Number (Item_Level, Item_Clause_Number) &
                       " ::",
                     Target => First_Part);

                  Put_Line (Output_Object.File, First_Part & Item_Title);
               else
                  Quit := True;
               end if;
            else
               Quit := True;
            end if;
         when Dead_Clause =>
            raise Program_Error; -- No dead clauses should be output.
         end case;
      end Put_Subclause_Menu_Item;

      procedure Put_Subclause_Menu is new For_Each (Put_Subclause_Menu_Item);

      function Safe_Next_Clause (Clause : in String) return String
      is begin
         if Clause = Index_Clause then
            return Index_Clause_Next;
         else
            declare
               Result : constant String := ARM_Contents.Next_Clause (Clause);
            begin
               if Result = Index_Clause then
                  return Index_Clause_Name;
               else
                  return Result;
               end if;
            end;
         end if;
      exception
      when Not_Found_Error =>
         return "";
      end Safe_Next_Clause;

      function Safe_Previous_Clause (Clause : in String) return String
      is begin
         return ARM_Contents.Previous_Clause (Clause);
      exception
      when Not_Found_Error =>
         return "";
      end Safe_Previous_Clause;

      function Safe_Parent_Clause (Clause : in String) return String
      is
         Temp : constant String := ARM_Contents.Parent_Clause (Clause_Number);
      begin
         if Temp'Length = 0 or Temp = "0" then
            return "Top";
         else
            return Temp;
         end if;
      end Safe_Parent_Clause;

   begin
      Check_Not_In_Paragraph (Output_Object);

      --  Handle special cases
      if Clause_Number = "" and Header_Text = "Table of Contents" then
         --  Actual contents output in TOC_Marker below.
         return;

      elsif Header_Text = "The Standard Libraries" then
         --  This section has no content; don't confuse makeinfo.
         return;

      elsif Clause_Number = Index_Clause and Header_Text = Index_Clause_Name then

         Put_Line
           (Output_Object.File,
            "@node " & Index_Clause_Name &
              ", " & Index_Clause_Next &
              ", " & Safe_Previous_Clause (Clause_Number) &
              ", " & Safe_Parent_Clause (Clause_Number));

         Put_Line (Output_Object.File, "@chapter Index");
         Output_Object.State := Index_Start;

         return;
      end if;

      case Level is
      when Section | Normative_Annex | Informative_Annex | Plain_Annex =>
         --  Menu of these done at @node Top
         null;

      when Unnumbered_Section =>
         --  Unnumbered sections are not in ARM_Contents, but there's
         --  currently only one of them, so they are not worth adding;
         --  just hard-code the menu here.
         Get_Clause_Section (Clause_Number, Section_Number, Clause_Integer);

         if Section_Number = 0 and Clause_Integer = 1 then
            Put_Line (Output_Object.File, "@menu");
            Put_Line (Output_Object.File, "* 0.1 :: Foreword to this version of the Ada Reference Manual");
            Put_Line (Output_Object.File, "* 0.2 :: Foreword");
            Put_Line (Output_Object.File, "* 0.3 :: Introduction");
            Put_Line (Output_Object.File, "* 0.99 :: International Standard");
            Put_Line (Output_Object.File, "@end menu");
         end if;

      when Clause =>
         --  Output menu of Clauses in this section, if we haven't already
         Get_Clause_Section (Clause_Number, Section_Number, Clause_Integer);

         if Output_Object.Menu_Section /= Section_Number then
            Put_Line (Output_Object.File, "@menu");
            Put_Clause_Menu;
            Put_Line (Output_Object.File, "@end menu");
            Output_Object.Menu_Section := Section_Number;
            Output_Object.Menu_Clause  := 0;
         end if;

      when Subclause =>
         --  Output menu of Subclauses in this Clause, if we haven't already
         Get_Clause_Section (Clause_Number, Section_Number, Clause_Integer);

         if Output_Object.Menu_Section = Section_Number and
           Output_Object.Menu_Clause /= Clause_Integer
         then
            Put_Line (Output_Object.File, "@menu");
            Put_Subclause_Menu;
            Put_Line (Output_Object.File, "@end menu");
            Output_Object.Menu_Clause := Clause_Integer;
         end if;

      when Subsubclause =>
         Put_Line (Output_Object.File, "FIXME: Clause_Header: Subsubclause");

      when Dead_Clause  =>
	 raise Program_Error; -- No headers for dead clauses.
      end case;

      Put_Line
        (Output_Object.File,
         "@node " & Clause_Number &
           ", " & Safe_Next_Clause (Clause_Number) &
           ", " & Safe_Previous_Clause (Clause_Number) &
           ", " & Safe_Parent_Clause (Clause_Number));

      case Level is
      when Section =>
         Put_Line (Output_Object.File, "@chapter " & Title);

      when Normative_Annex | Informative_Annex | Plain_Annex =>
         Put_Line (Output_Object.File, "@chapter " & Title);

      when Clause | Unnumbered_Section =>
         Put_Line (Output_Object.File, "@section " & Title);

      when Subclause =>
         Put_Line (Output_Object.File, "@subsection " & Title);

      when Subsubclause =>
         Put_Line (Output_Object.File, "@subsubsection " & Title);

      when Dead_Clause =>
         raise Program_Error; -- No output of dead clauses.
      end case;

   end Clause_Header;

   procedure Clause_Reference
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      Clause_Number : in     String)
   is begin
      case Output_Object.State is
      when Contents =>
         null;

      when Multi_Column | Table_Header =>
         --  If this happens, we need to store escaped text in columns.
         Ada.Exceptions.Raise_Exception
           (ARM_Output.Not_Valid_Error'Identity,
            "clause reference in multi-column");

      when Normal =>
         if Text = Clause_Number then
            Put
              (Output_Object.File,
               "@ref{" &
                 Clause_Number &
                 "}");
         else
            Put
              (Output_Object.File,
               "@ref{" &
                 Clause_Number &
                 "} " &
                 Text);
         end if;

      when Title | Index_Start | Index =>
         Unexpected_State (Output_Object);

      end case;
   end Clause_Reference;

   procedure Close (Output_Object : in out Texinfo_Output_Type)
   is begin
      Check_Valid (Output_Object);

      Put_Line (Output_Object.File, "@bye");

      Close (Output_Object.File);

      Output_Object.Is_Valid := False;
   end Close;

   procedure Create
     (Output_Object : in out Texinfo_Output_Type;
      File_Prefix   : in     String;
      Title         : in     String)
   is
      File_Name : constant String := Ada.Strings.Fixed.Trim (File_Prefix & ".texinfo", Ada.Strings.Right);
   begin
      if Output_Object.Is_Valid then
         Ada.Exceptions.Raise_Exception
           (ARM_Output.Not_Valid_Error'Identity,
            "Already valid object");
      end if;

      Output_Object.Is_Valid := True;

      Create (Output_Object.File, Out_File, File_Name);

      Put_Line (Output_Object.File, "\input texinfo");
      Put_Line (Output_Object.File, "@settitle " & Title);
      Put_Line (Output_Object.File, "@paragraphindent none");
      Put_Line (Output_Object.File, "@exampleindent" & Integer'Image (Indentation));

      Put_Line (Output_Object.File, "@node Top");
      Put_Line (Output_Object.File, "@top " & Title);

      Output_Object.State           := ARM_Texinfo.Title;
      Output_Object.First_Word_Last := 0;

   end Create;

   procedure DR_Reference
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      DR_Number     : in     String)
   is begin
      Ordinary_Text (Output_Object, DR_Number & Text);
   end DR_Reference;

   procedure End_Hang_Item (Output_Object : in out Texinfo_Output_Type)
   is
      use ARM_Output;
   begin
      -- *** Prob: Not sure if Paragraph_Indent plays any role here. Perhaps not.

      case Output_Object.Paragraph_Style is
      when
        Swiss_Examples | Small_Swiss_Examples =>
         Put_Line
           (Output_Object.File,
            "FIXME: End_Hang_Item: hanging? "& Paragraph_Style_Type'Image (Output_Object.Paragraph_Style));

      when Normal |
        Wide_Above |
        Small |
        Small_Wide_Above |
        Header |
        Small_Header |
        Index |
        Title |
        Syntax_Summary |
        Examples |
        Small_Examples |
        Bulleted |
        Nested_Bulleted |
        Small_Bulleted |
        Small_Nested_Bulleted =>

         null;

      when
	Wide_Hanging |
	Small_Wide_Hanging |
	Narrow_Hanging |
	Small_Narrow_Hanging |
        Hanging_in_Bulleted |
        Small_Hanging_in_Bulleted =>

         --  End of term in definition list; indent rest of paragraph.
         --  But sometimes we never get an "end_hang_item" in a
         --  hanging paragraph, so let End_Paragraph know we got one
         --  this time.
         Output_Object.End_Hang_Seen := True;

         New_Line (Output_Object.File);
         Put_Line (Output_Object.File, "@quotation");

      when Enumerated |
        Small_Enumerated =>

         --  Number has just been output; start text.
         Put (Output_Object.File, "@w{  }");

      end case;

   end End_Hang_Item;

   procedure End_Paragraph (Output_Object : in out Texinfo_Output_Type)
   is
      use ARM_Output;
   begin
      Output_Object.In_Paragraph := False;

      case Output_Object.State is
      when Contents =>
         null;

      when Multi_Column =>
         --  Skip a row, to separate paragraphs in a column.
         Output_Object.Current_Row := Output_Object.Current_Row + 2;

      when Title =>
         if Output_Object.Line_Empty then
            null;
         else
            New_Line (Output_Object.File, 2);
            Put (Output_Object.File, "@center ");
            Output_Object.Line_Empty := True;
         end if;

      when Normal =>
         -- *** Prob: Paragraph_Indent is not taken into account here,

         case Output_Object.Paragraph_Style is
         when
           Swiss_Examples | Small_Swiss_Examples | Title =>
            Put_Line
              (Output_Object.File, "FIXME : End_Paragraph: " & Paragraph_Style_Type'Image (Output_Object.Paragraph_Style));

         when Normal |
           Wide_Above |
           Header |
           Small |
           Small_Wide_Above |
           Small_Header |
           Syntax_Summary |
           Index =>

            New_Line (Output_Object.File, 2);

         when Examples |
           Small_Examples =>

            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@end example");
            New_Line (Output_Object.File);

         when Bulleted |
           Small_Bulleted =>

            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@end itemize");
            New_Line (Output_Object.File);

         when Nested_Bulleted |
           Small_Nested_Bulleted =>

            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@end itemize");
            Put_Line (Output_Object.File, "@end itemize");
            New_Line (Output_Object.File);

         when Wide_Hanging |
           Small_Wide_Hanging |
           Narrow_Hanging |
           Small_Narrow_Hanging |
           Hanging_in_Bulleted |
           Small_Hanging_in_Bulleted =>

            New_Line (Output_Object.File);
            if Output_Object.End_Hang_Seen then
               Put_Line (Output_Object.File, "@end quotation");
            end if;
            New_Line (Output_Object.File);

         when Enumerated |
           Small_Enumerated =>

            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@end itemize");
            New_Line (Output_Object.File);
         end case;

      when Index_Start =>
         Output_Object.State := Index;

         Index_Menu (Output_Object);

      when Index =>
         --  Keep index items tightly grouped.
         Put_Line (Output_Object.File, "@*");

      when Table_Header =>
         Unexpected_State (Output_Object);

      end case;
   end End_Paragraph;

   procedure Hard_Space (Output_Object : in out Texinfo_Output_Type)
   is begin
      case Output_Object.State is
      when Contents =>
         null;

      when Multi_Column | Table_Header =>
         --  Can't do line breaks in columns
         Add_To_Column_Item (Output_Object, " ");

      when Title =>
         if Output_Object.Line_Empty then
            null;
         else
            Put (Output_Object.File, "@w{ }");
         end if;

      when Normal | Index_Start | Index =>
         Put (Output_Object.File, "@w{ }");
      end case;
   end Hard_Space;

   procedure Index_Line_Break
     (Output_Object        : in out Texinfo_Output_Type;
      Clear_Keep_with_Next : in     Boolean)
   is
      pragma Unreferenced (Clear_Keep_with_Next);
   begin
      Put_Line (Output_Object.File, "@*");
   end Index_Line_Break;

   procedure Index_Reference
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      Index_Key     : in     Natural;
      Clause_Number : in     String)
   is
      pragma Unreferenced (Clause_Number);
      --  Text is clause_number & paragraph number (optional).
   begin
      Put (Output_Object.File, "@ref{" & Integer'Image (Index_Key) & ", " & Text & "}");
   end Index_Reference;

   procedure Index_Target
     (Output_Object : in out Texinfo_Output_Type;
      Index_Key     : in     Natural)
   is begin
      Put (Output_Object.File, "@anchor{" & Integer'Image (Index_Key) & "}");
   end Index_Target;

   procedure Line_Break (Output_Object : in out Texinfo_Output_Type)
   is
      use ARM_Output;
   begin
      case Output_Object.State is
      when Title =>
         if Output_Object.Line_Empty then
            null;
         else
            Put_Line (Output_Object.File, "@*");
            Output_Object.Line_Empty := True;
         end if;

      when Contents =>
         null;

      when Multi_Column | Table_Header =>
         Output_Object.Current_Row := Output_Object.Current_Row + 1;
         if Output_Object.Max_Row < Output_Object.Current_Row then
            Output_Object.Max_Row := Output_Object.Current_Row;
         end if;

      when Index_Start =>
         --  This doesn't happen
         Put_Line (Output_Object.File, "FIXME: Line_Break Index_Start");

      when Normal | Index =>
         case Output_Object.Paragraph_Style is
         when
           Swiss_Examples | Small_Swiss_Examples =>
            Put_Line
              (Output_Object.File, "FIXME: Line_Break: "& Paragraph_Style_Type'Image (Output_Object.Paragraph_Style));

         when Normal |
           Wide_Above |
           Small |
           Small_Wide_Above |
           Header |
           Small_Header |
           Index |
           Title |
           Syntax_Summary |
           Bulleted |
           Nested_Bulleted |
           Small_Bulleted |
           Small_Nested_Bulleted |
           Wide_Hanging |
           Small_Wide_Hanging |
           Narrow_Hanging |
           Small_Narrow_Hanging |
           Hanging_in_Bulleted |
           Small_Hanging_in_Bulleted |
           Enumerated |
           Small_Enumerated =>

            Put_Line (Output_Object.File, "@*");

         when Examples |
           Small_Examples =>

            New_Line (Output_Object.File);

         end case;

      end case;
   end Line_Break;

   procedure Local_Link
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      Target        : in     String;
      Clause_Number : in     String)
   is
      pragma Unreferenced (Target);
      pragma Unreferenced (Clause_Number);
   begin
      --  These are typically references to words in the grammar
      --  summary. Mildly useful, but the best we can do is:
      --
      --  "@ref{" & Target & "," & Text & "}"
      --
      --  makeinfo prepends 'see' and postpends '.', so it screws up
      --  the text. For example, section 2.1 (1) ends up with "the
      --  @ref{S0229, compilation}s." => "the see compilation: S0229."
      --  Emacs info-mode suppresses the ': S0229', but not the 'see'
      --  and the trailing '.'. So we just output the text.
      Ordinary_Text (Output_Object, Text);
   end Local_Link;

   procedure Local_Link_End
     (Output_Object : in out Texinfo_Output_Type;
      Target        : in     String;
      Clause_Number : in     String)
   is begin
      --  These work better than local links, because they are not in
      --  the middle of plurals. First use is section 3.1 (1).
      Put (Output_Object.File, " (@pxref{" & Target & "," & Clause_Number & "})");
   end Local_Link_End;

   procedure Local_Link_Start
     (Output_Object : in out Texinfo_Output_Type;
      Target        : in     String;
      Clause_Number : in     String)
   is
      pragma Unreferenced (Output_Object);
      pragma Unreferenced (Target);
      pragma Unreferenced (Clause_Number);
   begin
      --  implemented in Local_Link_End
      null;
   end Local_Link_Start;

   procedure Local_Target
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      Target        : in     String)
   is begin
      Put (Output_Object.File, "@anchor{" & Target & "}");
      Ordinary_Text (Output_Object, Text);
   end Local_Target;

   procedure New_Column (Output_Object : in out Texinfo_Output_Type)
   is begin
      if Output_Object.Column_Count >= 4 then
         Output_Object.Current_Column := Output_Object.Current_Column + 1;
         Output_Object.Current_Row    := 1;
      end if;
   end New_Column;

   procedure New_Page
     (Output_Object : in out Texinfo_Output_Type;
      Kind          :        ARM_Output.Page_Kind_Type := ARM_Output.Any_Page)
   is
      pragma Unreferenced (Kind);
      pragma Unreferenced (Output_Object);
   begin
      --  No such thing in Info.
      null;
   end New_Page;

   procedure Ordinary_Character
     (Output_Object : in out Texinfo_Output_Type;
      Char          : in     Character)
   is
      Copyright : constant String := "Copyright";
   begin
      case Output_Object.State is
      when Contents =>
         null;

      when Multi_Column | Table_Header =>
         Add_To_Column_Item (Output_Object, "" & Char);

      when Title =>
         --  Check for end of title page; indicated by line starting with "Copyright"
         if Output_Object.Line_Empty then
            if Output_Object.First_Word_Last > 0 then
               if Copyright (Output_Object.First_Word_Last + 1) = Char then
                  Output_Object.First_Word_Last := Output_Object.First_Word_Last + 1;
                  Output_Object.First_Word (Output_Object.First_Word_Last) := Char;

                  if Output_Object.First_Word_Last = Copyright'Last then
                     End_Title_Page (Output_Object);
                     Output_Object.State := Normal;
                     Ordinary_Text (Output_Object, Output_Object.First_Word (1 .. Output_Object.First_Word_Last));
                  end if;
               else
                  --  First word is not Copyright; output it
                  Ordinary_Text (Output_Object, Output_Object.First_Word (1 .. Output_Object.First_Word_Last));
                  Output_Object.Line_Empty := False;
               end if;
            else
               --  No non-space seen yet
               if Char = ' ' then
                  null;
               elsif Char = Copyright (1) then
                  Output_Object.First_Word_Last := 1;
                  Output_Object.First_Word (1)  := Char;
               else
                  Escape_Put (Output_Object, Char);
                  Output_Object.Line_Empty := False;
               end if;
            end if;
         else
            --  Line already has stuff on it
            Escape_Put (Output_Object, Char);
         end if;

      when Normal =>
         Output_Object.Line_Empty := Char /= ' ';

         Escape_Put (Output_Object, Char);

      when Index_Start =>
         Escape_Put (Output_Object, Char);
         if Char = '&' then
            --  give debugger a place to break
            Put_Line ("first index entry");
         end if;

      when Index =>
         case Char is
         when ' ' | ',' | '[' | ']' =>
            Put (Output_Object.File, Char);

         when 'A' .. Last_Index_Clause =>
            --  Index section heading

            --  @node current, next, prev, up
            case Char is
            when 'A' =>
               Put_Line
                 (Output_Object.File,
                  "@node " & Char &
                    ", B, " & Operators_Clause &
                    ", " & Index_Clause_Name);

            when Last_Index_Clause =>
               Put_Line
                 (Output_Object.File,
                  "@node " & Char &
                    ", , " & Character'Pred (Char) &
                    ", " & Index_Clause_Name);

            when others =>
               Put_Line
                 (Output_Object.File,
                  "@node " & Char &
                    ", " & Character'Succ (Char) &
                    ", " & Character'Pred (Char) &
                    ", " & Index_Clause_Name);
            end case;

            --  Add non-break space so Emacs info will use big bold
            --  font for single letter titles.
            Put_Line (Output_Object.File, "@section " & Char & "@w{ }");

         when others =>
            Ada.Exceptions.Raise_Exception (ARM_Output.Not_Valid_Error'Identity, "Unexpected char in Index: " & Char);
         end case;
      end case;
   end Ordinary_Character;

   procedure Ordinary_Text
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String)
   is begin
      case Output_Object.State is
      when Contents =>
         null;

      when Multi_Column | Table_Header =>
         Add_To_Column_Item (Output_Object, Text);

      when Normal | Title | Index_Start | Index =>
         Output_Object.Line_Empty := False;

         Escape_Put (Output_Object, Text);
      end case;
   end Ordinary_Text;

   procedure Picture
     (Output_Object : in out Texinfo_Output_Type;
      Name          : in     String;
      Descr         : in     String;
      Alignment     : in     ARM_Output.Picture_Alignment;
      Height, Width : in     Natural;
      Border        : in     ARM_Output.Border_Kind)
   is
      pragma Unreferenced (Border);
      pragma Unreferenced (Width);
      pragma Unreferenced (Height);
      pragma Unreferenced (Alignment);
      pragma Unreferenced (Name);
   begin
      Put_Line (Output_Object.File, "FIXME: Picture: " & Descr);
   end Picture;

   procedure Revised_Clause_Header
     (Output_Object   : in out Texinfo_Output_Type;
      New_Header_Text : in     String;
      Old_Header_Text : in     String;
      Level           : in     ARM_Contents.Level_Type;
      Clause_Number   : in     String;
      Version         : in     ARM_Contents.Change_Version_Type;
      Old_Version     : in     ARM_Contents.Change_Version_Type;
      No_Page_Break   : in     Boolean                          := False)
   is
      pragma Unreferenced (Version);
      pragma Unreferenced (Old_Version);
      pragma Unreferenced (Old_Header_Text);
   begin
      Clause_Header (Output_Object, New_Header_Text, Level, Clause_Number, No_Page_Break);
   end Revised_Clause_Header;

   procedure Section
     (Output_Object : in out Texinfo_Output_Type;
      Section_Title : in     String;
      Section_Name  : in     String)
   is
      pragma Unreferenced (Section_Name);
      pragma Unreferenced (Section_Title);
      pragma Unreferenced (Output_Object);
   begin
      --  This is redundant with the various Clause functions
      null;
   end Section;

   procedure Separator_Line
     (Output_Object : in out Texinfo_Output_Type;
      Is_Thin       :        Boolean             := True)
   is begin
      --  Can't be in a multi-column setting.
      New_Line (Output_Object.File);
      if Is_Thin then
         Put_Line (Output_Object.File, "----------");
      else
         Put_Line (Output_Object.File, "==========");
      end if;
   end Separator_Line;

   procedure Set_Columns
     (Output_Object     : in out Texinfo_Output_Type;
      Number_of_Columns : in     ARM_Output.Column_Count)
   is begin
      Check_Valid (Output_Object);
      Check_Not_In_Paragraph (Output_Object);

      --  2 and 3 column formats are displayed without any columns.
      --  This is mainly used for the syntax cross-reference and
      --  index, and these definitely look better without columns.
      --
      --  4 or more columns are output as a table. Note that we assume
      --  such items are formated with explicit New_Column calls, and
      --  do not contain any nested paragraph formats.

      case Output_Object.State is
      when Normal =>
         if Number_of_Columns >= 4 then
            Output_Object.State          := Multi_Column;
            Output_Object.Current_Column := 1;
            Output_Object.Current_Row    := 1;
            Output_Object.Column_Widths  := (others => 0);

            --  Accumulate all column rows in Output_Text, then output
            --  when done, so we can set the correct column width in
            --  the header. Each column is a linked list of allocated
            --  Column_Text_Item_Type.
         else
            null;
         end if;

      when Multi_Column =>
         if Number_of_Columns = 1 then
            --  Finished accumulating columns, output the columns as a table.
            Output_Column_Widths (Output_Object);
            Output_Columns (Output_Object);
            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@end multitable");
            New_Line (Output_Object.File);

            Output_Object.State := Normal;
         else
            Ada.Exceptions.Raise_Exception
              (ARM_Output.Not_Valid_Error'Identity, "New multi-column section before end of old");
         end if;

      when Index_Start | Index =>
         null;

      when Table_Header | Contents | Title =>
         Unexpected_State (Output_Object);
      end case;

      Output_Object.Column_Count := Number_of_Columns;
   end Set_Columns;

   procedure Soft_Hyphen_Break (Output_Object : in out Texinfo_Output_Type)
   is begin
      Put (Output_Object.File, "@-");
   end Soft_Hyphen_Break;

   procedure Soft_Line_Break (Output_Object : in out Texinfo_Output_Type)
   is begin
      case Output_Object.State is
      when Contents | Title =>
         null;

      when Normal | Index_Start | Index =>
         Put (Output_Object.File, "@-");

      when Multi_Column | Table_Header =>
         Unexpected_State (Output_Object);

      end case;
   end Soft_Line_Break;

   procedure Special_Character
     (Output_Object : in out Texinfo_Output_Type;
      Char          : in     ARM_Output.Special_Character_Type)
   is begin
      --  We use Ordinary_Text, so this is output to columns when appropriate.
      case Char is
      when ARM_Output.EM_Dash =>
         Ordinary_Text (Output_Object, "--");
      when ARM_Output.EN_Dash =>
         Ordinary_Text (Output_Object, "--");
      when ARM_Output.GEQ =>
         Ordinary_Text (Output_Object, ">=");
      when ARM_Output.LEQ =>
         Ordinary_Text (Output_Object, "<=");
      when ARM_Output.NEQ =>
         Ordinary_Text (Output_Object, "/=");
      when ARM_Output.PI =>
         Ordinary_Text (Output_Object, "PI");

      when ARM_Output.Left_Ceiling =>
         case Output_Object.State is
         when Multi_Column | Table_Header =>
            Ada.Exceptions.Raise_Exception
              (ARM_Output.Not_Valid_Error'Identity,
               "Info does not support ceiling in multi-column");
         when Contents =>
            null;

         when Normal | Index_Start | Index =>
            Put (Output_Object.File, "@code{ceiling(");

         when Title =>
            Unexpected_State (Output_Object);

         end case;

      when ARM_Output.Right_Ceiling =>
         case Output_Object.State is
         when Multi_Column | Table_Header =>
            Ada.Exceptions.Raise_Exception
              (ARM_Output.Not_Valid_Error'Identity,
               "Info does not support ceiling in multi-column");
         when Contents =>
            null;

         when Normal | Index_Start | Index =>
            Put (Output_Object.File, ")}");

         when Title =>
            Unexpected_State (Output_Object);

         end case;

      when ARM_Output.Left_Floor =>
         case Output_Object.State is
         when Multi_Column | Table_Header =>
            Ada.Exceptions.Raise_Exception
              (ARM_Output.Not_Valid_Error'Identity,
               "Info does not support floor in multi-column");
         when Contents =>
            null;

         when Normal | Index_Start | Index =>
            Put (Output_Object.File, "@code{floor(");

         when Title =>
            Unexpected_State (Output_Object);

         end case;

      when ARM_Output.Right_Floor =>
         case Output_Object.State is
         when Multi_Column | Table_Header =>
            Ada.Exceptions.Raise_Exception
              (ARM_Output.Not_Valid_Error'Identity,
               "Info does not support floor in multi-column");
         when Contents =>
            null;

         when Normal | Index_Start | Index =>
            Put (Output_Object.File, ")}");

         when Title =>
            Unexpected_State (Output_Object);

         end case;

      when ARM_Output.Thin_Space =>
         Ordinary_Text (Output_Object, " ");

      when ARM_Output.Left_Quote =>
         Ordinary_Text (Output_Object, "`");

      when ARM_Output.Right_Quote =>
         Ordinary_Text (Output_Object, "'");

      when ARM_Output.Left_Double_Quote =>
         Ordinary_Text (Output_Object, """");

      when ARM_Output.Right_Double_Quote =>
         Ordinary_Text (Output_Object, """");

      when ARM_Output.Small_Dotless_I =>
         Ordinary_Text (Output_Object, "i");

      when ARM_Output.Capital_Dotted_I =>
         Ordinary_Text (Output_Object, "I");
      end case;
   end Special_Character;

   procedure Start_Paragraph
     (Output_Object  : in out Texinfo_Output_Type;
      Style          : in     ARM_Output.Paragraph_Style_Type;
      Indent         : in     ARM_Output.Paragraph_Indent_Type;
      Number         : in     String;
      No_Prefix      : in     Boolean                       := False;
      Tab_Stops      : in     ARM_Output.Tab_Info           := ARM_Output.NO_TABS;
      No_Breaks      : in     Boolean                       := False;
      Keep_with_Next : in     Boolean                       := False;
      Space_After    : in     ARM_Output.Space_After_Type   := ARM_Output.Normal;
      Justification  : in     ARM_Output.Justification_Type := ARM_Output.Default)
   is
      pragma Unreferenced (Justification);
      pragma Unreferenced (Space_After);
      pragma Unreferenced (Keep_with_Next);
      pragma Unreferenced (No_Breaks);
      pragma Unreferenced (Tab_Stops);

      use ARM_Output;

   begin
      Check_Valid (Output_Object);
      Check_Not_In_Paragraph (Output_Object);

      --  Note: makeinfo will do most of the formatting, so No_Breaks,
      --  Keep_with_Next, Space_After, and Justification have no
      --  effect here. In addition, info format has no support for
      --  fonts, so the font aspects of Format is ignored as well. But
      --  we try to respect the indentation and margin aspects.

      --  TexInfo does not directly support tabs, but does use a fixed
      --  font, so we could emulate them. But then we'd have to track
      --  output characters; let's see if we really need it.

      case Output_Object.State is
      when Contents =>
         null;

      when Normal =>
         if Number'Length > 0 then
            Put_Line (Output_Object.File, Number & " @*");
         end if;

         Output_Object.In_Paragraph     := True;
         Output_Object.Paragraph_Style  := Style;
         Output_Object.Paragraph_Indent := Indent;

         -- *** Prob: The old Format parameter has been replaced by Style and
         -- *** Indent (to cut down on the number of related formats), and
         -- *** I don't know Texinfo well enough to have any idea how to handle
         -- *** Indents in general. So Indent is ignored for now. Hopefully
         -- *** someone will be able to fix this.
	 -- *** Note: Some of the formats that no longer exist used
         -- *** @quotation for indentation. Not sure that's general enough.
         if Indent /= 0 then
            Put_Line (Output_Object.File, "FIXME: Start_Paragraph Indent=" & Paragraph_Indent_Type'Image (Indent));
            -- Should be Put_Line (Output_Object.File, "@quotation"); ???
         end if;

         case Style is
         when
           Swiss_Examples | Small_Swiss_Examples | Title =>
            Put_Line (Output_Object.File, "FIXME: Start_Paragraph " & ARM_Output.Paragraph_Style_Type'Image (Style));

         when Normal |
           Wide_Above |
           Header |
           Index |
           Small |
           Small_Wide_Above |
           Small_Header |
           Syntax_Summary =>

            null;

         when Examples |
           Small_Examples =>

            Put_Line (Output_Object.File, "@example");

         when Bulleted |
           Small_Bulleted =>

            Put_Line (Output_Object.File, "@itemize @bullet");
            if not No_Prefix then
               Put (Output_Object.File, "@item ");
            end if;

         when Nested_Bulleted |
           Small_Nested_Bulleted =>

            Put_Line (Output_Object.File, "@itemize @bullet");
            Put_Line (Output_Object.File, "@itemize @bullet");
            if not No_Prefix then
               Put (Output_Object.File, "@item ");
            end if;

         when Wide_Hanging |
           Small_Wide_Hanging |
           Narrow_Hanging |
           Small_Narrow_Hanging |
           Hanging_in_Bulleted |
           Small_Hanging_in_Bulleted =>

            if No_Prefix then
               --  Still in hanging part
               Put_Line (Output_Object.File, "@quotation");
               Output_Object.End_Hang_Seen := True;
            else
               Output_Object.End_Hang_Seen := False;
            end if;

         when Enumerated |
           Small_Enumerated =>

            Put_Line (Output_Object.File, "@itemize @w{}");
            Put (Output_Object.File, "@item ");

         end case;

      when Index_Start | Index | Title | Multi_Column | Table_Header =>
         if Number'Length > 0 then
            Unexpected_State (Output_Object);
         end if;

         Output_Object.In_Paragraph     := True;
         Output_Object.Paragraph_Style  := Style;
         Output_Object.Paragraph_Indent := Indent;

      end case;

   end Start_Paragraph;

   procedure Start_Table
     (Output_Object      : in out Texinfo_Output_Type;
      Columns            : in     ARM_Output.Column_Count;
      First_Column_Width : in     ARM_Output.Column_Count;
      Last_Column_Width  : in     ARM_Output.Column_Count;
      Alignment          : in     ARM_Output.Column_Text_Alignment;
      No_Page_Break      : in     Boolean;
      Has_Border         : in     Boolean;
      Small_Text_Size    : in     Boolean;
      Header_Kind        : in     ARM_Output.Header_Kind_Type)
   is
      pragma Unreferenced (Small_Text_Size);
      pragma Unreferenced (Has_Border);
      pragma Unreferenced (No_Page_Break);
      pragma Unreferenced (Alignment);
      pragma Unreferenced (Last_Column_Width);
      pragma Unreferenced (First_Column_Width);
      use ARM_Output;
   begin
      Output_Object.Column_Count := Columns;
      case Header_Kind is
      when Both_Caption_and_Header =>
         New_Line (Output_Object.File);
         --  Next text output will be the caption, which we don't
         --  format in any special way (first example is F.3.2 (19)).
         --  Then Table_Marker (End_Caption) is called, which will
         --  start the actual table.

      when Header_Only =>
         --  Same as Table_Marker, End_Caption.
         case Columns is
         when 1 =>
            Ada.Exceptions.Raise_Exception
              (ARM_Output.Not_Valid_Error'Identity,
               "Table with 1 column");

         when 2 =>
            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@table @asis");

         when others =>
            New_Line (Output_Object.File);
            Put (Output_Object.File, "@multitable");
            Output_Object.State          := Table_Header;
            Output_Object.Current_Column := 1;
            Output_Object.Current_Row    := 1;
            Output_Object.Max_Row        := 0;
            --  The next text output via Ordinary_Text or
            --  Ordinary_Character is the table headers. We
            --  capture them in Output_Object.Column_Text, and
            --  use them to set the table column widths.
         end case;

      when No_Headers =>
         null;

      end case;
   end Start_Table;

   procedure Tab (Output_Object : in out Texinfo_Output_Type)
   is begin
      case Output_Object.State is
      when Contents =>
         null;

      when Multi_Column | Table_Header =>
         Ada.Exceptions.Raise_Exception
           (ARM_Output.Not_Valid_Error'Identity,
            "Tab in multi-column");

      when Title =>
         if Output_Object.Line_Empty then
            null;
         else
            Put (Output_Object.File, "@w{ }");
         end if;

      when Normal | Index_Start | Index =>
         --  Just three spaces for now, for indented trees
         Put (Output_Object.File, "@w{   }");

      end case;
   end Tab;

   procedure Table_Marker
     (Output_Object : in out Texinfo_Output_Type;
      Marker        : in     ARM_Output.Table_Marker_Type)
   is begin
      case Marker is
      when ARM_Output.End_Caption =>
         --  Start the actual table
         case Output_Object.Column_Count is
         when 1 =>
            Ada.Exceptions.Raise_Exception
              (ARM_Output.Not_Valid_Error'Identity,
               "Table with 1 column");

         when 2 =>
            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@table @asis");

         when others =>
            New_Line (Output_Object.File);
            Put (Output_Object.File, "@multitable");
            Output_Object.State          := Table_Header;
            Output_Object.Current_Column := 1;
            Output_Object.Current_Row    := 1;
            Output_Object.Max_Row        := 0;
            --  The next text output via Ordinary_Text or
            --  Ordinary_Character is the table headers. We
            --  capture them in Output_Object.Column_Text, and
            --  use them to set the table column widths.
         end case;

      when ARM_Output.End_Item =>
         case Output_Object.State is
         when Table_Header =>
            Output_Object.Current_Column := Output_Object.Current_Column + 1;
            Output_Object.Current_Row    := 1;

         when Normal =>
            case Output_Object.Column_Count is
            when 2 =>
               --  using @table
               Put (Output_Object.File, ' ');

            when others =>
               Put (Output_Object.File, " @tab ");
            end case;

         when Multi_Column | Contents | Title | Index_Start | Index =>
            Unexpected_State (Output_Object);
         end case;

      when ARM_Output.End_Header =>
         case Output_Object.State is
         when Table_Header =>
            Output_Object.State := Normal;

            for I in 1 .. Output_Object.Column_Count loop
               Put
                 (Output_Object.File,
                  " {" &
                    Output_Object.Column_Text (I).Text (1 .. Output_Object.Column_Text (I).Length) &
                    "}");
            end loop;

            New_Line (Output_Object.File);

            Put (Output_Object.File, "@item ");

            Pad_Columns (Output_Object);
            Output_Columns (Output_Object);
            New_Line (Output_Object.File);
            Put (Output_Object.File, "@item ");
            Output_Object.Current_Column := 1;

         when Normal =>
            --  A two-column table; header has been output
            null;

         when Contents | Multi_Column | Title | Index_Start | Index =>
            Unexpected_State (Output_Object);
         end case;

      when ARM_Output.End_Row | ARM_Output.End_Row_Next_Is_Last =>
         New_Line (Output_Object.File);
         Put (Output_Object.File, "@item ");
         Output_Object.Current_Column := 1;

      when ARM_Output.End_Table =>
         case Output_Object.Column_Count is
         when 2 =>
            New_Line (Output_Object.File);
            Put_Line (Output_Object.File, "@end table");

         when others =>
            Put_Line (Output_Object.File, "@end multitable");

         end case;

      end case;
   end Table_Marker;

   procedure Text_Format
     (Output_Object : in out Texinfo_Output_Type;
      Format        : in     ARM_Output.Format_Type)
   is
      pragma Unreferenced (Format);
   begin
      --  Info does not support formats; Emacs info-mode font-lock
      --  does some, but doesn't need any help from here.
      null;
   end Text_Format;

   procedure TOC_Marker
     (Output_Object : in out Texinfo_Output_Type;
      For_Start     : in     Boolean)
   is begin
      --  We use menus, not @contents (since makeinfo ignores
      --  @contents in info mode). The menus (including the top menu)
      --  are generated from data stored in ARM_Contents during the
      --  scan pass.

      if For_Start then
         Output_Object.State := Contents;
         --  Ignore futher output until For_Start = False.
      else
         Output_Object.State := Normal;
      end if;
   end TOC_Marker;

   procedure Unicode_Character
     (Output_Object : in out Texinfo_Output_Type;
      Char          : in     ARM_Output.Unicode_Type)
   is begin
      --  Used in section 2.3 Identifiers examples, 2.5 character
      --  literals examples, 2.6 string literals examples, 3.3.1
      --  Object Declarations examples, 4.4 Expressions examples
      Put_Line (Output_Object.File, "[Unicode" & ARM_Output.Unicode_Type'Image (Char) & "]");
   end Unicode_Character;

   procedure URL_Link
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      URL           : in     String)
   is begin
      Put (Output_Object.File, "@uref{" & URL & "," & Text & "}");
   end URL_Link;

end ARM_Texinfo;
