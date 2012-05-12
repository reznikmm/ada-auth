with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with ARM_Output;
with ARM_Contents;
package ARM_Texinfo is

   --
   --  Ada reference manual formatter.
   --
   --  This package defines the TEXINFO output object.
   --  Output objects are responsible for implementing the details of
   --  a particular format.
   --
   -- ---------------------------------------
   --
   --  Copyright (C) 2003, 2007, 2011 Stephen Leake.  All Rights Reserved.
   --  E-Mail: stephen_leake@stephe-leake.org
   --
   --  This library is free software; you can redistribute it and/or
   --  modify it under terms of the GNU General Public License as
   --  published by the Free Software Foundation; either version 3, or (at
   --  your option) any later version. This library is distributed in the
   --  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
   --  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   --  PURPOSE. See the GNU General Public License for more details. You
   --  should have received a copy of the GNU General Public License
   --  distributed with this program; see file gnu-3-0.txt. If not, write to
   --  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   --  MA 02111-1307, USA.
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
   --  4/ 1/12 - S L - Various revisions.

   type Texinfo_Output_Type is new ARM_Output.Output_Type with private;

   --not overriding - Ada 2005-only
   procedure Create
     (Output_Object : in out Texinfo_Output_Type;
      File_Prefix   : in     String;
      Title         : in     String);
   --  Create an Output_Object for a document.

   -- overriding - Ada 2005-only
   procedure Close (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Section
     (Output_Object : in out Texinfo_Output_Type;
      Section_Title : in     String;
      Section_Name  : in     String);

   -- overriding
   procedure Set_Columns
     (Output_Object     : in out Texinfo_Output_Type;
      Number_of_Columns : in     ARM_Output.Column_Count);

   -- overriding
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
      Justification  : in     ARM_Output.Justification_Type := ARM_Output.Default);

   -- overriding
   procedure End_Paragraph (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Category_Header
     (Output_Object : in out Texinfo_Output_Type;
      Header_Text   :        String);

   -- overriding
   procedure Clause_Header
     (Output_Object : in out Texinfo_Output_Type;
      Header_Text   : in     String;
      Level         : in     ARM_Contents.Level_Type;
      Clause_Number : in     String;
      No_Page_Break : in     Boolean                 := False);

   -- overriding
   procedure Revised_Clause_Header
     (Output_Object   : in out Texinfo_Output_Type;
      New_Header_Text : in     String;
      Old_Header_Text : in     String;
      Level           : in     ARM_Contents.Level_Type;
      Clause_Number   : in     String;
      Version         : in     ARM_Contents.Change_Version_Type;
      Old_Version     : in     ARM_Contents.Change_Version_Type;
      No_Page_Break   : in     Boolean                          := False);

   -- overriding
   procedure TOC_Marker (Output_Object : in out Texinfo_Output_Type;
                         For_Start : in Boolean);

   -- overriding
   procedure New_Page (Output_Object : in out Texinfo_Output_Type;
                       Kind : ARM_Output.Page_Kind_Type := ARM_Output.Any_Page);

   -- overriding
   procedure New_Column (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Separator_Line (Output_Object : in out Texinfo_Output_Type;
                             Is_Thin : Boolean := True);

   -- overriding
   procedure Start_Table
     (Output_Object      : in out Texinfo_Output_Type;
      Columns            : in     ARM_Output.Column_Count;
      First_Column_Width : in     ARM_Output.Column_Count;
      Last_Column_Width  : in     ARM_Output.Column_Count;
      Alignment          : in     ARM_Output.Column_Text_Alignment;
      No_Page_Break      : in     Boolean;
      Has_Border         : in     Boolean;
      Small_Text_Size    : in     Boolean;
      Header_Kind        : in     ARM_Output.Header_Kind_Type);

   -- overriding
   procedure Table_Marker (Output_Object : in out Texinfo_Output_Type;
                           Marker : in ARM_Output.Table_Marker_Type);

   -- overriding
   procedure Ordinary_Text (Output_Object : in out Texinfo_Output_Type;
                            Text : in String);

   -- overriding
   procedure Ordinary_Character (Output_Object : in out Texinfo_Output_Type;
                                 Char : in Character);

   -- overriding
   procedure Hard_Space (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Line_Break (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Index_Line_Break (Output_Object : in out Texinfo_Output_Type;
                               Clear_Keep_with_Next : in Boolean);

   -- overriding
   procedure Soft_Line_Break (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Soft_Hyphen_Break (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Tab (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Special_Character
     (Output_Object : in out Texinfo_Output_Type;
      Char          : in     ARM_Output.Special_Character_Type);

   -- overriding
   procedure Unicode_Character
     (Output_Object : in out Texinfo_Output_Type;
      Char          : in     ARM_Output.Unicode_Type);

   -- overriding
   procedure End_Hang_Item (Output_Object : in out Texinfo_Output_Type);

   -- overriding
   procedure Text_Format
     (Output_Object : in out Texinfo_Output_Type;
      Format        : in     ARM_Output.Format_Type);

   -- overriding
   procedure Clause_Reference (Output_Object : in out Texinfo_Output_Type;
                               Text : in String;
                               Clause_Number : in String);

   -- overriding
   procedure Index_Target
     (Output_Object : in out Texinfo_Output_Type;
      Index_Key     : in     Natural);

   -- overriding
   procedure Index_Reference
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      Index_Key     : in     Natural;
      Clause_Number : in     String);

   -- overriding
   procedure DR_Reference
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      DR_Number     : in     String);

   -- overriding
   procedure AI_Reference
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      AI_Number     : in     String);

   -- overriding
   procedure Local_Target
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      Target        : in     String);

   -- overriding
   procedure Local_Link
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      Target        : in     String;
      Clause_Number : in     String);

   -- overriding
   procedure Local_Link_Start
     (Output_Object : in out Texinfo_Output_Type;
      Target        : in     String;
      Clause_Number : in     String);

   -- overriding
   procedure Local_Link_End
     (Output_Object : in out Texinfo_Output_Type;
      Target        : in     String;
      Clause_Number : in     String);

   -- overriding
   procedure URL_Link
     (Output_Object : in out Texinfo_Output_Type;
      Text          : in     String;
      URL           : in     String);

   -- overriding
   procedure Picture
     (Output_Object : in out Texinfo_Output_Type;
      Name          : in     String;
      Descr         : in     String;
      Alignment     : in     ARM_Output.Picture_Alignment;
      Height, Width : in     Natural;
      Border        : in     ARM_Output.Border_Kind);

private

   subtype Column_Index_Type is Integer range 1 .. 5;
   type Column_Text_Item_Type;
   type Column_Text_Ptr is access Column_Text_Item_Type;
   type Column_Text_Item_Type is record
      Text     : String (1 .. 80);
      Length   : Natural;
      Row      : Natural; -- Which row in the column.
      Next     : Column_Text_Ptr;
   end record;
   type Column_Text_Ptrs_Type is array (Column_Index_Type) of Column_Text_Ptr;
   type Column_Widths_Type is array (Column_Index_Type) of Natural;

   procedure Free is new Ada.Unchecked_Deallocation (Column_Text_Item_Type, Column_Text_Ptr);

   type State_Type is (Title, Contents, Table_Header, Multi_Column, Normal, Index_Start, Index);

   type Texinfo_Output_Type is new ARM_Output.Output_Type with record
      File     : Ada.Text_IO.File_Type;
      Is_Valid : Boolean := False;

      State         : State_Type;
      In_Paragraph  : Boolean := False; --  Sub-state within major states
      Style         : ARM_Output.Paragraph_Style_Type;
      Indent        : ARM_Output.Paragraph_Indent_Type;
      End_Hang_Seen : Boolean;

      --  Detecting end of title page
      Line_Empty      : Boolean := False; --  True if current line contains only whitespace.
      First_Word      : String (1 .. 80);
      First_Word_Last : Natural;

      --  Building menus
      Menu_Section : ARM_Contents.Section_Number_Type := 0;
      Menu_Clause  : Natural := 0;

      --  Table and Multi-Column format
      Column_Count    : ARM_Output.Column_Count;
      Current_Column  : Natural;
      Current_Row     : Natural;
      Column_Text     : Column_Text_Ptrs_Type := (others => null);
      Column_Widths   : Column_Widths_Type;
      Max_Row         : Natural := 0;
   end record;

end ARM_Texinfo;
