with Ada.Text_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Characters.Handling,
     Ada.Command_Line;
procedure Rtf2Form is

    --
    -- Convert an RTF file for the Rational to a roughly formatted .MSS
    -- file.
    --
    -- Edit History:
    --
    --  1/19/06 - RLB - Created program.
    --  1/20/06 - RLB - Changed to generate tab markers for tabs, as John
    --			wants to preserve tabs where possible.

    use Ada.Strings.Unbounded;

    -- Declarations stolen from Trash-Finder:

    type Line_Count is range 0 .. 1_000_000; -- 1 million lines ought to be enough.
    type Line_Type;
    type Line_Access is access Line_Type;
    type Line_Type is record
        Number : Line_Count;
        Text : Ada.Strings.Unbounded.Unbounded_String;
        Next : Line_Access;
    end record;

    procedure Free is new Ada.Unchecked_Deallocation (Line_Type, Line_Access);
        -- Free an individual line.

    procedure Free_All (List : in out Line_Access) is
        -- Free the entire list.
        Temp : Line_Access;
    begin
        while List /= null loop
            Temp := List;
            List := List.Next;
            Free (Temp);
        end loop;
    end Free_All;

    procedure Load_File (File_Name : in String;
			 Data : in out Line_Access) is
	-- Read the file, breaking at each paragraph end (to keep the lines from
	-- getting too long). The original line breaks are irrelevant.
	File : Ada.Text_IO.File_Type;
        Last, Working : Line_Access := null;
	Count : Line_Count := 0;
	Buffer : String (1..1000); -- Since line endings aren't significant, the
				   -- length doesn't matter much, other than we
				   -- might miss a "\par" that way.
	BLength : Natural;
	Break_Loc, Start_Loc : Natural;
    begin
	Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);
	loop
	    Ada.Text_IO.Get_Line (File, Buffer, BLength);
	    Count := Count + 1;
--Ada.Text_IO.Put_Line("Line" & Line_Count'Image(Count) & ": read=" & Buffer(1..BLength));
	    Start_Loc := 1;
	    Break_Loc := Start_Loc;
	    while Break_Loc <= BLength-4 loop
		-- Look for "\par " or "\par\", only (in particular, not "\pard").
		exit when Buffer(Break_Loc..Break_Loc+3) = "\par" and then
		   (Buffer(Break_Loc+4) = ' ' or else
		    Buffer(Break_Loc+4) = '\');
		    -- Found it.
		Break_Loc := Break_Loc + 1;
	    end loop;
	    if Break_Loc > BLength-4 then
		Break_Loc := BLength+1;
	    end if;
	    if Last = null then
	        Working := new Line_Type;
	        Working.Number := Count;
	        Working.Next := null;
	        Working.Text := To_Unbounded_String (Buffer(1..Break_Loc-1));
	        Data := Working;
		Last := Working;
--Ada.Text_IO.Put_Line("Line" & Line_Count'Image(Count) & ": initial=" & Buffer(1..Break_Loc-1));
	    elsif Length(Last.Text) > 30000 and then Break_Loc > BLength then
		-- Break at the original place, as this is too long.
		-- This is fine so long as the length of Buffer < 2768/2 (1386)
	        Last.Text := Last.Text & To_Unbounded_String (Buffer(1..Break_Loc-1));
--Ada.Text_IO.Put_Line("Line" & Line_Count'Image(Count) & ": append, then break=" & Buffer(1..Break_Loc-1));
		Break_Loc := BLength - 3; -- Force a new line here. (This will make
					  -- Start_Loc small enough to enter the
					  -- loop below and create an empty line.)
	    else
	        Last.Text := Last.Text & To_Unbounded_String (Buffer(1..Break_Loc-1));
--Ada.Text_IO.Put_Line("Line" & Line_Count'Image(Count) & ": append=" & Buffer(1..Break_Loc-1));
	    end if;
	    Start_Loc := Break_Loc + 4; -- Skip \par.
	    if Start_Loc <= BLength and then Buffer(Start_Loc) = ' ' then
		Start_Loc := Start_Loc + 1; -- Skip trailling space, too.
	    end if;
--Ada.Text_IO.Put_Line("Start_Loc = " & Natural'Image(Start_Loc) & " Blength=" & Natural'Image(BLength));
	    while Start_Loc <= BLength+1 loop -- +1 so we handle end of line "\par" properly.
		-- Create a new line:
	        Working := new Line_Type;
	        Working.Number := Count;
	        Working.Next := null;
	        Last.Next := Working;
	        Last := Working;
	        Break_Loc := Start_Loc;
	        while Break_Loc <= BLength-4 loop
		    -- Look for "\par " or "\par\", only (in particular, not "\pard").
		    exit when Buffer(Break_Loc..Break_Loc+3) = "\par" and then
		       (Buffer(Break_Loc+4) = ' ' or else
		        Buffer(Break_Loc+4) = '\');
		        -- Found it.
		    Break_Loc := Break_Loc + 1;
	        end loop;
	        if Break_Loc > BLength-4 then
		    Break_Loc := BLength+1;
	        end if;
	        Working.Text := To_Unbounded_String (Buffer(Start_Loc..Break_Loc-1));
--Ada.Text_IO.Put_Line("Line" & Line_Count'Image(Count) & ": startp=" & Buffer(Start_Loc..Break_Loc-1));
	        Start_Loc := Break_Loc + 4; -- Skip \par.
	        if Start_Loc <= BLength and then Buffer(Start_Loc) = ' ' then
		    Start_Loc := Start_Loc + 1; -- Skip trailling space, too.
	        end if;
	    end loop;
	    -- Done with line when we get here.
	end loop;
    exception
	when Ada.Text_IO.End_Error => -- Done reading.
	    Ada.Text_IO.Close (File);
    end Load_File;


    -- Stack of RTF command information:
    type RTF_Group_Kind is (Unknown, Empty,
			    Bold, Italic, Tab, Line_Break,
			    Exam, ExamCom, Key); -- Type of RTF command.
    type Brace_Info is record
	Kind : RTF_Group_Kind;
    end record;
    Brace_Info_Stack : array (1..1000) of Brace_Info;
    Current_Brace : Natural := 0;

    type Paragraph_Styles is (Unknown, None, Normal, Example, Description);
    Current_Paragraph_Style : Paragraph_Styles := Unknown;

    procedure Process_and_Write_File (File_Name : in String;
				      Data : in out Line_Access) is
	File : Ada.Text_IO.File_Type;
        Cursor : Line_Access := Data;
	Chars_on_Line : Natural; -- Number of characters output on the current line.
	-- Data:
	Deepest_Brace_Nesting : Natural := 0;
	Paragraphs : Natural := 0;
	Escaped_Chars : Natural := 0;
	EM_Dashes : Natural := 0;
	EN_Dashes : Natural := 0;
	Intraparagraph_Line_Breaks : Natural := 0;
	Intraparagraph_Tabs : Natural := 0;
	Brace_Kinds_Found : array (RTF_Group_Kind) of Natural := (others => 0);
	Styles_Found : array (Paragraph_Styles) of Natural := (others => 0);
	procedure Close_Old_Style is
	    -- Close the current paragraph style in preparation for a new one.
	begin
	    case Current_Paragraph_Style is
		when Unknown => null;
		when None => null;
		when Normal =>
		    null; --Ada.Text_IO.Put_Line (File, "@end[Intro]"); -- ** Temp (no style needed for these).
		when Example =>
		    Ada.Text_IO.Put_Line (File, "@end[Example]");
		when Description =>
		    Ada.Text_IO.Put_Line (File, "@end[Description]");
	    end case;
	end Close_Old_Style;
    begin
	Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);
	-- Header stuff (ignored, really).
	Ada.Text_IO.Put_Line (File, "@Part(xxx, Root=""rat.msm"")");
	Ada.Text_IO.Put_Line (File, "");
	Ada.Text_IO.Put_Line (File, "@comment($Source: e:\\cvsroot/ARM/Progs/rtf2form.ada,v $)");
	Ada.Text_IO.Put_Line (File, "@comment($Revision: 1.2 $ $Date: 2006/01/28 06:49:32 $)");
	Ada.Text_IO.Put_Line (File, "");

	while Cursor /= null loop
	    declare
		Our_Line : constant String := To_String(Cursor.Text);
		Working : Natural := Our_Line'First;
		Last_Output : Natural := Working - 1;
		procedure Clean_Buffer_and_Close_Old_Style is
		    -- Close the current paragraph style in preparation for a new one.
		begin
		    Ada.Text_IO.Put_Line (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Close style on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working-1));
		    Chars_on_Line := 0; -- Start over.
		    Last_Output := Working-1;
		    Close_Old_Style;
		end Clean_Buffer_and_Close_Old_Style;

		function Possible_Style (Head, Tail : Natural) return Boolean is
		    -- Check that the stuff between Head and Tail
		    -- can *only* be a style, with nothing extra.
		begin
--Ada.Text_IO.Put_Line ("Possible style=" & Our_Line(Head..Tail));
		    for I in Head .. Tail loop
			if Our_Line(I) = '{' then
--Ada.Text_IO.Put_Line ("  NO - open group");
			    return False; -- Starts some group, not part of a style.
			elsif Our_Line(I) = '{' then
--Ada.Text_IO.Put_Line ("  NO - close group");
			    return False; -- Ends some group, not part of a style.
			elsif Our_Line(I) = ' ' then
			    if I = Tail then
--Ada.Text_IO.Put_Line ("  OK");
				return True;
                            elsif Our_Line(I+1) = '\' then
				null; -- OK.
			    else -- We have to have another command, not text, immediately following.
--Ada.Text_IO.Put_Line ("  NO - plain text found");
				return False;
			    end if;
			elsif Our_Line(I) in 'a'..'z' or else
			     Our_Line(I) in '0'..'9' or else
			     Our_Line(I) = '-' or else
			     Our_Line(I) = '\' then
			    null; -- OK.
			else
--Ada.Text_IO.Put_Line ("  NO - odd character at" & Natural'Image(I));
			    return False; -- Don't know what this is.
			end if;
		    end loop;
--Ada.Text_IO.Put_Line ("  OK");
		    return True; -- OK if we get here.
		end Possible_Style;

		procedure Start_Style (Style_Code_Loc, End_of_Style : Natural) is
		    -- Generate an appropriate style operation.
		    -- We've already checked the basic legality of the style.
		    -- Set Working and Last_Output as needed.
		begin
		    -- We can assume that the style code has enough room.
		    if Our_Line(Style_Code_Loc..Style_Code_Loc+4) = "\s15\" then
		        -- Normal style (called "TP Text para")
--Ada.Text_IO.Put_Line ("  Normal style");
		        if Current_Paragraph_Style /= Normal then
			    Clean_Buffer_and_Close_Old_Style;
			    --Ada.Text_IO.Put_Line (File, "@begin[Intro]"); -- ** Temp.
			    Current_Paragraph_Style := Normal;
		        -- else no change needed.
		        end if;
		        Styles_Found(Normal) := Styles_Found(Normal) + 1;
		        -- We just discard the entire style mess.
		        Last_Output := End_of_Style;
		        Working := Last_Output + 1;

		    elsif Our_Line(Style_Code_Loc..Style_Code_Loc+4) = "\s25\" then
		        -- Example style (called "PP Program para")
--Ada.Text_IO.Put_Line ("  Example style");
		        if Current_Paragraph_Style /= Example then
			    Clean_Buffer_and_Close_Old_Style;
			    Ada.Text_IO.Put_Line (File, "@begin[Example]");
			    Current_Paragraph_Style := Example;
		        -- else no change needed.
		        end if;
		        Styles_Found(Example) := Styles_Found(Example) + 1;
		        -- We just discard the entire style mess.
		        Last_Output := End_of_Style;
		        Working := Last_Output + 1;

		    elsif Our_Line(Style_Code_Loc..Style_Code_Loc+4) = "\s32\" then
		        -- Description style (called "BP Bullet", but often
			-- not used for bullets)
--Ada.Text_IO.Put_Line ("  Description style");
		        if Current_Paragraph_Style /= Description then
			    Clean_Buffer_and_Close_Old_Style;
			    Ada.Text_IO.Put_Line (File, "@begin[Description]");
			    Current_Paragraph_Style := Description;
		        -- else no change needed.
		        end if;
		        Styles_Found(Description) := Styles_Found(Description) + 1;
		        -- We just discard the entire style mess.
		        Last_Output := End_of_Style;
		        Working := Last_Output + 1;

		    else -- Unknown style.
--Ada.Text_IO.Put_Line ("  Unknown style: code = " & Our_Line(Style_Code_Loc..Style_Code_Loc+4));
		        if Current_Paragraph_Style /= Unknown then
			    Clean_Buffer_and_Close_Old_Style;
			    Current_Paragraph_Style := Unknown;
		        -- else no change needed.
		        end if;
		        Styles_Found(Unknown) := Styles_Found(Unknown) + 1;
		        -- We leave the style mess for analysis.
		        Working := Working + 1;
		    end if;
		end Start_Style;

	    begin
	        Paragraphs := Paragraphs + 1;
		Chars_on_Line := 0;
		while Working <= Our_Line'Last loop
		    if Our_Line(Working) = '{' then
			-- Start an RTF section.
			-- Output everything to here:
			Ada.Text_IO.Put (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Open Brace on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working-1));
			Chars_on_Line := Chars_on_Line + ((Working-1) - (Last_Output+1) + 1);

			-- Stack this opening:
			Current_Brace := Current_Brace + 1;
			if Current_Brace > Deepest_Brace_Nesting then
			    Deepest_Brace_Nesting := Current_Brace;
			end if;

			if Working+1 > Our_Line'Last or else
			   Our_Line(Working+1) /= '\' then
			    -- No command here, so it is not interesting (ever)
			    -- and we might as well remove it.
			    Brace_Info_Stack(Current_Brace).Kind := Empty;
			    Last_Output := Working; -- Skip the brace.
--Ada.Text_IO.Put_Line("Empty Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+3 <= Our_Line'Last and then
			   Our_Line(Working..Working+3) = "{\b " then
			    Brace_Info_Stack(Current_Brace).Kind := Bold;
			    Ada.Text_IO.Put (File, "@b[");
			    Chars_on_Line := Chars_on_Line + 3;
			    Last_Output := Working+3;
--Ada.Text_IO.Put_Line("Bold Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+3 <= Our_Line'Last and then
			   Our_Line(Working..Working+3) = "{\i " then
			    Brace_Info_Stack(Current_Brace).Kind := Italic;
			    Ada.Text_IO.Put (File, "@i[");
			    Chars_on_Line := Chars_on_Line + 3;
			    Last_Output := Working+3;
--Ada.Text_IO.Put_Line("Italic Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+6 <= Our_Line'Last and then
			   Our_Line(Working..Working+6) = "{\line " then
			    Brace_Info_Stack(Current_Brace).Kind := Line_Break;
			    Ada.Text_IO.New_Line (File);
			    Chars_on_Line := 0; -- Start over.
			    Last_Output := Working+6;
--Ada.Text_IO.Put_Line("Line_Break Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+5 <= Our_Line'Last and then
			   Our_Line(Working..Working+5) = "{\tab " then
			    Brace_Info_Stack(Current_Brace).Kind := Tab;
			    Ada.Text_IO.Put (File, "@\");
			    Chars_on_Line := Chars_on_Line + 2;
			    Last_Output := Working+5;
--Ada.Text_IO.Put_Line("Tab Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+14 <= Our_Line'Last and then
			   Our_Line(Working..Working+14) = "{\cs16\f1\fs20 " then
			    -- \cs16 = "PC Program char" style. Could look for
			    -- that early on to find the number. Same with
			    -- "\f1" (Arial). But they might be constant, and
			    -- that would be wasted work.
			    Brace_Info_Stack(Current_Brace).Kind := Exam;
			    Ada.Text_IO.Put (File, "@exam[");
			    Chars_on_Line := Chars_on_Line + 6;
			    Last_Output := Working+14;
--Ada.Text_IO.Put_Line("Exam Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+8 <= Our_Line'Last and then
			   Our_Line(Working..Working+8) = "{\cs20\b " then
			    -- \cs20 = "RW Reserved word" style. Could look for
			    -- that early on to find the number.
			    Brace_Info_Stack(Current_Brace).Kind := Key;
			    Ada.Text_IO.Put (File, "@key[");
			    Chars_on_Line := Chars_on_Line + 5;
			    Last_Output := Working+8;
--Ada.Text_IO.Put_Line("Key (in example) Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+16 <= Our_Line'Last and then
			   Our_Line(Working..Working+16) = "{\cs20\b\f1\fs20 " then
			    -- \cs20 = "RW Reserved word" style. \f1 = Arial
			    -- font.
			    Brace_Info_Stack(Current_Brace).Kind := Key;
			    Ada.Text_IO.Put (File, "@key[");
			    Chars_on_Line := Chars_on_Line + 5;
			    Last_Output := Working+16;
--Ada.Text_IO.Put_Line("Key (in text) Open Brace on Line" & Line_Count'Image(Cursor.Number));

			elsif Working+8 <= Our_Line'Last and then
			   Our_Line(Working..Working+8) = "{\cs24\i " then
			    -- \cs24 = "CT Comment" style.
			    Brace_Info_Stack(Current_Brace).Kind := ExamCom;
			    Ada.Text_IO.Put (File, "@examcom[");
			    Chars_on_Line := Chars_on_Line + 9;
			    Last_Output := Working+8;
--Ada.Text_IO.Put_Line("ExamCom Open Brace on Line" & Line_Count'Image(Cursor.Number));

			else
			    Brace_Info_Stack(Current_Brace).Kind := Unknown;
			    Ada.Text_IO.Put (File, '{');
			    Chars_on_Line := Chars_on_Line + 1;
			    Last_Output := Working;
--Ada.Text_IO.Put_Line("Unknown Open Brace on Line" & Line_Count'Image(Cursor.Number));
			end if;
			Brace_Kinds_Found(Brace_Info_Stack(Current_Brace).Kind) :=
			    Brace_Kinds_Found(Brace_Info_Stack(Current_Brace).Kind) + 1;
			Working := Last_Output + 1;

		    elsif Our_Line(Working) = '}' then
			-- End an RTF section.
			-- Output everything to here:
			Ada.Text_IO.Put (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Close Brace on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working-1));
			Chars_on_Line := Chars_on_Line + ((Working-1) - (Last_Output+1) + 1);
			Last_Output := Working; -- We're including this character.
			case Brace_Info_Stack(Current_Brace).Kind is
			    when Unknown =>
				Ada.Text_IO.Put (File, '}');
--Ada.Text_IO.Put_Line("Unknown Close Brace on Line" & Line_Count'Image(Cursor.Number));
				Chars_on_Line := Chars_on_Line + 1;
			    when Empty =>
				-- Just remove this one.
--Ada.Text_IO.Put_Line("Empty Close Brace on Line" & Line_Count'Image(Cursor.Number));
				null;
			    when Bold =>
				Ada.Text_IO.Put (File, ']');
--Ada.Text_IO.Put_Line("Bold Close Brace on Line" & Line_Count'Image(Cursor.Number));
				Chars_on_Line := Chars_on_Line + 1;
			    when Italic =>
				Ada.Text_IO.Put (File, ']');
--Ada.Text_IO.Put_Line("Italic Close Brace on Line" & Line_Count'Image(Cursor.Number));
				Chars_on_Line := Chars_on_Line + 1;
			    when Line_Break =>
				null;
--Ada.Text_IO.Put_Line("Link_Break Close Brace on Line" & Line_Count'Image(Cursor.Number));
			    when Tab =>
				null;
--Ada.Text_IO.Put_Line("Tab Close Brace on Line" & Line_Count'Image(Cursor.Number));
			    when Exam =>
				Ada.Text_IO.Put (File, ']');
--Ada.Text_IO.Put_Line("Exam Close Brace on Line" & Line_Count'Image(Cursor.Number));
				Chars_on_Line := Chars_on_Line + 1;
			    when ExamCom =>
				Ada.Text_IO.Put (File, ']');
--Ada.Text_IO.Put_Line("ExamCom Close Brace on Line" & Line_Count'Image(Cursor.Number));
				Chars_on_Line := Chars_on_Line + 1;
			    when Key =>
				Ada.Text_IO.Put (File, ']');
--Ada.Text_IO.Put_Line("Key Close Brace on Line" & Line_Count'Image(Cursor.Number));
				Chars_on_Line := Chars_on_Line + 1;
			end case;
			Current_Brace := Current_Brace - 1;
			Working := Working + 1;

		    elsif Our_Line(Working) = ' ' then
			-- Possible break here.
			if Chars_on_Line + (Working - (Last_Output+1) + 1) > 65 then
			    -- We need a break here; write it.
			    Ada.Text_IO.Put_Line (File, Our_Line(Last_Output+1..Working));
--Ada.Text_IO.Put_Line("Soft line break on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working));
			    Chars_on_Line := 0; -- Start over.
			    Last_Output := Working;
			    Working := Working + 1;
			else -- Don't need a break.
			    Working := Working + 1;
			end if;

		    elsif Our_Line(Working) = '\' then
			-- RTF control word or symbol; see whether it is
			-- interesting.
			if Working+5 <= Our_Line'Last and then
			   Our_Line(Working..Working+5) = "\line " then
			    -- Line break in a paragraph (without an open
			    -- brace). Output a New_line.
			    Ada.Text_IO.Put_Line (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Line break on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working));
			    Intraparagraph_Line_Breaks := Intraparagraph_Line_Breaks + 1;
			    Chars_on_Line := 0; -- Start over.
			    Last_Output := Working+5;
			    Working := Last_Output + 1;

			elsif Working+4 <= Our_Line'Last and then
			   Our_Line(Working..Working+4) = "\tab " then
			    -- Tab inside of a paragraph. Output the previous text and a tab command.
			    -- (Note: We don't define any tabstops here; that
			    -- will need to be done by hand as needed).
			    Ada.Text_IO.Put (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Tab on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working));
			    Chars_on_Line := Chars_on_Line + ((Working-1) - (Last_Output+1) + 1);
			    Ada.Text_IO.Put (File, "@\");
			    Chars_on_Line := Chars_on_Line + 2;
			    Intraparagraph_Tabs := Intraparagraph_Tabs + 1;
			    Last_Output := Working+4;
			    Working := Last_Output + 1;

			elsif Working+1 <= Our_Line'Last and then
			   (Our_Line(Working+1) = '\' or else
			    Our_Line(Working+1) = '{' or else
			    Our_Line(Working+1) = '}') then
			    -- Escaped character. Output the previous text and
			    -- the literal character. (We have to do this to
			    -- prevent the escaped character from being
			    -- acted on as if it is a control or group marker).
			    Ada.Text_IO.Put (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Escaped char on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working));
			    Chars_on_Line := Chars_on_Line + ((Working-1) - (Last_Output+1) + 1);
			    Escaped_Chars := Escaped_Chars + 1;
			    Last_Output := Working;
			    Working := Working + 2;
			    -- We leave the literal character in the output buffer for later writing.

			elsif Working+7 <= Our_Line'Last and then
			   Our_Line(Working..Working+7) = "\endash " then
			    -- Line break in a paragraph. Output a New_line.
			    Ada.Text_IO.Put (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Endash on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working));
			    Chars_on_Line := Chars_on_Line + ((Working-1) - (Last_Output+1) + 1);
			    Ada.Text_IO.Put (File, "@en");
			    Chars_on_Line := Chars_on_Line + 3;
			    EN_Dashes := EN_Dashes + 1;
			    Last_Output := Working+7;
			    Working := Last_Output + 1;

			elsif Working+7 <= Our_Line'Last and then
			   Our_Line(Working..Working+7) = "\emdash " then
			    -- Line break in a paragraph. Output a New_line.
			    Ada.Text_IO.Put (File, Our_Line(Last_Output+1..Working-1));
--Ada.Text_IO.Put_Line("Emdash on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Working));
			    Chars_on_Line := Chars_on_Line + ((Working-1) - (Last_Output+1) + 1);
			    Ada.Text_IO.Put (File, "@em");
			    Chars_on_Line := Chars_on_Line + 3;
			    EM_Dashes := EM_Dashes + 1;
			    Last_Output := Working+7;
			    Working := Last_Output + 1;

			elsif Working+5 <= Our_Line'Last and then
			   Our_Line(Working..Working+5) = "\pard\" then
 			    -- Starts a (full) paragraph style.
--Ada.Text_IO.Put_Line("Paragraph style on Line" & Line_Count'Image(Cursor.Number));
			    declare
				End_of_Style : Natural :=
				    Ada.Strings.Fixed.Index (Our_Line(Working..Our_Line'Last),
					"\langfenp1033 ") + 13;
				-- For some reason, all of the styles end with this.
				Style_Code : Natural :=
				    Ada.Strings.Fixed.Index (Our_Line(Working..Our_Line'Last),
					"\s");
				-- The style code is generally first.
			    begin
				if Style_Code = 0 or else End_of_Style = 13 or else
				    Style_Code > End_of_Style or else
				    (not Possible_Style (Working, End_of_Style)) then
				    -- No style code, no end of style, the style
				    -- code comes after the end of the style, or
				    -- the style has problems. (In which case, we are
				    -- probably taking too much as the style).
 				    -- This is unknown. Do nothing and just output the character.
				    Working := Working + 1;
				else
				    Start_Style (Style_Code, End_of_Style);
					-- This sets Working and Last_Output appropriately.
				end if;
			    end;

			elsif Working+5 <= Our_Line'Last and then
			    Our_Line(Working..Working+5) = "\pard " then
 			    -- Starts a (short) paragraph style.
--Ada.Text_IO.Put_Line("Short paragraph style on Line" & Line_Count'Image(Cursor.Number));
			    declare
				End_of_Style : Natural :=
				    Ada.Strings.Fixed.Index (Our_Line(Working..Our_Line'Last),
					"\lin0\itap0 ");
				-- For some reason, all of the styles end with this.
				Style_Code : Natural :=
				    Ada.Strings.Fixed.Index (Our_Line(Working..Our_Line'Last),
					"\s");
				-- The style code is generally first.
			    begin
				if End_of_Style = 0 then
				    End_of_Style :=
				        Ada.Strings.Fixed.Index (Our_Line(Working..Our_Line'Last),
					    "\lin480\itap0 ");
				    if End_of_Style = 0 then
				        End_of_Style :=
				            Ada.Strings.Fixed.Index (Our_Line(Working..Our_Line'Last),
					        "\lin360\itap0 ");
				        if End_of_Style /= 0 then
				            End_of_Style := End_of_Style + 13;
				        end if;
				    else
				        End_of_Style := End_of_Style + 13;
				    end if;
				else
				    End_of_Style := End_of_Style + 11;
				end if;

				if Style_Code = 0 or else End_of_Style = 0 or else
				    Style_Code > End_of_Style or else
				    (not Possible_Style (Working, End_of_Style)) then
				    -- No style code, no end of style, the style
				    -- code comes after the end of the style, or
				    -- the style has problems. (In which case, we are
				    -- probably taking too much as the style).
 				    -- This is unknown. Do nothing and just output the character.
				    Working := Working + 1;
				else
				    Start_Style (Style_Code, End_of_Style);
					-- This sets Working and Last_Output appropriately.
				end if;
			    end;

			else -- Don't recognize it, just output it (later).
			    Working := Working + 1;
			end if;
		    else -- Just output it (later).
			Working := Working + 1;
		    end if;
		end loop;
		-- Output any remaining text (it can't need to be broken):
		Ada.Text_IO.Put (File, Our_Line(Last_Output+1..Our_Line'Last));
--Ada.Text_IO.Put_Line("Remaining text on Line" & Line_Count'Image(Cursor.Number) & ": output=" & Our_Line(Last_Output+1..Our_Line'Last));
	    end;
	    Ada.Text_IO.New_Line (File, 2); -- Double space.
	    Cursor := Cursor.Next;
	end loop;
	Close_Old_Style; -- Close the last style.
	Ada.Text_IO.Close (File);
	Free_All (Data);
	-- Display statistics:
	Ada.Text_IO.Put_Line ("Conversion statistics:");
	Ada.Text_IO.Put_Line ("  Total paragraphs:" & Natural'Image(Paragraphs));
	Ada.Text_IO.Put_Line ("  Deepest nesting:" & Natural'Image(Deepest_Brace_Nesting));
	for I in RTF_Group_Kind loop
	    Ada.Text_IO.Put_Line ("  Kind " & RTF_Group_Kind'Image(I) &
		":" & Natural'Image(Brace_Kinds_Found(I)));
	end loop;
	Ada.Text_IO.Put_Line ("  Escaped characters found:" & Natural'Image(Escaped_Chars));
	Ada.Text_IO.Put_Line ("  EM Dashes found:" & Natural'Image(EM_Dashes));
	Ada.Text_IO.Put_Line ("  EN Dashes found:" & Natural'Image(EN_Dashes));
	Ada.Text_IO.Put_Line ("  Line breaks inside of paragraphs:" & Natural'Image(Intraparagraph_Line_Breaks));
	Ada.Text_IO.Put_Line ("  Tabs inside of paragraphs:" & Natural'Image(Intraparagraph_Tabs));
	for I in Paragraph_Styles loop
	    Ada.Text_IO.Put_Line ("  Style " & Paragraph_Styles'Image(I) &
		":" & Natural'Image(Styles_Found(I)));
	end loop;
    end Process_and_Write_File;

    File_Contents : Line_Access;

begin
    Ada.Text_IO.Put_Line ("Convert " &
       Ada.Command_Line.Argument(1) & ".rtf to " &
       Ada.Command_Line.Argument(1) & ".mss");
    Load_File (Ada.Command_Line.Argument(1) & ".rtf", File_Contents);
    Process_and_Write_File (Ada.Command_Line.Argument(1) & ".mss", File_Contents);
    Ada.Text_IO.Put_Line ("Conversion completed");
end Rtf2Form;
