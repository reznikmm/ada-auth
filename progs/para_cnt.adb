    -- Count numbered paragraphs in a version of the RM, and store
    -- store the results into a CVS file. Run this program in the directory
    -- with the RM*.html files.

    -- Edit History:
    --
    --  2/21/19 - RLB - Created program.

with Ada.Text_IO;
with Ada.Directories;
package Para_Cnt_Pkg is

    Result_File : Ada.Text_IO.File_Type;

    procedure Count_File (Item : in Ada.Directories.Directory_Entry_Type);
end Para_Cnt_Pkg;


with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
package body Para_Cnt_Pkg is

    procedure Count_File (Item : in Ada.Directories.Directory_Entry_Type) is
	Cnts : array ('0'..'9') of Natural := (others => 0);
	Total : Natural := 0;
	Inserted : Natural := 0;
        The_File : Ada.Text_IO.File_Type;

	Buffer : String (1..1024);
 	BLen   : Natural;
	LCnt   : Long_Integer := 0;

	PStart : Natural;
	PEnd   : Natural;
    begin
        Ada.Text_IO.Put_Line ("  Counting file " & Ada.Directories.Simple_Name (Item));

	Ada.Text_IO.Open (The_File, Ada.Text_IO.In_File,
				Ada.Directories.Full_Name (Item));

	begin
	    loop
		Ada.Text_IO.Get_Line (The_File, Buffer, BLen);
		LCnt := LCnt + 1;

		-- In the absence of a bug, there should be only one
		-- paragraph per line. A paragraph number looks like:
		--    <div class="paranum"><a name="p20">20/5</a></div>
		-- Older files are missing the anchor (the "a" command),
		-- and they sometimes have the markup in upper case.

		Ada.Strings.Fixed.Translate (Buffer(1..BLen),
		    Ada.Strings.Maps.Constants.Lower_Case_Map);

		PStart := Ada.Strings.Fixed.Index (Buffer(1..BLen),
			"<div class=""paranum"">");
		if PStart /= 0 then -- A paragraph number found.
		    PEnd := Ada.Strings.Fixed.Index (Buffer(PStart..BLen),
			"</div>");
		    if PEnd = 0 then
			Ada.Text_IO.Put_Line ("** Unable to find paranum end on line" & LCnt'Image);
		    elsif Ada.Strings.Fixed.Index (Buffer(PEnd..BLen),
			   "<div class=""paranum"">") /= 0 then
			Ada.Text_IO.Put_Line ("** Multiple paranums on line" & LCnt'Image);
		    else
			Total := Total + 1;
			if Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				".") /= 0 then
			    Inserted := Inserted + 1;
			-- else not inserted.
			end if;
			if Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/1<") /= 0 then
			    Cnts('1') := Cnts('1') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/2<") /= 0 then
			    Cnts('2') := Cnts('2') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/3<") /= 0 then
			    Cnts('3') := Cnts('3') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/4<") /= 0 then
			    Cnts('4') := Cnts('4') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/5<") /= 0 then
			    Cnts('5') := Cnts('5') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/6<") /= 0 then
			    Cnts('6') := Cnts('6') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/7<") /= 0 then
			    Cnts('7') := Cnts('7') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/8<") /= 0 then
			    Cnts('8') := Cnts('8') + 1;
			elsif Ada.Strings.Fixed.Index (Buffer(PStart+22..PEnd+4),
				"/9<") /= 0 then
			    Cnts('9') := Cnts('9') + 1;
			else -- No expected / number.
			    Cnts('0') := Cnts('0') + 1;
			end if;
		    end if;
		-- else no paragraph number.
		end if;

            end loop;

	exception
	    when Ada.Text_IO.End_Error => -- Reached the end of the file.
	        Ada.Text_IO.Close (The_File);
	end;


	-- Write the results:
        Ada.Text_IO.Put (Result_File, Ada.Directories.Simple_Name (Item));
        Ada.Text_IO.Put (Result_File, "," & Total'Image);
        Ada.Text_IO.Put (Result_File, "," & Inserted'Image);
        for I in Cnts'Range loop
	    Ada.Text_IO.Put (Result_File, "," & Cnts(I)'Image);
	end loop;
	Ada.Text_IO.New_Line (Result_File);

        Ada.Text_IO.Put_Line ("    Saw" & Total'Image & " paragraphs, with" &
				Inserted'Image & " inserted paragraphs");
    end Count_File;

end Para_Cnt_Pkg;


with Ada.Text_IO;
with Ada.Directories;
with Ada.Command_Line;
with Para_Cnt_Pkg;
procedure Para_Cnt is

begin
    Ada.Text_IO.Put_Line ("Paragraph Count v. 1.0");
    if Ada.Command_Line.Argument_Count /= 1 then
        Ada.Text_IO.Put_Line ("** No output file specified on command line!");
	return;
    -- else OK.
    end if;
    Ada.Text_IO.Put_Line ("  Write results to " & Ada.Command_Line.Argument(1) & ".csv");

    Ada.Text_IO.Create (Para_Cnt_Pkg.Result_File, Ada.Text_IO.Out_File,
			Ada.Command_Line.Argument(1) & ".csv");

    -- Put a CVS header:
    Ada.Text_IO.Put_Line (Para_Cnt_Pkg.Result_File, "File, Total, Inserted, Org, '/1, '/2, '/3, '/4, '/5, '/6, '/7, '/8, '/9");

    Ada.Directories.Search (Directory => ".",
			    Pattern => "RM-*.html",
			    Process => Para_Cnt_Pkg.Count_File'Access);

    Ada.Text_IO.Close (Para_Cnt_Pkg.Result_File);
    Ada.Text_IO.Put_Line ("Paragraph Counting completed");

end Para_Cnt;
