with ARM_Format, ARM_Contents;
package ARM_Master is

    --
    -- Ada reference manual formatter (ARM_Form).
    --
    -- This package contains the routines to parse the master file, and
    -- execute it.
    --
    -- ---------------------------------------
    -- Copyright 2006, 2011, 2012
    --   AXE Consultants. All rights reserved.
    -- P.O. Box 1512, Madison WI  53701
    -- E-Mail: randy@rrsoftware.com
    --
    -- ARM_Form is free software: you can redistribute it and/or modify
    -- it under the terms of the GNU General Public License version 3
    -- as published by the Free Software Foundation.
    --
    -- AXE CONSULTANTS MAKES THIS TOOL AND SOURCE CODE AVAILABLE ON AN "AS IS"
    -- BASIS AND MAKES NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE ACCURACY,
    -- CAPABILITY, EFFICIENCY, MERCHANTABILITY, OR FUNCTIONING OF THIS TOOL.
    -- IN NO EVENT WILL AXE CONSULTANTS BE LIABLE FOR ANY GENERAL,
    -- CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
    -- EVEN IF AXE CONSULTANTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
    -- DAMAGES.
    --
    -- A copy of the GNU General Public License is available in the file
    -- gpl-3-0.txt in the standard distribution of the ARM_Form tool.
    -- Otherwise, see <http://www.gnu.org/licenses/>.
    --
    -- If the GPLv3 license is not satisfactory for your needs, a commercial
    -- use license is available for this tool. Contact Randy at AXE Consultants
    -- for more information.
    --
    -- ---------------------------------------
    --
    -- Edit History:
    --
    --  1/05/06 - RLB - Created base package to replace hard-coded main program.
    --  1/12/06 - RLB - Removed obsolete Document parameter.
    -- 10/18/11 - RLB - Changed to GPLv3 license.
    --  8/31/12 - RLB - Added Output_Path.

    type Output_Format_Type is (HTML, RTF, Text, Corr, Info);

    procedure Read_and_Process_Master_File (
	File_Name : in String;
	The_Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
	The_Change_Version : ARM_Contents.Change_Version_Type; -- Change version.
        Output_Format : in Output_Format_Type;
        Output_Path : in String);

	-- Read and process the master file given.

end ARM_Master;
