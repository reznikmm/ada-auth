with ARM_Format, ARM_Contents;
package ARM_Master is

    --
    -- Ada reference manual formatter.
    --
    -- This package contains the routines to parse the master file, and
    -- execute it.
    --
    -- ---------------------------------------
    -- Copyright 2006  AXE Consultants.
    -- P.O. Box 1512, Madison WI  53701
    -- E-Mail: randy@rrsoftware.com
    --
    -- AXE Consultants grants to all users the right to use/modify this
    -- formatting tool for non-commercial purposes. (ISO/IEC JTC 1 SC 22 WG 9
    -- activities are explicitly included as "non-commercial purposes".)
    -- Commercial uses of this software and its source code, including but not
    -- limited to documents for sale and sales of modified versions of this
    -- tool, are prohibited without the prior written permission of
    -- AXE Consultants. All rights not explicitly granted above are reserved
    -- by AXE Consultants.
    --
    -- You use this tool and/or its source code on the condition that you indemnify and hold harmless
    -- AXE Consultants, its agents, and employees, from any and all liability
    -- or damages to yourself or your hardware or software, or third parties,
    -- including attorneys' fees, court costs, and other related costs and
    -- expenses, arising out of your use of this tool and/or source code irrespective of the
    -- cause of said liability.
    --
    -- AXE CONSULTANTS MAKES THIS TOOL AND SOURCE CODE AVAILABLE ON AN "AS IS"
    -- BASIS AND MAKES NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE ACCURACY,
    -- CAPABILITY, EFFICIENCY, MERCHANTABILITY, OR FUNCTIONING OF THIS TOOL.
    -- IN NO EVENT WILL AXE CONSULTANTS BE LIABLE FOR ANY GENERAL,
    -- CONSEQUENTIAL, INDIRECT, INCIDENTAL, EXEMPLARY, OR SPECIAL DAMAGES,
    -- EVEN IF AXE CONSULTANTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH
    -- DAMAGES.
    -- ---------------------------------------
    --
    -- Edit History:
    --
    --  1/05/06 - RLB - Created base package to replace hard-coded main program.
    --  1/12/06 - RLB - Removed obsolete Document parameter.

    type Output_Format_Type is (HTML, RTF, Text, Corr, Info);

    procedure Read_and_Process_Master_File (
	File_Name : in String;
	The_Change_Kind : ARM_Format.Change_Kind; -- Changes to generate.
	The_Change_Version : ARM_Contents.Change_Version_Type; -- Change version.
        Output_Format : in Output_Format_Type);

	-- Read and process the master file given.

end ARM_Master;
