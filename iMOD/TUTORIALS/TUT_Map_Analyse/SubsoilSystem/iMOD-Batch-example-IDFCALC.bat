REM - Example of a DOS script to create and execute an iMOD Batch function
REM - This function calculates the thickness of a series of 6 aquifers
rem - check the version number of the iMOD executable

SETLOCAL ENABLEDELAYEDEXPANSION

REM create the iMOD Batch file IDFCALC.INI
FOR /L %%A IN (1,1,6) DO (
  ECHO FUNCTION=IDFCALC > IDFCALC.INI
  ECHO FUNC=C=A-B >> IDFCALC.INI
  ECHO NREPEAT=1 >> IDFCALC.INI
  ECHO ABC1=TOP_SDL%%A_M.IDF BOT_SDL%%A_M.IDF T_AQUIFER%%A.IDF >> IDFCALC.INI

  REM execute the iMOD Batch file using the iMOD executable
  ..\..\..\IMOD_V3_6_X64R.EXE IDFCALC.INI
)

rem Some DOS explanation
rem - a line starting with REM or :: are skipped by DOS
rem - the loop "FOR /L %%A IN (1,2,6) DO (" loops over number 1 to 6 with step 2
rem - the loop "FOR    %%A IN (1,2,6) DO (" loops over text 1, 2 and 6
rem - ECHO will write the line to the screen
rem - for ECHO to a    new    file use ">  IDFCALC.INI"
rem - for ECHO to an existing file use ">> IDFCALC.INI"

