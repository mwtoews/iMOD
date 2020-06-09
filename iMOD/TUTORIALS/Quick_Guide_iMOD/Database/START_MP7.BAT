set imod=d:\COMPILE\OSSDELTARES\iMOD\VS2017\iMOD\x64\debug\iMOD_x64_D.exe
set modpath7=d:\COMPILE\MODPATH7\MODPATH7\MODPATH7\x64\Release\MODPATH7.exe

echo FUNCTION=MODPATH7                     >  MODPATH7.INI
ECHO MF6DIR=d:\COMPILE\ossdeltares\iMOD\TUTORIALS\Quick_Guide_iMOD\Database\results\MF6\single              >> MODPATH7.INI
ECHO MODPATH7=%modpath7%                  >> MODPATH7.INI
ECHO OUTDIR=d:\COMPILE\ossdeltares\iMOD\TUTORIALS\Quick_Guide_iMOD\Database\results\MF6\mf7          >> MODPATH7.INI
ECHO ISDNAME=d:\COMPILE\ossdeltares\iMOD\TUTORIALS\Quick_Guide_iMOD\Database\teaser10.isd >> MODPATH7.INI
ECHO NLAY=37                              >> MODPATH7.INI
FOR /L %%L IN (1,2,37) DO (
 ECHO POR_L%%L=0.30                       >> MODPATH7.INI
)
FOR /L %%L IN (2,2,37) DO (
 ECHO POR_L%%L=0.10                       >> MODPATH7.INI
)

%imod% MODPATH7.INI

pause