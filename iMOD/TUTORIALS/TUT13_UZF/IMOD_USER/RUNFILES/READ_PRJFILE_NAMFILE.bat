
 echo FUNCTION=RUNFILE                                           >  RUNFILE.INI

 echo PRJFILE_IN= d:\OSS\iMOD\TUTORIALS\TUT11_LAK\IMOD_USER\RUNFILES\model_uzf.prj      >>  RUNFILE.INI
 echo NAMFILE_OUT=d:\OSS\iMOD\TUTORIALS\TUT11_LAK\IMOD_USER\MODELS\TUT13_uzf\TUT13.NAM >>  RUNFILE.INI

 ECHO SDATE=20131006000000 >> RUNFILE.INI
 ECHO EDATE=20150107000000 >> RUNFILE.INI
 ECHO ITT=1 >> RUNFILE.INI
 ECHO IDT=1 >> RUNFILE.INI
 ECHO ISS=1                                                     >>  RUNFILE.INI

REM echo isolve=1 >> runfile.ini
REM echo modflow=d:\OSS\iMOD\TUTORIALS\TUT10_SFR\iMODFLOW_V3_4_X32R.exe >> runfile.ini

 D:\OSS\IMOD\BIN\IMOD_X64_d RUNFILE.INI

pause