
 echo FUNCTION=RUNFILE                                           >  RUNFILE.INI

 echo PRJFILE_IN= d:\OSS\iMOD\TUTORIALS\TUT13_UZF\IMOD_USER\RUNFILES\model_uzf2.prj     >>  RUNFILE.INI
 echo NAMFILE_OUT=d:\OSS\iMOD\TUTORIALS\TUT13_uzf\IMOD_USER\MODELS\TUT13_uzf2\TUT13_uzf2.NAM >>  RUNFILE.INI

 ECHO SDATE=20131006000000 >> RUNFILE.INI
 ECHO EDATE=20150107000000 >> RUNFILE.INI
 ECHO ITT=2 >> RUNFILE.INI
 ECHO IDT=1 >> RUNFILE.INI
 ECHO ISS=1                                                     >>  RUNFILE.INI
 ECHO UNCONFINED=1                                                     >>  RUNFILE.INI
 ECHO NSTEP=1 >> RUNFILE.INI
 ECHO NMULT=1.0 >> RUNFILE.INI

rem echo isolve=0 >> runfile.ini
rem echo modflow=d:\OSS\iMOD\TUTORIALS\TUT10_SFR\iMODFLOW_V3_4_X32R.exe >> runfile.ini

 D:\OSS\IMOD\BIN\IMOD_X64_d RUNFILE.INI

pause