REM clean file
IF EXIST p:\1206764-imod\imod30\testbank\output_V3_00_02\runs\test_ALL\imodflow_drn\MODFLOW_V3_00_02\ERROR.TXT DEL p:\1206764-imod\imod30\testbank\output_V3_00_02\runs\test_ALL\imodflow_drn\MODFLOW_V3_00_02\ERROR.TXT
REM execute program
p:\1206764-imod\imod30\ontwikkeling\imodflow\_exe\MODFLOW_V3_00_02_METASWAP_SVN1004_X64R.exe p:\1206764-imod\imod30\testbank\output_V3_00_02\runs\test_ALL\imodflow_drn\MODFLOW_V3_00_02\imodflow.run
REM echo exitcode
IF ERRORLEVEL == 1 ECHO %ERRORLEVEL% > p:\1206764-imod\imod30\testbank\output_V3_00_02\runs\test_ALL\imodflow_drn\MODFLOW_V3_00_02\error.txt
