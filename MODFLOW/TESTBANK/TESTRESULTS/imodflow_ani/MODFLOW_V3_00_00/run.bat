REM clean file
IF EXIST p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_ani\MODFLOW_V3_00_00\ERROR.TXT DEL p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_ani\MODFLOW_V3_00_00\ERROR.TXT
REM execute program
p:\1206764-imod\imod30\ontwikkeling\imodflow\_exe\MODFLOW_X64R_V3_0.exe p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_ani\MODFLOW_V3_00_00\imodflow.run
REM echo exitcode
IF ERRORLEVEL == 1 ECHO %ERRORLEVEL% > p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_ani\MODFLOW_V3_00_00\error.txt
