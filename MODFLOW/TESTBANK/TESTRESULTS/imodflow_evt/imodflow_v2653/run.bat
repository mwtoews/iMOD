REM clean file
IF EXIST p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_evt\imodflow_v2653\ERROR.TXT DEL p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_evt\imodflow_v2653\ERROR.TXT
REM execute program
p:\1206764-imod\imod30\ontwikkeling\imodflow\_exe\iMODFLOW_br.exe p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_evt\imodflow_v2653\imodflow.run
REM echo exitcode
IF ERRORLEVEL == 1 ECHO %ERRORLEVEL% > p:\1206764-imod\imod30\testbank\output_V3_00_01\runs\test_ALL\imodflow_evt\imodflow_v2653\error.txt
