REM clean file
IF EXIST p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_rch\imodflow_v2653\ERROR.TXT DEL p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_rch\imodflow_v2653\ERROR.TXT
REM execute program
p:\1206764-imod\imod30\ontwikkeling\imodflow\_exe\iMODFLOW_X64_V2653_VMETASWAP7220.exe p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_rch\imodflow_v2653\imodflow.run
REM echo exitcode
IF ERRORLEVEL == 1 ECHO %ERRORLEVEL% > p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_rch\imodflow_v2653\error.txt
