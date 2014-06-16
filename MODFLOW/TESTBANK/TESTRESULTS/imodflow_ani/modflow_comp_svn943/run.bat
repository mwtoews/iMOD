REM clean file
IF EXIST p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_ani\modflow_comp_svn943\ERROR.TXT DEL p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_ani\modflow_comp_svn943\ERROR.TXT
REM execute program
p:\1206764-imod\imod30\ontwikkeling\imodflow\_exe\driver_win32_svn943.exe p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_ani\modflow_comp_svn943\imodflow.run
REM echo exitcode
IF ERRORLEVEL == 1 ECHO %ERRORLEVEL% > p:\1206764-imod\imod30\testbank\output\runs\test_ALL\imodflow_ani\modflow_comp_svn943\error.txt
