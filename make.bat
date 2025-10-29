@echo off
if exist env.bat del env.bat
for /f "tokens=*" %%a in (env) do echo set "%%a">>env.bat
call env.bat
del env.bat

if not "%WATCOM_BIN_DIR%" == "" set WATCOM_BIN_DIR=%WATCOM_BIN_DIR%\

if "%1" == "db" goto :maketest
"%WATCOM_BIN_DIR%wmake" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto :quit

:maketest
"%WATCOM_BIN_DIR%wmake" %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 goto :quit

:test
if "%DOSBOX_BIN%" == "" goto :dostest
"%DOSBOX_BIN%" test.bat -exit -c mount . -conf dosbox.conf
goto :quit

:dostest
call test.bat
goto :quit

:quit
