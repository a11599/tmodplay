@echo off

rem DOS only!

rem Compiles and starts test.bat for quick testing. Requires wmake accessible
rem via PATH.

wmake
if errorlevel 1 goto :quit
call test.bat

:quit
