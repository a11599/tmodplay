@echo off

rem Compiles and starts test.bat using DOSBox-X for quick testing. Requires
rem wmake accessible via PATH and DOSBox-X in C:\DOSBox-X directory.

wmake
if errorlevel 1 goto :quit
call emu\env.bat
"%dosbox%" test.bat -exit -c mount . -conf emu\dosbox-x.conf

:quit
