@echo off

rem Compiles and starts test.bat using DOSBox-X for quick testing. Requires
rem wmake accessible via PATH and DOSBox-X in C:\DOSBox-X directory.

wmake
C:\DOSBox-X\dosbox-x.exe test.bat -c mount . -conf emu\dosbox-x.conf
