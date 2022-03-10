@echo off

rem Just a shortcut to start DOSBox-X in the same environment/settings as
rem makedb.bat. Requires DOSBox-X in C:\DOSBox-X directory.

pushd "%~dp0"
call env.bat
"%dosbox%" -c mount .. -conf dosbox-x.conf
popd
