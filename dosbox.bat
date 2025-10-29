@echo off
if exist env.bat del env.bat
for /f "tokens=*" %%a in (env) do echo set "%%a">>env.bat
call env.bat
del env.bat

"%DOSBOX_BIN%" -c mount . -conf dosbox.conf
