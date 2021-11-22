@echo off
setlocal enableextensions enabledelayedexpansion
call :setESC

rem Set output parameters

set "outdir=dist"
set "outfile=tmodplay"
set "outformat=dos"
set "postbuild=test.cmd"
set "DEBUG_BUILD=1"

pushd

rem Determine source folder for relative paths

cd "%~dp0"
set "rootdir=%~dp0"
set "rootdir=%rootdir:/=\%"
if "%rootdir:~-1%" == "\" set "rootdir=%rootdir:~0,-1%"
set "foo=%rootdir%"
set cut=

:loop
if defined foo (
    set /A cut+=1
    set "foo=!foo:~1!"
    goto :loop
)

rem also remove leading /
set /A cut+=1

rem Compile .asm files into the obj folder and generate linker response file

echo %ESC%[92mCompiling sources...%ESC%[0m

:compile
mkdir obj >nul 2>nul
del /q "%rootdir%\obj\link.tmp" >nul 2>nul
set "separator="
for /R "%rootdir%\src" %%F in (*) do (

    rem Get relative paths of .asm and .obj files

    set "S=%%~fF"
    set "S=!S:~%cut%!"
    set "E=!S:~-4!"
    set "O=%%~dpnF"
    set "O=!O:~%cut%!"
    set "O=obj\!O:~4!.obj"
    set "F=%%~dpF"
    set "F=!F:~%cut%!"
    set "R=!F:~4,6!"
    set "F=obj\!F:~4!"

    rem Build only .asm files except in folder debug when not in debug mode

    set "build=no"
    if /I not "!R!" == "debug\" set "build=yes"
    if defined DEBUG_BUILD set "build=yes"
    if /I not "!E!" == ".asm" set "build=no"

    if "!build!" == "yes" (

        rem Create target directory

        mkdir !F! >nul 2>nul

        rem Compile .asm file

        echo !S! -^> !O!
        c:/Programs/asm/nasm/nasm.exe -i "%rootdir%\src" -f obj "!S!" -o "!O!"
        if errorlevel 1 goto error

        set "link=no"
        if /I not "!R!" == "debug\" set "link=yes"
        if defined DEBUG_BUILD set "link=yes"

        <nul set /p ="!separator!!O!">>"%rootdir%\obj\link.tmp"

        rem Add a comma separator between .obj file entries

        set "separator=,"
    )
)
echo. >>"%rootdir%\obj\link.tmp"

rem Link object files compiled in previous step and external .obj files from
rem optional link.lst file

echo.
echo %ESC%[92mLinking...%ESC%[0m

set "binfile=%outfile%"
if [%outdir%]==[] goto link
mkdir "%outdir%" >nul 2>nul
set "binfile=%outdir%\%outfile%"

:link
copy "%rootdir%\obj\link.tmp" "%rootdir%\obj\link.lst" >nul 2>nul
if exist "%rootdir%\link.lst" copy "%rootdir%\obj\link.tmp"+"%rootdir%\link.lst" "%rootdir%\obj\link.lst" >nul 2>nul
del "%rootdir%\obj\link.tmp" >nul 2>nul
c:/Programs/asm/jwlink\JWlink.exe name "%binfile%" file @"%rootdir%\obj\link.lst" option dosseg option map="obj\%outfile%.map" option packcode=0 option packdata=0 form %outformat%
if errorlevel 1 goto error

rem Build complete, run optional postbuild task

echo.
echo %ESC%[102m%ESC%[30m OK %ESC%[0m

if [%postbuild%]==[] goto quit
%postbuild%
goto quit

rem Build failed

:error
echo.
echo %ESC%[101;93m BUILD FAILED %ESC%[0m

rem Exit

:quit
popd

rem Setup ESC variable for colored terminal output on Windows 10

:setESC
for /F "tokens=1,2 delims=#" %%a in ('"prompt #$H#$E# & echo on & for %%b in (1) do rem"') do (
  set ESC=%%b
  exit /B 0
)
exit /B 0
