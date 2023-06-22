#------------------------------------------------------------------------------
# Therapy MOD player makefile
#------------------------------------------------------------------------------

# Compiler options

nasm_pe_opts = -i "src" -i "../pmi/src" -i "../mod/src" -f win32

# Build mode
# Set to "release" in command line parameter to create a release build.
# Example for full recompilation of the release version:
# wmake build=release full

build = debug

# Target application EXE file without extension

tmodplay = build build\$(build) build\$(build)\obj build\$(build)\obj\gui &
	build_mod &
	build\$(build)\tmodplay.exe

# PMI stub EXE file to link with the application

pmi = ..\pmi\build\$(build)\pmi.exe

# Validate build target environment value

build_ok = 0
!ifeq build debug
%log_level = debug
debug_objs = ..\pmi\build\$(build)\rtl\log.obj
build_ok = 1
!endif
!ifeq build release
%log_level =
debug_objs =
build_ok = 1
!endif
!ifneq build_ok 1
pmi = abort
rtl = abort
!endif

# Append \ at the end of nasm/watcom path variables if not empty

!ifneq nasm_dir
nasm_dir = $(nasm_dir)\
!endif
!ifneq watcom_dir
watcom_dir = $(watcom_dir)\
!endif

# Build application

incremental: $(tmodplay)
full: clean clean_mod $(tmodplay)

# Create binary distribution package

dist: .SYMBOLIC
	$(watcom_dir)wmake full
	$(watcom_dir)wmake build=release full
	@if not exist dist mkdir dist
	@if not exist dist\debug mkdir dist\debug
	@if not exist dist\release mkdir dist\release
	@del /q dist\debug\*.*
	@del /q dist\release\*.*
	@del /q dist\*.*
	@copy build\debug\*.exe dist\debug
	@copy build\release\*.exe dist\release
	@copy tmodplay.txt dist

# Cleanup

clean: .SYMBOLIC .MULTIPLE
	@if exist build\$(build)\obj\gui del /q build\$(build)\obj\gui\*.*
	@if exist build\$(build)\obj del /q build\$(build)\obj\*.*
	@if exist build\$(build)\obj\gui rmdir build\$(build)\obj\gui
	@if exist build\$(build)\obj rmdir build\$(build)\obj


#------------------------------------------------------------------------------
# Build application
#------------------------------------------------------------------------------

# List of application objects

app_objs = &
	..\pmi\build\$(build)\rtl\string.obj &
	..\pmi\build\$(build)\rtl\env_arg.obj &
	..\pmi\build\$(build)\rtl\irq.obj &
	..\pmi\build\$(build)\rtl\timer.obj &
	..\pmi\build\$(build)\rtl\systimer.obj &
	..\pmi\build\$(build)\rtl\keyboard.obj &
	..\pmi\build\$(build)\rtl\profiler.obj &
	..\mod\build\$(build)\mod\convert.obj &
	..\mod\build\$(build)\mod\dev_dac.obj &
	..\mod\build\$(build)\mod\dev_none.obj &
	..\mod\build\$(build)\mod\dev_sb.obj &
	..\mod\build\$(build)\mod\player.obj &
	..\mod\build\$(build)\mod\routine.obj &
	..\mod\build\$(build)\mod\wtbl_sw.obj &
	build\$(build)\obj\gui\setup.obj &
	build\$(build)\obj\gui\draw.obj &
	build\$(build)\obj\tmodplay.obj

# Abort if unknown build environment is given

abort:
	echo "$(build)" is not a valid build target.
	@%abort

# Create directory for binary files

build: .SYMBOLIC .ALWAYS
	@if not exist build mkdir build

build\$(build): build .SYMBOLIC .ALWAYS
	@if not exist build\$(build) mkdir build\$(build)

build\$(build)\obj: build\$(build) .SYMBOLIC .ALWAYS
	@if not exist build\$(build)\obj mkdir build\$(build)\obj

build\$(build)\obj\gui: build\$(build)\obj .SYMBOLIC .ALWAYS
	@if not exist build\$(build)\obj\gui mkdir build\$(build)\obj\gui

# Build MOD player library

build_mod: .SYMBOLIC
	@cd ..\mod
	@wmake build=$(build)
	@cd ..\tmodplay

clean_mod: .SYMBOLIC
	@cd ..\mod
	@wmake build=$(build) clean
	@cd ..\tmodplay

# Binary build and link

build\$(build)\tmodplay.exe: $(app_objs) $(debug_objs) build\$(build)
	@%create build\$(build)\obj\tmodplay.lnk
	@%write build\$(build)\obj\tmodplay.lnk NAME build\$(build)\tmodplay
	@%write build\$(build)\obj\tmodplay.lnk OPTION map=build\$(build)\obj\tmodplay.map
	@%write build\$(build)\obj\tmodplay.lnk OPTION stub=$(pmi)
	@%write build\$(build)\obj\tmodplay.lnk OPTION start=_main
	@%write build\$(build)\obj\tmodplay.lnk OPTION stack=4096
	@%write build\$(build)\obj\tmodplay.lnk FORM Windows NT TNT
	@%write build\$(build)\obj\tmodplay.lnk FILE {$(app_objs) $(debug_objs)}
	$(watcom_dir)wlink @build\$(build)\obj\tmodplay.lnk

# .inc file dependencies

src\gui\api\gui.inc: &
	src\gui\consts\public.inc

	$(watcom_dir)wtouch src\gui\api\gui.inc

# .obj file dependencies with included external files and build instructions

build\$(build)\obj\gui\setup.obj: src\gui\setup.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\gui\config.inc &
	src\gui\consts\public.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\obj\gui\draw.obj: src\gui\draw.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\gui\config.inc &
	src\gui\api\setup.inc &
	src\gui\consts\public.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@

build\$(build)\obj\tmodplay.obj: src\tmodplay.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\env_arg.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\gui\api\gui.inc &
	..\mod\src\mod\api\mod.inc &
	src\fonts\rpgsys.inc &
	src\fonts\sgk075.inc &
	src\fonts\digits.inc

	$(nasm_dir)nasm $(nasm_pe_opts) $[@ -o $^@
