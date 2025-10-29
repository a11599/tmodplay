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

tmodplay = build build$(ps)$(build) build$(ps)$(build)$(ps)obj build$(ps)$(build)$(ps)obj$(ps)gui &
	build_mod &
	build$(ps)$(build)$(ps)tmodplay.exe

# PMI stub EXE file to link with the application

pmi = ..$(ps)pmi$(ps)build$(ps)$(build)$(ps)pmi.exe

# Validate build target environment value

build_ok = 0
!ifeq build debug
%log_level = debug
debug_objs = ..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)log.obj
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

# Build application

incremental: $(tmodplay)
full: clean clean_mod $(tmodplay)

# Create binary distribution package

dist: .SYMBOLIC
	$(watcom_bin_dir)wmake full
	$(watcom_bin_dir)wmake build=release full
	@if not exist dist mkdir dist
	@if not exist dist$(ps)debug mkdir dist$(ps)debug
	@if not exist dist$(ps)release mkdir dist$(ps)release
	@$(del) dist$(ps)debug$(ps)*.*
	@$(del) dist$(ps)release$(ps)*.*
	@$(del) dist$(ps)*.*
	@$(copy) build$(ps)debug$(ps)*.exe dist$(ps)debug
	@$(copy) build$(ps)release$(ps)*.exe dist$(ps)release
	@$(copy) tmodplay.txt dist

# Cleanup

clean: .SYMBOLIC .MULTIPLE
	@if exist build$(ps)$(build)$(ps)obj$(ps)gui $(del) build$(ps)$(build)$(ps)obj$(ps)gui$(ps)*.*
	@if exist build$(ps)$(build)$(ps)obj $(del) build$(ps)$(build)$(ps)obj$(ps)*.*
	@if exist build$(ps)$(build)$(ps)obj$(ps)gui rmdir build$(ps)$(build)$(ps)obj$(ps)gui
	@if exist build$(ps)$(build)$(ps)obj rmdir build$(ps)$(build)$(ps)obj


#------------------------------------------------------------------------------
# Build application
#------------------------------------------------------------------------------

# List of application objects

app_objs = &
	..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)string.obj &
	..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)env_arg.obj &
	..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)irq.obj &
	..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)timer.obj &
	..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)systimer.obj &
	..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)keyboard.obj &
	..$(ps)pmi$(ps)build$(ps)$(build)$(ps)rtl$(ps)profiler.obj &
	..$(ps)mod$(ps)build$(ps)$(build)$(ps)mod$(ps)convert.obj &
	..$(ps)mod$(ps)build$(ps)$(build)$(ps)mod$(ps)dev_dac.obj &
	..$(ps)mod$(ps)build$(ps)$(build)$(ps)mod$(ps)dev_none.obj &
	..$(ps)mod$(ps)build$(ps)$(build)$(ps)mod$(ps)dev_sb.obj &
	..$(ps)mod$(ps)build$(ps)$(build)$(ps)mod$(ps)player.obj &
	..$(ps)mod$(ps)build$(ps)$(build)$(ps)mod$(ps)routine.obj &
	..$(ps)mod$(ps)build$(ps)$(build)$(ps)mod$(ps)wtbl_sw.obj &
	build$(ps)$(build)$(ps)obj$(ps)gui$(ps)setup.obj &
	build$(ps)$(build)$(ps)obj$(ps)gui$(ps)draw.obj &
	build$(ps)$(build)$(ps)obj$(ps)tmodplay.obj

# Abort if unknown build environment is given

abort:
	echo "$(build)" is not a valid build target.
	@%abort

# Create directory for binary files

build: .SYMBOLIC .ALWAYS
	@if not exist build mkdir build

build$(ps)$(build): build .SYMBOLIC .ALWAYS
	@if not exist build$(ps)$(build) mkdir build$(ps)$(build)

build$(ps)$(build)$(ps)obj: build$(ps)$(build) .SYMBOLIC .ALWAYS
	@if not exist build$(ps)$(build)$(ps)obj mkdir build$(ps)$(build)$(ps)obj

build$(ps)$(build)$(ps)obj$(ps)gui: build$(ps)$(build)$(ps)obj .SYMBOLIC .ALWAYS
	@if not exist build$(ps)$(build)$(ps)obj$(ps)gui mkdir build$(ps)$(build)$(ps)obj$(ps)gui

# Build MOD player library

build_mod: .SYMBOLIC
	@cd ..$(ps)mod
	@$(watcom_bin_dir)wmake build=$(build)
	@cd ..$(ps)tmodplay

clean_mod: .SYMBOLIC
	@cd ..$(ps)mod
	@$(watcom_bin_dir)wmake build=$(build) clean
	@cd ..$(ps)tmodplay

# Binary build and link

build$(ps)$(build)$(ps)tmodplay.exe: $(app_objs) $(debug_objs) build$(ps)$(build)
	@%create build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk
	@%write build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk NAME build$(ps)$(build)$(ps)tmodplay
	@%write build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk OPTION map=build$(ps)$(build)$(ps)obj$(ps)tmodplay.map
	@%write build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk OPTION stub=$(pmi)
	@%write build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk OPTION start=_main
	@%write build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk OPTION stack=4096
	@%write build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk FORM Windows NT TNT
	@%write build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk FILE {$(app_objs) $(debug_objs)}
	$(watcom_bin_dir)wlink @build$(ps)$(build)$(ps)obj$(ps)tmodplay.lnk

# .inc file dependencies

src$(ps)gui$(ps)api$(ps)gui.inc: &
	src$(ps)gui$(ps)consts$(ps)public.inc

	$(watcom_bin_dir)wtouch src$(ps)gui$(ps)api$(ps)gui.inc

# .obj file dependencies with included external files and build instructions

build$(ps)$(build)$(ps)obj$(ps)gui$(ps)setup.obj: src$(ps)gui$(ps)setup.asm &
	..$(ps)pmi$(ps)src$(ps)pmi$(ps)api$(ps)pmi.inc &
	..$(ps)pmi$(ps)src$(ps)rtl$(ps)api$(ps)string.inc &
	..$(ps)pmi$(ps)src$(ps)rtl$(ps)api$(ps)log.inc &
	src$(ps)gui$(ps)config.inc &
	src$(ps)gui$(ps)consts$(ps)public.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)obj$(ps)gui$(ps)draw.obj: src$(ps)gui$(ps)draw.asm &
	..$(ps)pmi$(ps)src$(ps)pmi$(ps)api$(ps)pmi.inc &
	..$(ps)pmi$(ps)src$(ps)rtl$(ps)api$(ps)string.inc &
	..$(ps)pmi$(ps)src$(ps)rtl$(ps)api$(ps)log.inc &
	src$(ps)gui$(ps)config.inc &
	src$(ps)gui$(ps)api$(ps)setup.inc &
	src$(ps)gui$(ps)consts$(ps)public.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@

build$(ps)$(build)$(ps)obj$(ps)tmodplay.obj: src$(ps)tmodplay.asm &
	..$(ps)pmi$(ps)src$(ps)pmi$(ps)api$(ps)pmi.inc &
	..$(ps)pmi$(ps)src$(ps)rtl$(ps)api$(ps)env_arg.inc &
	..$(ps)pmi$(ps)src$(ps)rtl$(ps)api$(ps)string.inc &
	..$(ps)pmi$(ps)src$(ps)rtl$(ps)api$(ps)log.inc &
	src$(ps)gui$(ps)api$(ps)gui.inc &
	..$(ps)mod$(ps)src$(ps)mod$(ps)api$(ps)mod.inc &
	src$(ps)fonts$(ps)rpgsys.inc &
	src$(ps)fonts$(ps)sgk075.inc &
	src$(ps)fonts$(ps)digits.inc

	$(nasm_bin) $(nasm_pe_opts) $[@ -o $^@
