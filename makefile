#------------------------------------------------------------------------------
# Therapy MOD player makefile
#------------------------------------------------------------------------------

# Compiler options

nasm_pe_opts = -i "src" -i "../pmi/src" -f win32

# Target file names and directories

dist_bin = bin				# Binary EXE file target directory

# PMI stub EXE file to link with the application

stub = ..\pmi\bin\pmi.exe
stub_dbg = ..\pmi\bin\pmi_dbg.exe

# Target application EXE file without extension

app = tmodplay
app_dbg = tmod_dbg

# Build targets
# debug (default): Incremental build with debugging enabled.
# release: Clean build without debugging. Deletes object files after the build.

debug: enable_debug $(dist_bin)\$(app_dbg).exe
release: clean $(dist_bin)\$(app).exe clean_app_obj


#------------------------------------------------------------------------------
# Build application
#------------------------------------------------------------------------------

# List of application objects

app_objs = &
	..\pmi\rtl\string.obj &
	..\pmi\rtl\env_arg.obj &
	..\pmi\rtl\irq.obj &
	..\pmi\rtl\timer.obj &
	..\pmi\rtl\systimer.obj &
	..\pmi\rtl\keyboard.obj &
	..\pmi\rtl\profiler.obj &
	obj\gui\setup.obj &
	obj\gui\draw.obj &
	obj\mod\convert.obj &
	obj\mod\dev_dac.obj &
	obj\mod\dev_none.obj &
	obj\mod\dev_sb.obj &
	obj\mod\player.obj &
	obj\mod\routine.obj &
	obj\mod\wtbl_sw.obj &
	obj\tmodplay.obj

debug_objs = &
	..\pmi\rtl\log.obj

# Enable debug build

enable_debug: .SYMBOLIC
	set LOG_LEVEL=DEBUG

# Cleanup

clean: clean_app_obj .SYMBOLIC
	set LOG_LEVEL=
	if exist $(dist_bin)\$(app).exe del $(dist_bin)\$(app).exe

clean_app_obj: .SYMBOLIC .MULTIPLE
	if exist obj del obj\*.obj >nul
	if exist obj\mod del obj\mod\*.obj >nul
	if exist obj\gui del obj\gui\*.obj >nul

# Binary build and link

$(dist_bin)\$(app_dbg).exe: obj $(app_objs) $(debug_objs)
	@if not exist $(dist_bin) mkdir $(dist_bin)
	@%write obj\$(app_dbg).lnk NAME $(dist_bin)\$(app_dbg)
	@%write obj\$(app_dbg).lnk OPTION map=obj\$(app_dbg).map
	@%write obj\$(app_dbg).lnk OPTION stub=$(stub_dbg)
	@%write obj\$(app_dbg).lnk OPTION start=_main
	@%write obj\$(app_dbg).lnk OPTION stack=4096
	@%write obj\$(app_dbg).lnk FORM Windows NT TNT
	@%write obj\$(app_dbg).lnk FILE {$(app_objs) $(debug_objs)}
	$(wlink) @obj\$(app_dbg).lnk

$(dist_bin)\$(app).exe: obj $(app_objs) $(debug_objs)
	@if not exist $(dist_bin) mkdir $(dist_bin)
	@%write obj\$(app).lnk NAME $(dist_bin)\$(app)
	@%write obj\$(app).lnk OPTION map=obj\$(app).map
	@%write obj\$(app).lnk OPTION stub=$(stub)
	@%write obj\$(app).lnk OPTION start=_main
	@%write obj\$(app).lnk OPTION stack=4096
	@%write obj\$(app).lnk FORM Windows NT TNT
	@%write obj\$(app).lnk FILE {$(app_objs)}
	$(wlink) @obj\$(app).lnk

# Create obj directory for .obj files

obj: .SYMBOLIC .ALWAYS
	@if not exist obj mkdir obj
	@if not exist obj\mod mkdir obj\mod
	@if not exist obj\gui mkdir obj\gui

# .inc file dependencies

src\gui\api\gui.inc: &
	src\gui\consts\public.inc

	wtouch src\gui\api\gui.inc

src\mod\api\mod.inc: &
	src\mod\consts\public.inc &
	src\mod\structs\public.inc

	wtouch src\mod\api\mod.inc

# .obj file dependencies with included external files and build instructions

obj\gui\setup.obj: src\gui\setup.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\gui\config.inc &
	src\gui\consts\public.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\gui\draw.obj: src\gui\draw.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\gui\config.inc &
	src\gui\api\setup.inc &
	src\gui\consts\public.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\mod\convert.obj: src\mod\convert.asm

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\mod\dev_dac.obj: src\mod\dev_dac.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	..\pmi\src\rtl\api\irq.inc &
	..\pmi\src\rtl\api\timer.inc &
	src\mod\config.inc &
	src\mod\api\wtbl_sw.inc &
	src\mod\api\routine.inc &
	src\mod\structs\public.inc &
	src\mod\consts\public.inc &
	src\mod\structs\dev.inc &
	src\mod\consts\dev.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\mod\dev_none.obj: src\mod\dev_none.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	..\pmi\src\rtl\api\irq.inc &
	..\pmi\src\rtl\api\timer.inc &
	src\mod\api\routine.inc &
	src\mod\structs\dev.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\mod\dev_sb.obj: src\mod\dev_sb.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\env_arg.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	..\pmi\src\rtl\api\irq.inc &
	src\mod\config.inc &
	src\mod\api\wtbl_sw.inc &
	src\mod\api\routine.inc &
	src\mod\structs\public.inc &
	src\mod\consts\public.inc &
	src\mod\structs\dev.inc &
	src\mod\consts\dev.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\mod\player.obj: src\mod\player.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\env_arg.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\mod\config.inc &
	src\mod\api\convert.inc &
	src\mod\api\routine.inc &
	src\mod\structs\public.inc &
	src\mod\consts\public.inc &
	src\mod\structs\mod_file.inc &
	src\mod\structs\dev.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\mod\routine.obj: src\mod\routine.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\mod\config.inc &
	src\mod\api\convert.inc &
	src\mod\structs\public.inc &
	src\mod\structs\mod_file.inc &
	src\mod\consts\dev.inc &
	src\mod\structs\dev.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\mod\wtbl_sw.obj: src\mod\wtbl_sw.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\mod\config.inc &
	src\mod\structs\public.inc &
	src\mod\consts\dev.inc &
	src\mod\structs\mod_file.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@

obj\tmodplay.obj: src\tmodplay.asm &
	..\pmi\src\pmi\api\pmi.inc &
	..\pmi\src\rtl\api\env_arg.inc &
	..\pmi\src\rtl\api\string.inc &
	..\pmi\src\rtl\api\log.inc &
	src\gui\api\gui.inc &
	src\mod\api\mod.inc &
	src\fonts\rpgsys.inc &
	src\fonts\sgk075.inc &
	src\fonts\digits.inc

	$(nasm) $(nasm_pe_opts) $[@ -o $^@
