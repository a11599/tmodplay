# Object group macros

debugobjs = obj\debug\log.obj
sysobjs = obj\system\dma.obj obj\system\env.obj obj\system\file.obj obj\system\memory.obj obj\system\pic.obj obj\system\pit.obj obj\system\string.obj
modobjs = obj\mod\convert.obj obj\mod\loader.obj obj\mod\out_dac.obj obj\mod\out_none.obj obj\mod\out_sb.obj obj\mod\player.obj obj\mod\routine.obj obj\mod\wtbl_sw.obj
appobjs = obj\tmodplay.obj

# Compiler options

nasmopts = -i "src" -f obj

# Compilation target

dest = dist\tmodplay

# Build targets
# debug (default): build with debug logging enabled, keeps object files
# release: build for release, also cleans up obj directory

debug: debugbuild $(dest).exe
release: clean $(dest).exe distclean

# Enable debug build

debugbuild: .SYMBOLIC
	set DEBUG_BUILD=1

# Cleanup

clean: cleanobj .SYMBOLIC
	set DEBUG_BUILD=
	if exist $(dest).exe del $(dest).exe

cleanobj: .SYMBOLIC
	if exist obj rmdir /s /q obj

distclean: .SYMBOLIC
	del /s /q obj\*.obj >nul

# Binary build and link

$(dest).exe: obj $(debugobjs) $(sysobjs) $(modobjs) $(appobjs)
	if not exist dist mkdir dist
	%write obj\tmodplay.lnk NAME $(dest)
	%write obj\tmodplay.lnk OPTION dosseg
	%write obj\tmodplay.lnk OPTION map=obj\tmodplay.map
	%write obj\tmodplay.lnk OPTION packcode=0
	%write obj\tmodplay.lnk OPTION packdata=0
	%write obj\tmodplay.lnk FORM dos
	%write obj\tmodplay.lnk FILE {$(debugobjs)}
	%write obj\tmodplay.lnk FILE {$(sysobjs)}
	%write obj\tmodplay.lnk FILE {$(modobjs)}
	%write obj\tmodplay.lnk FILE {$(appobjs)}
	$(wlink) @obj\tmodplay.lnk

# Create obj directory for .obj files

obj: .SYMBOLIC .ALWAYS
	if not exist obj\debug mkdir obj\debug
	if not exist obj\mod mkdir obj\mod
	if not exist obj\system mkdir obj\system

# .inc file dependencies

src\debug\log.inc: &
	src\debug\global.inc

	wtouch src\debug\log.inc

src\mod\api\player.inc: &
	src\mod\consts\public.inc &
	src\mod\structs\public.inc

	wtouch src\mod\api\player.inc

src\mod\structs\global.inc: &
	src\mod\consts\global.inc &
	src\mod\structs\public.inc &
	src\mod\structs\out.inc &
	src\mod\structs\routine.inc &
	src\mod\structs\out_none.inc &
	src\mod\structs\out_dac.inc &
	src\mod\structs\out_sb.inc &
	src\mod\structs\wtbl_sw.inc

	wtouch src\mod\structs\global.inc

src\mod\structs\routine.inc: &
	src\mod\consts\global.inc

	wtouch src\mod\structs\routine.inc

src\mod\structs\wtbl_sw.inc: &
	src\mod\consts\global.inc

	wtouch src\mod\structs\wtbl_sw.inc

src\system\api\dma.inc: &
	src\system\consts\public.inc

	wtouch src\system\api\dma.inc

src\system\api\memory.inc: &
	src\system\consts\public.inc

	wtouch src\system\api\memory.inc

# .obj file dependencies with included external files and build instructions

obj\debug\log.obj: src\debug\log.asm &
	src\debug\global.inc &
	src\system\api\string.inc &
	src\system\api\file.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\convert.obj: src\mod\convert.asm &
	src\mod\structs\global.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\loader.obj: src\mod\loader.asm &
	src\system\api\memory.inc &
	src\mod\consts\public.inc &
	src\mod\consts\global.inc &
	src\mod\structs\global.inc &
	src\mod\api\convert.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\out_dac.obj: src\mod\out_dac.asm &
	src\system\api\memory.inc &
	src\system\api\pit.inc &
	src\system\api\pic.inc &
	src\mod\consts\global.inc &
	src\mod\consts\public.inc &
	src\mod\structs\global.inc &
	src\mod\structs\out_dac.inc &
	src\mod\consts\out.inc &
	src\mod\consts\out_dac.inc &
	src\mod\api\wtbl_sw.inc &
	src\mod\api\routine.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\out_none.obj: src\mod\out_none.asm &
	src\system\api\memory.inc &
	src\system\api\pit.inc &
	src\system\api\pic.inc &
	src\mod\consts\global.inc &
	src\mod\consts\public.inc &
	src\mod\structs\global.inc &
	src\mod\structs\out_none.inc &
	src\mod\consts\out.inc &
	src\mod\api\routine.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\out_sb.obj: src\mod\out_sb.asm &
	src\system\api\memory.inc &
	src\system\api\pic.inc &
	src\system\api\dma.inc &
	src\system\api\env.inc &
	src\system\api\string.inc &
	src\mod\consts\global.inc &
	src\mod\consts\public.inc &
	src\mod\structs\global.inc &
	src\mod\structs\out_sb.inc &
	src\mod\consts\out.inc &
	src\mod\consts\out_sb.inc &
	src\mod\api\convert.inc &
	src\mod\api\wtbl_sw.inc &
	src\mod\api\routine.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\player.obj: src\mod\player.asm &
	src\system\api\memory.inc &
	src\mod\consts\public.inc &
	src\mod\consts\player.inc &
	src\mod\structs\global.inc &
	src\mod\structs\player.inc &
	src\mod\api\loader.inc &
	src\mod\api\routine.inc &
	src\mod\api\out_none.inc &
	src\mod\api\out_dac.inc &
	src\mod\api\out_sb.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\routine.obj: src\mod\routine.asm &
	src\mod\structs\global.inc &
	src\mod\consts\routine.inc &
	src\mod\consts\out.inc &
	src\mod\api\convert.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\mod\wtbl_sw.obj: src\mod\wtbl_sw.asm &
	src\system\api\memory.inc &
	src\mod\structs\global.inc &
	src\mod\consts\out.inc &
	src\mod\consts\wtbl_sw.inc &
	src\mod\structs\wtbl_sw.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\system\dma.obj: src\system\dma.asm &
	src\system\structs\dma.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\system\env.obj: src\system\env.asm &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\system\file.obj: src\system\file.asm &
	src\system\api\memory.inc &
	src\system\consts\file.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\system\memory.obj: src\system\memory.asm &
	src\system\consts\public.inc &
	src\system\consts\memory.inc &
	src\system\structs\memory.inc &
	src\system\api\pic.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\system\pic.obj: src\system\pic.asm &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\system\pit.obj: src\system\pit.asm &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\system\string.obj: src\system\string.asm &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@

obj\tmodplay.obj: src\tmodplay.asm &
	src\system\api\memory.inc &
	src\system\api\file.inc &
	src\system\api\env.inc &
	src\system\api\string.inc &
	src\mod\api\player.inc &
	src\debug\log.inc

	$(nasm) $(nasmopts) $[@ -o $^@
