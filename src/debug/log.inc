;==============================================================================
; Debug logging tools during development
;------------------------------------------------------------------------------
; -> DEBUG_BUILD environment variable: logging is only generated if it exists
;    If the environment variable is not defined, no debug-related code will be
;    added to the code.
;------------------------------------------------------------------------------
; All macros can be called at any point in the code, they don't alter flags or
; registers. Macros emit a far call at the point of inclusion (taking 5 bytes)
; into the current segment. The logging code itself will be output to the
; "debug" segment and log messages (via the log macro) are put into the
; "debug_messages" segment.
;==============================================================================

cpu 386


;------------------------------------------------------------------------------
; Log formatted string to stdout.
;------------------------------------------------------------------------------
; -> %1 - Source string literal for sys_str_format
;    %2... - Placeholder values. Can't be CS, (E)BP or (E)SP. Memory addresses
;            cannot use the CS: prefix.
;------------------------------------------------------------------------------

%macro	log 1-*
	%ifenv DEBUG_BUILD

	[section debug_messages]

%%message	db %1, 0

	[section debug]

%%print:
	pushf
	push si
	push bp
	mov bp, sp

	%rotate 1			; Get rid of format source string
	%rep (%0 - 1)			; Push placeholder values to stack

	; 8-bit register

	%ifidni %1, al
	push eax
	%elifidni %1, ah
	xchg ah, al
	push eax
	xchg ah, al
	%elifidni %1, bl
	push ebx
	%elifidni %1, bh
	xchg bh, bl
	push ebx
	xchg bh, bl
	%elifidni %1, cl
	push ecx
	%elifidni %1, ch
	xchg ch, cl
	push ecx
	xchg ch, cl
	%elifidni %1, dl
	push edx
	%elifidni %1, dh
	xchg dh, dl
	push edx
	xchg dh, dl

	; 16-bit register

	%elifidni %1, ax
	push eax
	%elifidni %1, bx
	push ebx
	%elifidni %1, cx
	push ecx
	%elifidni %1, dx
	push edx
	%elifidni %1, si
	push esi
	%elifidni %1, di
	push edi
	%elifidni %1, bp
	%error "log: Cannot log value of BP!"
	%elifidni %1, sp
	%error "log: Cannot log value of SP!"

	; 32-bit register

	%elifidni %1, eax
	push eax
	%elifidni %1, ebx
	push ebx
	%elifidni %1, ecx
	push ecx
	%elifidni %1, edx
	push edx
	%elifidni %1, esi
	push esi
	%elifidni %1, edi
	push edi
	%elifidni %1, ebp
	%error "log: Cannot log value of EBP!"
	%elifidni %1, esp
	%error "log: Cannot log value of ESP!"

	; Segment register

	%elifidni %1, cs
	%error "log: Cannot log value of CS!"
	%elifidni %1, ds
	o32 push ds
	%elifidni %1, es
	o32 push es
	%elifidni %1, fs
	o32 push fs
	%elifidni %1, gs
	o32 push gs
	%elifidni %1, ss
	o32 push ss

	; Constant or memory pointer

	%else
	push dword %1

	%endif

	%rotate 1
	%endrep

	mov si, %%message
	extern log_format
	call log_format

	mov sp, bp
	pop bp
	pop si
	popf
	retf

	__SECT__

	call far %%print

	%endif
%endmacro


;------------------------------------------------------------------------------
; Log flags to the standard output.
;------------------------------------------------------------------------------

%macro	logf 0
	%ifenv DEBUG_BUILD

	extern log_flags
	call far log_flags

	%endif
%endmacro


;------------------------------------------------------------------------------
; Log and verify memory blocks to console.
;------------------------------------------------------------------------------
; Requires system/memory.
;------------------------------------------------------------------------------

%macro	logmcbs 0
	%ifenv DEBUG_BUILD

	extern log_mcbs
	call far log_mcbs

	%endif
%endmacro
