;==============================================================================
; Debug logging tools during development
;------------------------------------------------------------------------------
; -> DEBUG_BUILD environment variable: logging is only generated if it exists
;    If the environment variable is not defined, no debug-related code will be
;    added to the application.
;------------------------------------------------------------------------------
; All macros can be called at any point in the code, they don't alter flags or
; registers. Macros emit a far call at the point of inclusion (taking 5 bytes)
; into the current segment. The logging code itself will be output to the
; "debug" segment and log messages (via the log macro) are put into the
; "debug_data" segment.
;==============================================================================

cpu 386

%include "debug/global.inc"

%ifdef __DEBUG__

extern log_setup
extern log_shutdown

%endif


;------------------------------------------------------------------------------
; Log formatted string.
;------------------------------------------------------------------------------
; -> %1 - Source string literal for sys_str_format
;    %2... - Placeholder values. Memory addresses cannot use the CS: prefix!
;------------------------------------------------------------------------------

%macro	log 1-*
	%ifdef __DEBUG__

	[section debug_data]

%%message	db %1, 0

	[section debug]

%%eax		dd 0
%%ebp		dd 0
%%esp		dd 0

%%print:
	mov cs:[%%ebp], ebp
	mov cs:[%%esp], esp
	pushf
	push si
	push bp
	mov bp, sp

	%rotate 1			; Get rid of format source string
	%rep (%0 - 1)			; Push placeholder values to stack

	; 8-bit register

	%ifidni %1, al
	mov cs:[%%eax], eax
	movzx eax, al
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, ah
	mov cs:[%%eax], eax
	movzx eax, ah
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, bl
	mov cs:[%%eax], eax
	movzx eax, bl
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, bh
	mov cs:[%%eax], eax
	movzx eax, bh
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, cl
	mov cs:[%%eax], eax
	movzx eax, cl
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, ch
	mov cs:[%%eax], eax
	movzx eax, ch
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, dl
	mov cs:[%%eax], eax
	movzx eax, dl
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, dh
	mov cs:[%%eax], eax
	movzx eax, dh
	push eax
	mov eax, cs:[%%eax]

	; 16-bit register

	%elifidni %1, ax
	mov cs:[%%eax], eax
	movzx eax, ax
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, bx
	mov cs:[%%eax], eax
	movzx eax, bx
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, cx
	mov cs:[%%eax], eax
	movzx eax, cx
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, dx
	mov cs:[%%eax], eax
	movzx eax, dx
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, si
	mov cs:[%%eax], eax
	movzx eax, si
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, di
	mov cs:[%%eax], eax
	movzx eax, di
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, bp
	mov cs:[%%eax], eax
	movzx eax, word cs:[%%ebp]
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, sp
	mov cs:[%%eax], eax
	movzx eax, word cs:[%%esp]
	add eax, 4			; Skip far return address
	push eax
	mov eax, cs:[%%eax]

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
	mov cs:[%%eax], eax
	mov eax, cs:[%%ebp]
	push eax
	mov eax, cs:[%%eax]
	%elifidni %1, esp
	mov cs:[%%eax], eax
	mov eax, cs:[%%esp]
	add eax, 4			; Skip far return address
	push eax
	mov eax, cs:[%%eax]

	; Segment register

	%elifidni %1, cs
	mov cs:[%%eax], eax
	mov eax, cs:[%%esp]
	movzx eax, word ss:[eax + 2]
	push eax
	mov eax, cs:[%%eax]
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
; Log flags.
;------------------------------------------------------------------------------

%macro	logf 0
	%ifdef __DEBUG__

	pushf
	extern log_flags
	call far log_flags
	popf

	%endif
%endmacro


;------------------------------------------------------------------------------
; Log and verify memory blocks to console.
;------------------------------------------------------------------------------
; Requires system/memory.
;------------------------------------------------------------------------------

%macro	logmcbs 0
	%ifdef __DEBUG__

	extern log_mcbs
	call far log_mcbs

	%endif
%endmacro


;------------------------------------------------------------------------------
; VGA frametime visualizer.
;------------------------------------------------------------------------------
; Sets the color of a palette entry to a specific RGB code.
; -> %1 - VGA palette entry
;    %2 - Red component (0 - 63)
;    %3 - Green component (0 - 63)
;    %4 - Blue component (0 - 63)
;------------------------------------------------------------------------------

%macro	logframe 4
	%ifdef __DEBUG__

	push ax
	push dx

	mov al, %1
	mov dx, 0x3c8
	out dx, al
	inc dx
	mov al, %2
	out dx, al
	mov al, %3
	out dx, al
	mov al, %4
	out dx, al

	pop dx
	pop ax

	%endif
%endmacro