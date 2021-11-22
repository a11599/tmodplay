;==============================================================================
; Debug logging tools during development
;------------------------------------------------------------------------------
; -> DEBUG_BUILD environment variable: logging is only generated if it exists
;    If the environment variable is not defined, no debug-related code will be
;    added to the code.
;------------------------------------------------------------------------------
; These functions are not meant to be called directly. Use the log* macros in
; debug/log.inc instead.
;==============================================================================

cpu 386

%include "debug/log.inc"

%ifdef __DEBUG__

MAX_LOG_LEN	EQU 512			; Absolute max. len of any log message

%include "system/api/string.inc"
%include "system/api/file.inc"

segment debug


;------------------------------------------------------------------------------
; Log flags to the standard output.
;------------------------------------------------------------------------------

global log_flags
log_flags:
	pushf
	push ax
	push bx
	push esi
	push edi
	push bp
	push ds

	pushf
	pop ax

	mov bx, debug_messages
	mov ds, bx
	mov bp, sp
	mov bh, 16
	mov si, 0x8000

.check_flags_loop:
	test ax, si			; Test flag bit
	setnz bl
	push ebx			; Save flag value (0 or 1)
	shr si, 1			; Next bit
	dec bh
	jnz .check_flags_loop

	mov esi, flags			; Format flags string
	mov edi, buf
	call far sys_str_format
	mov sp, bp
	mov esi, buf
	call far sys_str_len		; ECX: length of string
	mov ah, 0x40
	mov bx, 1			; Write to file handle 1 (stdout)
	mov dx, si			; DS:DX: pointer to string
	int 0x21

	pop ds
	pop bp
	pop edi
	pop esi
	pop bx
	pop ax
	popf
	retf


;------------------------------------------------------------------------------
; Log formatted text to standard output.
;------------------------------------------------------------------------------
; -> SI - Offset of ASCIIZ string message in debug_messages segment
;------------------------------------------------------------------------------

global log_format
log_format:
	push eax
	push bx
	push ecx
	push dx
	push esi
	push edi
	push ds

	xor eax, eax
	mov ax, ds
	shl eax, 4			; EAX: DS segment linear address
	xor ecx, ecx
	mov cx, debug_messages
	mov bx, cx
	shl ecx, 4			; ECX: debug_messages linear address
	movzx esi, si
	add esi, ecx
	sub esi, eax			; DS:ESI: source string to format
	mov edi, buf
	add edi, ecx
	sub edi, eax			; DS:EDI: output buffer
	call far sys_str_format
	mov ax, cs
	mov ds, bx
	mov esi, buf
	call far sys_str_len		; ECX: length of string
	mov ah, 0x40
	mov bx, 1			; Write to file handle 1 (stdout)
	mov dx, si			; DS:DX: pointer to string
	int 0x21

	pop ds
	pop edi
	pop esi
	pop dx
	pop ecx
	pop bx
	pop eax
	retn


;==============================================================================
; Data area
;==============================================================================

segment debug_messages

flags		db '+---+---+------+---+---+---+---+---+---+---+---+---+---+---+---+', 13, 10
		db '| - | N | IOPL | O | D | I | T | S | Z | - | A | - | P | - | C |', 13, 10
		db '| {u8} | {u8} |  {u8}{u8}  | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} |', 13, 10
		db '+---+---+------+---+---+---+---+---+---+---+---+---+---+---+---+', 13, 10, 0

buf		db MAX_LOG_LEN dup (0)

;------------------------------------------------------------------------------
; Debugging messages added here by the log macro
;------------------------------------------------------------------------------

%endif