;==============================================================================
; Debug logging tools during development
;------------------------------------------------------------------------------
; -> DEBUG_BUILD environment variable: logging is only generated if it exists
;    If the environment variable is not defined, no debug-related code will be
;    added to the application.
;------------------------------------------------------------------------------
; These functions are not meant to be called directly. Use the log* macros in
; debug/log.inc instead.
;==============================================================================

cpu 386

%include "debug/global.inc"

%ifdef __DEBUG__

MAX_LOG_LEN	EQU 512			; Absolute max. len of any log message

%include "system/api/string.inc"
%include "system/api/file.inc"

segment debug


;------------------------------------------------------------------------------
; Setup logging. No log messages will be written until this function is called.
;------------------------------------------------------------------------------
; -> EAX - Logging flags
;    DS:ESI - Pointer to ASCIIZ filename for LOG_FILE.
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global log_setup
log_setup:
	push eax

	mov cs:[flags], eax
	test al, LOG_FILE
	jz .console			; Log to console
	test al, LOG_APPEND		; Set file open mode
	setz al				; AL: 1 when overwrite, 0 when append
	add al, al			; AL: 2 when overwrite, 0 when append
	add al, 1			; AL: 3 when overwrite, 1 when append
	mov al, 3
	call far sys_file_open		; Open the file
	jc .error
	mov cs:[file_handle], eax

.done:
	clc
	pop eax

.exit:
	retf

.error:
	mov dword cs:[file_handle], 1
	add sp, 4
	stc
	jmp .exit

.console:
	test al, LOG_STDERR
	jz .stderr
	mov dword cs:[file_handle], 1
	jmp .done

.stderr:
	mov dword cs:[file_handle], 2
	jmp .done


;------------------------------------------------------------------------------
; Shutdown logging. Should be called before exiting to DOS. No messages will be
; logged after this call.
;------------------------------------------------------------------------------

	align 4

global log_shutdown
log_shutdown:
	push eax
	push ebx

	cmp dword cs:[file_handle], 4	; Not an actual file, no need to close
	jbe .exit

	mov ebx, cs:[file_handle]
	call far sys_file_close		; Close logfile

.exit:
	mov dword cs:[file_handle], 1	; Log to stdout from now on
	pop ebx
	pop eax
	retf


;------------------------------------------------------------------------------
; Log flags to the standard output.
;------------------------------------------------------------------------------

	align 4

global log_flags
log_flags:
	push ax
	push bx
	push esi
	push edi
	push bp
	push ds

	pushf
	pop ax

	cmp dword cs:[file_handle], 0	; Debug not properly initialized, exit
	je .done

	mov bx, debug_data
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

	mov esi, flags_fmt		; Format flags string
	mov edi, buf
	call far sys_str_format
	mov sp, bp
	mov esi, buf
	call far sys_str_len		; ECX: length of string
	mov ah, 0x40
	mov bx, cs:[file_handle]
	mov dx, si			; DS:DX: pointer to string
	int 0x21
	test dword cs:[flags], LOG_AUTOCOMMIT
	jz .done
	mov ah, 0x68			; Commit write to log file
	int 0x21

.done:

	pop ds
	pop bp
	pop edi
	pop esi
	pop bx
	pop ax
	retf


;------------------------------------------------------------------------------
; Log formatted text to standard output.
;------------------------------------------------------------------------------
; -> SI - Offset of ASCIIZ string message in debug_data segment
;------------------------------------------------------------------------------

	align 4

global log_format
log_format:
	cmp dword cs:[file_handle], 0	; Debug not properly initialized, exit
	je .noop

	push eax
	push bx
	push ecx
	push dx
	push esi
	push edi
	push ds

	mov ax, debug_data
	mov ds, ax
	movzx esi, si
	mov edi, buf
	call far sys_str_format
	mov esi, buf
	call far sys_str_len		; ECX: length of string
	mov ah, 0x40
	mov bx, cs:[file_handle]
	mov dx, si			; DS:DX: pointer to string
	int 0x21
	test dword cs:[flags], LOG_AUTOCOMMIT
	jz .done
	mov ah, 0x68			; Commit write to log file
	int 0x21

.done:
	pop ds
	pop edi
	pop esi
	pop dx
	pop ecx
	pop bx
	pop eax

.noop:
	retn


;==============================================================================
; Data area
;==============================================================================

		alignb 4
flags		dd 0
file_handle	dd 1			; Log to console until initialized

segment debug_data

flags_fmt	db '+---+---+------+---+---+---+---+---+---+---+---+---+---+---+---+', 13, 10
		db '| - | N | IOPL | O | D | I | T | S | Z | - | A | - | P | - | C |', 13, 10
		db '| {u8} | {u8} |  {u8}{u8}  | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} | {u8} |', 13, 10
		db '+---+---+------+---+---+---+---+---+---+---+---+---+---+---+---+', 13, 10, 0

buf		db MAX_LOG_LEN dup (0)

;------------------------------------------------------------------------------
; Debugging messages added here by the log macro
;------------------------------------------------------------------------------

%endif