;==============================================================================
; System library - DOS environment management
;==============================================================================

cpu 386

%include "debug/log.inc"

segment system public use16 class=CODE align=16
segment system


;------------------------------------------------------------------------------
; Initializes the environment manager.
;------------------------------------------------------------------------------
; -> ES - PSP segment
;------------------------------------------------------------------------------

global sys_env_setup
sys_env_setup:
	push eax
	push cx
	push si
	push di
	push ds
	push es

	cld

	; Save pointers to DOS data areas

	xor eax, eax
	mov ax, es
	mov ds, ax			; DS: PSP segment
	shl eax, 4
	mov cs:[psp_addr], eax		; Save PSP linear address
	xor eax, eax
	mov ax, es:[0x2c]
	shl eax, 4
	mov cs:[env_addr], eax		; Save environment linear address

	; Extract command line arguments

	mov si, 0x81			; DS:SI: command line arguments in PSP
	mov di, args			; ES:DI: argument list data area
	movzx cx, byte es:[0x80]	; CX: length of command line arguments
	cmp cx, 127			; Limit length for safety
	jbe .convert_args
	mov cx, 127

.convert_args:
	xor eax, eax
	mov ax, cs
	mov es, ax			; ES: CS
	shl eax, 4
	add eax, args
	mov cs:[args_addr], eax
	jcxz .next_arg

.skip_spaces_loop:
	lodsb				; Get next character
	cmp al, ' '
	setbe ah
	test ah, ah
	loopnz .skip_spaces_loop, cx	; Skip while space or control character
	jcxz .next_arg			; End of arguments

.save_arg_chars_loop:
	stosb				; Not space or control character, store
	lodsb				; Get next character
	cmp al, ' '
	setbe ah
	test ah, ah
	loopz .save_arg_chars_loop, cx	; Store while not space or control char
	jnz .next_arg			; Space or control char: next argument
	stosb				; Store last non-space or control char

.next_arg:
	xor al, al
	stosb				; Store 0 as argument separator
	jcxz .done_args			; End of arguments
	jmp .skip_spaces_loop		; Find start of next argument

.done_args:
	xor al, al
	stosb				; Store final 0 as end of argument list

	pop es
	pop ds
	pop di
	pop si
	pop cx
	pop eax
	retf


;------------------------------------------------------------------------------
; Returns the value of an environment variable.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to ASCIIZ string containing environment variable name in
;             uppercase.
; <- CF - Set if variable not found
;    DS:ESI - Pointer to ASCIIZ string containing environment variable value if
;             the variable exists.
;------------------------------------------------------------------------------

global sys_env_get_var
sys_env_get_var:
	push eax
	push ebx
	push edi
	push esi

	cld

	mov ebx, esi			; DS:EBX: wanted variable name
	xor eax, eax
	mov ax, ds
	shl eax, 4
	mov esi, cs:[env_addr]
	sub esi, eax			; DS:ESI: DOS environment area

.check_env_var_loop:
	xor edi, edi			; EDI: index into wanted variable name

.check_env_name_loop:
	mov ah, [ebx + edi]		; AH: next char of wanted variable name
	inc edi
	a32 lodsb			; AL: next character of variable name
	test al, al			; End of environment area: not found
	jz .not_found
	cmp al, '='			; End of environment variable name
	je .check_name
	cmp al, 'a'			; Convert variable name to uppercase
	jb .compare_name
	cmp al, 'z'
	ja .compare_name
	sub al, 'a' - 'A'

.compare_name:
	cmp al, ah			; Same character, continue with next
	je .check_env_name_loop

.skip_env_var:
	inc esi				; Skip to next 0 byte
	cmp byte [esi - 1], 0
	jne .skip_env_var
	jmp .check_env_var_loop

.check_name:
	test ah, ah			; Check end of wanted variable name
	jnz .skip_env_var		; Nope, no match

	add sp, 4			; Discard ESI from stack
	clc

.exit:
	pop edi
	pop ebx
	pop eax
	retf

.not_found:
	stc
	pop esi
	jmp .exit


;------------------------------------------------------------------------------
; Returns the number of arguments.
;------------------------------------------------------------------------------
; <- CL - Number of arguments
;------------------------------------------------------------------------------

global sys_env_get_arg_count
sys_env_get_arg_count:
	push eax
	push esi

	cld

	xor eax, eax
	mov ax, ds
	shl eax, 4
	mov esi, cs:[args_addr]
	sub esi, eax			; DS:ESI: program arguments
	xor cl, cl			; CL: number of arguments

.count_arg_loop:
	a32 lodsb
	test al, al			; Check end of argument area
	jz .exit

.skip_arg_loop:
	a32 lodsb			; Skip argument
	test al, al
	jnz .skip_arg_loop
	inc cl				; Next argument
	jmp .count_arg_loop

.exit:
	pop esi
	pop eax
	retf


;------------------------------------------------------------------------------
; Returns the value of an argument.
;------------------------------------------------------------------------------
; -> CL - Index of the argument to retrieve (0-based)
; <- CF - Set if argument not found
;    DS:ESI - Pointer to ASCIIZ string containing argument if it exists.
;------------------------------------------------------------------------------

global sys_env_get_arg
sys_env_get_arg:
	push ax
	push ebx
	push esi

	cld

	xor ebx, ebx
	mov bx, ds
	shl ebx, 4
	mov esi, cs:[args_addr]
	sub esi, ebx			; DS:ESI: program arguments
	xor bl, bl			; BL: index of current argument

.find_arg_loop:
	cmp bl, cl			; Check argument index match
	je .check_arg			; DS:ESI points to start of argument
	a32 lodsb			; Check end of argument area
	test al, al
	jz .not_found

.skip_arg_loop:
	a32 lodsb			; Skip argument
	test al, al
	jnz .skip_arg_loop
	inc bl				; Next argument
	jmp .find_arg_loop

.check_arg:
	cmp byte [esi], 0		; Check end of argument area
	je .not_found

	add sp, 4			; Discard ESI from stack
	clc

.exit:
	pop ebx
	pop ax
	retf

.not_found:
	stc
	pop esi
	jmp .exit


;------------------------------------------------------------------------------
; Returns the value of a named argument.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to ASCIIZ string containing argument name.
; <- CF - Set if argument not found
;    DS:ESI - Pointer to ASCIIZ string containing argument value if it exists.
;------------------------------------------------------------------------------

global sys_env_get_named_arg
sys_env_get_named_arg:
	push eax
	push ebx
	push edi
	push esi

	cld

	mov ebx, esi			; DS:EBX: wanted argument name
	xor eax, eax
	mov ax, ds
	shl eax, 4
	mov esi, cs:[args_addr]
	sub esi, eax			; DS:ESI: program arguments

.check_args_loop:
	xor edi, edi			; EDI: index into wanted argument name
	cmp byte [esi], 0		; End of argument area, not found
	je .not_found

.check_arg_loop:
	mov ah, [ebx + edi]		; AH: next char of wanted argument name
	inc edi
	a32 lodsb			; AL: next character of argument name
	test al, al			; End of argument
	jz .check_arg
	cmp al, ':'			; End of argument name
	je .check_arg
	cmp al, ah			; Same character, continue with next
	je .check_arg_loop

.skip_arg:
	test al, al
	jz .check_args_loop		; Check first, last char may already NUL
	a32 lodsb			; AL: next character of argument
	jmp .skip_arg

.check_arg:
	test ah, ah			; Check end of wanted variable name
	jnz .skip_arg			; Nope, no match

	add sp, 4			; Discard ESI from stack
	clc

.exit:
	pop edi
	pop ebx
	pop eax
	retf

.not_found:
	stc
	pop esi
	jmp .exit


;------------------------------------------------------------------------------
; Returns the full path to the running executable.
;------------------------------------------------------------------------------
; <- DS:ESI - Pointer to ASCIIZ string containing the full path of the running
;             executable.
;------------------------------------------------------------------------------

global sys_env_get_exe_path
sys_env_get_exe_path:
	push eax

	cld

	xor eax, eax
	mov ax, ds
	shl eax, 4
	mov esi, cs:[env_addr]
	sub esi, eax			; DS:ESI: DOS environment area
	mov al, 0xff

.find_exe_path_loop:
	mov ah, al
	a32 lodsb			; AX: previous + current character
	test ax, ax			; Look for two consecutive NULs
	jnz .find_exe_path_loop
	a32 lodsw			; Skip number of strings

	pop eax
	retf


;==============================================================================
; Data area
;==============================================================================

args		db 130 dup (0)		; Arguments, separated with zero bytes

		alignb 4
psp_addr	dd 0			; Linear address of PSP
env_addr	dd 0			; Linear address of environment
args_addr	dd 0			; Linear address of argument list
