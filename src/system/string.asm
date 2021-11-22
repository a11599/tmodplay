;==============================================================================
; System library - string operations
;==============================================================================

cpu 386

segment system public use16 class=CODE align=16
segment system

%include "debug/log.inc"


;------------------------------------------------------------------------------
; Return the length of an ASCIIZ string.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to ASCIIZ string
; <- ECX - Length of the string
;------------------------------------------------------------------------------

global sys_str_len
sys_str_len:
	push ax
	push edi
	push es

	cld

	mov ax, ds
	mov es, ax
	mov edi, esi			; ES:EDI: pointer to ASCIIZ string
	xor al, al			; AL: NUL terminator
	mov ecx, -1

	a32 repne scasb			; Scan for NUL terminator

	mov ecx, edi			; Calculate length of string
	sub ecx, esi
	dec ecx

	pop es
	pop edi
	pop ax
	retf


;------------------------------------------------------------------------------
; Reverse a string in place.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to ASCIIZ string
;------------------------------------------------------------------------------

global sys_str_reverse
sys_str_reverse:
	push ax
	push ecx
	push esi
	push edi

	push cs				; Simulate far call
	call sys_str_len		; Get length of string
	mov edi, esi
	add edi, ecx			; DI: end of string + 1
	shr ecx, 1			; Reverse until half of string
	jz .exit

.next_char:
	dec edi				; Move backward end of string pointer
	mov al, [esi]			; Exchange characters
	xchg al, [edi]
	mov [esi], al
	inc esi				; Move forward start of string pointer
	dec ecx
	jnz .next_char

.exit:
	pop edi
	pop esi
	pop ecx
	pop ax
	retf


;------------------------------------------------------------------------------
; Append a string at the end of another one.
;------------------------------------------------------------------------------
; -> ECX - Maximum length of the appended string
;    DS:ESI - Source ASCIIZ string
;    DS:EDI - Target ASCIIZ string to which the source will be appended in place
;------------------------------------------------------------------------------

global sys_str_append
sys_str_append:
	push ax
	push ecx
	push edx
	push esi
	push edi
	push es

	cld

	mov ax, ds
	mov es, ax
	xor edx, edx

.check_end:
	mov al, es:[edi]
	test al, al			; End of target: do the append
	jz .append
	inc edx
	jz .exit			; Overflow, exit
	inc edi				; Nope, check next char
	jmp .check_end

.append:
	cmp edx, ecx
	jae .exit
	sub ecx, edx			; ECX: characters left remaining

.next_char:
	a32 lodsb			; Get next char from source
	a32 stosb			; Add to target
	test al, al			; Not end of source: add next char
	loopnz .next_char, ecx

.exit:
	pop es
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ax
	retf


;------------------------------------------------------------------------------
; Find the first occurence of a character in a string.
;------------------------------------------------------------------------------
; -> AH - Character to search for
;    ECX - Maximum number of characters to search
;    DS:ESI - ASCIIZ string to find character within
; <- CF - Set if character was not found
;    EAX - Index of character's first occurence if CF is not set
;------------------------------------------------------------------------------

global sys_str_char_pos
sys_str_char_pos:
	push ebx
	push ecx
	push esi
	push eax

	jecxz .not_found		; ECX is zero, nothing to search

	cld

	mov ebx, esi			; EBX: start of string

.find_loop:
	a32 lodsb			; Get next character
	test al, al
	jz .not_found			; End of string: not found
	cmp al, ah			; Found character?
	loopne .find_loop, ecx		; Nope, try next

	sub esi, ebx
	dec esi
	add sp, 4			; Discard EAX from stack
	mov eax, esi
	clc

.exit:
	pop esi
	pop ecx
	pop ebx
	retf

.not_found:
	pop eax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Compares two ASCIIZ strings.
;------------------------------------------------------------------------------
; -> ECX - Maximum number of characters to compare
;    DS:ESI - Pointer to source ASCIIZ string
;    DS:EDI - Pointer to target ASCIIZ string
; <- CF - Set if the strings are different, false otherwise
;------------------------------------------------------------------------------

global sys_str_cmp
sys_str_cmp:
	push ax
	push ecx
	push esi
	push edi

	cld

.compare_loop:
	a32 lodsb			; AL: next character of source
	mov ah, [edi]			; AH: next character of target
	cmp al, ah
	jne .different			; Different characters or length
	inc edi
	test al, al			; End of string?
	loopnz .compare_loop, ecx	; Nope, compare next character

	clc

.exit:
	pop edi
	pop esi
	pop ecx
	pop ax
	retf

.different:
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Convert a 32-bit number into decimal string.
;------------------------------------------------------------------------------
; -> EAX - Number to convert
;    BL - Mode, 0 = unsigned, 1 = signed
;    DS:ESI - Buffer receiving ASCIIZ result string (min. 12 bytes)
;------------------------------------------------------------------------------

global sys_str_int
sys_str_int:
	push eax
	push ecx
	push edx
	push esi

	test bl, bl
	jz .calculate_length

	test eax, 0x80000000		; Unsigned: start conversion
	jz .calculate_length
	mov byte [esi], '-'		; Add minus char to buffer
	inc esi
	neg eax				; Negate value

.calculate_length:

	; Calculate length of string after conversion

	xor ecx, ecx

.loop_calculate_length:
	mov edx, cs:[intlentab + ecx * 4]
	test edx, edx
	jz .convert
	cmp eax, edx
	jbe .convert
	inc ecx
	jmp .loop_calculate_length

.convert:

	; Convert digits

	add esi, ecx
	mov byte [esi + 1], 0		; End of string
	mov ecx, 10
.next_digit:
	xor edx, edx			; Prepare for division
	div ecx				; Unsigned divide by 10
	add dl, '0'			; Remainder to string
	mov [esi], dl			; Store in output buffer
	dec esi
	test eax, eax			; Division result <> 0: next digit
	jnz .next_digit

	pop esi
	pop edx
	pop ecx
	pop eax
	retf


;------------------------------------------------------------------------------
; Convert a 32-bit fixed-point number into a decimal string.
;------------------------------------------------------------------------------
; -> EAX - Number to convert
;    BL - Mode, 0 = unsigned, 1 = signed
;    BH - Number of decimals in the converted string (rounding applies)
;    ECX - Fixed point base (value representing 1)
;    DS:ESI - Buffer receiving ASCIIZ result string
;------------------------------------------------------------------------------

global sys_str_fixed
sys_str_fixed:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	xor edx, edx
	div ecx				; EAX: integer part
	mov ebp, edx			; EBP: fraction part

	;----------------------------------------------------------------------
	; Convert integer part

	test bl, bl
	jz .calculate_length

	test eax, 0x80000000		; Unsigned: start conversion
	jz .calculate_length
	mov byte [esi], '-'		; Add minus char to buffer
	inc esi
	neg eax				; Negate value

.calculate_length:

	; Calculate length of integer part of string after conversion

	xor edi, edi

.loop_calculate_length:
	mov edx, cs:[intlentab + edi * 4]
	test edx, edx
	jz .convert_int
	cmp eax, edx
	jbe .convert_int
	inc edi
	jmp .loop_calculate_length

.convert_int:

	; Convert integer digits

	add esi, edi			; DS:ESI: last char of integer part
	push esi

	mov edi, 10

.next_int_digit:
	xor edx, edx			; Prepare for division
	div edi				; Unsigned divide by 10
	add dl, '0'			; Remainder to string
	mov [esi], dl			; Store in output buffer
	dec esi
	test eax, eax			; Division result <> 0: next digit
	jnz .next_int_digit
	pop esi				; DS:ESI: last integer character

	;----------------------------------------------------------------------
	; Convert fraction part

	test ebp, ebp			; No fraction part, done
	jz .done

	; Calculate length of fraction part string after conversion

	cmp bh, 9			; BH: maximum number of decimal chars
	jbe .get_frac_len		; Limit to 9
	mov bh, 9

.get_frac_len:
	movzx edi, bh
	mov edx, cs:[intlentab + edi * 4 - 4]
	inc edx				; EDX: 10 ^ number of decimal digits
	mov eax, ebp			; EAX: fraction part
	mul edx				; EDX:EAX: fraction * 10 ^ nb dec digits

	div ecx				; EAX: floor(10-base fraction)
	shr ecx, 1
	cmp edx, ecx			; Rounding
	setae dl
	movzx edx, dl
	add eax, edx			; EAX: round(10-base fraction)
	test eax, eax			; No 10-base fraction, done
	jz .done
	mov byte [esi + 1], '.'
	inc esi				; Skip decimal point
	add esi, edi			; DS:ESI: last char of fraction part
	mov edi, esi			; DS:EDI: last char of fraction part

	; Convert fraction digits

	mov ecx, 10
	xor ebp, ebp			; EBP: strip zero flag

.next_frac_digit:
	xor edx, edx			; Prepare for division
	div ecx				; Unsigned divide by 10
	test ebp, ebp
	jnz .save_frac_digit
	test dl, dl
	jz .skip_frac_digit
	inc ebp
	mov edi, esi			; DS:EDI: actual last char of string

.save_frac_digit:
	add dl, '0'			; Remainder to string
	mov [esi], dl			; Store in output buffer

.skip_frac_digit:
	dec esi
	dec bh				; Number of fraction characters left
	test eax, eax			; Division result <> 0: next digit
	jnz .next_frac_digit
	test bh, bh
	jz .frac_done

	; Fill fraction digits with 0 from left since that is missing from
	; the 10-base converted fraction value

.fill_frac_zero:
	mov byte [esi], '0'
	dec esi
	dec bh
	jnz .fill_frac_zero

.frac_done:
	mov esi, edi			; DS:ESI: last char of string

.done:
	mov byte [esi + 1], 0		; End of string

	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	retf


;------------------------------------------------------------------------------
; Convert a 32-bit number into a hexadecimal string.
;------------------------------------------------------------------------------
; -> EAX - Number to convert
;    BL - Casing: 0 = lowercase, 1 = uppercase
;    BH - Number of nibbles to convert (1 - 4 digits from lowest nibble)
;    DS:ESI - Buffer receiving ASCIIZ result string (min. BH + 1 bytes)
;------------------------------------------------------------------------------

global sys_str_hex
sys_str_hex:
	test bh, bh
	jnz .start
	mov byte [esi], 0		; 0 nibbles: empty string
	retf

.start:
	push eax
	push bx
	push ecx
	push esi

	cmp bh, 8			; Limit number of nibbles to 8
	jbe .setup
	mov bh, 8

.setup:
	movzx ecx, bh			; CX: number of loops
	add esi, ecx			; Adjust SI to end of buffer
	mov byte [esi], 0		; End of string

	mov bh, '9' - 'a' + 1		; Lowercase a - f
	test bl, bl
	jz .next_nibble
	mov bh, '9' - 'A' + 1		; Uppercase A - F

.next_nibble:
	dec esi				; Move buffer pointer forward
	mov bl, al
	and bl, 0x0f			; BL: lowest nibble
	cmp bl, 9
	jbe .digit
	sub bl, bh			; Adjust for correct A-F ASCII code

.digit:
	add bl, '0'			; Convert to ASCII
	mov [esi], bl
	shr eax, 4
	loop .next_nibble, cx

	pop esi
	pop ecx
	pop bx
	pop eax
	retf


;------------------------------------------------------------------------------
; Compose a string from a source by replacing placeholder variables with actual
; data into a target string. This isn't very fast, but convenient when speed is
; not of great concern. Use individual data converters when speed does matter.
;------------------------------------------------------------------------------
; -> DS:ESI - Source string with optional placeholder variables
;    DS:EDI - Buffer receiving ASCIIZ result string (must be large enough)
;    SS:BP - Pointer just above first variable value
;------------------------------------------------------------------------------
; Source string may contain placeholder "variables" enclosed in curly brackets.
; The following placeholders are supported:
; - {u[8|16|32]}: Unsigned integer of 8/16/32 bits (default: 32 bits)
; - {i[8|16|32]}: Signed integer of 8/16/32 bits (default: 32 bits)
; - {x[8|16|32]}: Lowercase hexadecimal value of 8/16/32 bits (default: 32 bits)
; - {X[8|16|32]}: Uppercase hexadecimal value of 8/16/32 bits (default: 32 bits)
; - {q[8|16|32]:base[.precision]}: Unsigned fixed-point value of 8/16/32 bits
;   (default: 32 bits) having "base" (value corresponding to 1). Fraction
;   decimals are displayed up to "precision" digits (default: max. precision).
; - {w[8|16|32]:base[.precision]}: Same as above, but signed.
; - {c}: A single character.
; - {s[:length]}: A string up to "length" characters (default: entire string).
;   Placeholder value on stack is pointer to string relative to DS.
;------------------------------------------------------------------------------
; For each of the placeholders, a value must be provided on the stack in the
; order of placeholders from top to bottom. The typical usage is:
;------------------------------------------------------------------------------
;	mov bp, sp			; Set BP to current stack top
;	push dword {value1}		; Push first placeholder value
;	push dword {value2}		; Push second placeholder value
;	mov esi, fmt_string		; DS:ESI: string to format
;	mov edi, buffer			; DS:EDI: taget buffer for formatted str
;	call far sys_str_format		; Compose formatted string
;	mov sp, bp			; Discard placeholder values
;------------------------------------------------------------------------------
; Each placeholder value should take 32 bits on the stack. However for smaller
; values, such as {c} or {i16} for example, only the low byte/word of the value
; is used and the rest is discarded. So if you want to format AL as a signed
; integer, you can safely push eax for {i8}; the upper 24 bits won't be used
; to print the integer value. For the high byte of registers, you need to
; either move it to another register, or if there is no spare register, you can
; do an xchg ah, al / push eax / xchg ah, al at the expense of some extra
; processing.
;------------------------------------------------------------------------------

global sys_str_format
sys_str_format:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push bp

	cld

.printf_loop:
	a32 lodsb
	test al, al
	jz .done
	cmp al, '{'
	je .variable

.store:
	mov [edi], al
	inc edi
	jmp .printf_loop

.done:
	mov byte [edi], 0		; Terminate string

	pop bp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	retf

.variable:

	; Variable

	a32 lodsb
	cmp al, '{'			; {{: print opening curly bracket
	je .store
	sub bp, 4			; Go to start of next variable on stack

	; Check variable type

	xor cl, cl
	cmp al, 'u'			; Unsigned integer
	je .int
	inc cl
	cmp al, 'i'			; Signed integer
	je .int
	xor cl, cl
	cmp al, 'x'			; Lowercase hexadecimal
	je .hex
	inc cl
	cmp al, 'X'			; Uppercase hexadecimal
	je .hex
	xor dl, dl
	cmp al, 'q'			; Unsigned fixed-point
	je .fixed
	inc dl
	cmp al, 'w'			; Signed fixed-point
	je .fixed
	cmp al, 'c'			; Character
	je .char
	cmp al, 's'			; String
	je .string
	jmp .skip_variable

.int:

	;----------------------------------------------------------------------
	; {u|i[8|16|32]}: unsigned or signed integer of bit length

	mov bx, 0xff * 256 + '}'	; Get integer bit length
	push cs				; Simulate far call
	call sys_str_parse_int
	jc .skip_variable
	test eax, eax			; {u|i}: 32 bit
	je .int_dword
	cmp eax, 32			; {u32|i32}: 32 bit
	je .int_dword
	test cl, cl
	jz .uint
	cmp eax, 16			; {u16}
	je .int_word
	cmp eax, 8			; {u8}
	je .int_byte
	jmp .skip_variable		; Unknown, skip variable

.uint:
	cmp eax, 16			; {i16}
	je .uint_word
	cmp eax, 8			; {i8}
	je .uint_byte
	jmp .skip_variable

.int_dword:
	mov eax, [bp]			; {u|i[32]}: get as 32 bit from stack
	jmp .str_int

.int_word:
	movsx eax, word [bp]		; {i16}: sign-extend into eax
	jmp .str_int

.int_byte:
	movsx eax, byte [bp]		; {i8}: sign-extend into eax
	jmp .str_int

.uint_word:
	movzx eax, word [bp]		; {u16}: zero-extend into eax
	jmp .str_int

.uint_byte:
	movzx eax, byte [bp]		; {u8}: zero-extend into eax

.str_int:
	mov edx, esi			; Save string pointer
	mov bl, cl			; Convert number to string
	mov esi, edi
	push cs				; Simulate far call
	call sys_str_int
	mov esi, edx			; Restore string pointer
	call .skip_dest_to_end		; Skip to end of result string
	jmp .skip_variable

.fixed:

	;----------------------------------------------------------------------
	; {q|w[8|16|32]:base[.precision]}: unsigned or signed fixed-point
	; decimal of bit length

	mov bx, 0xff * 256 + ':'	; Get integer bit length
	push cs				; Simulate far call
	call sys_str_parse_int
	jc .skip_variable
	test eax, eax			; {q|w}: 32 bit
	je .fixed_dword
	cmp eax, 32			; {q32|w32}: 32 bit
	je .fixed_dword
	test cl, cl
	jz .ufixed
	cmp eax, 16			; {q16}
	je .fixed_word
	cmp eax, 8			; {q8}
	je .fixed_byte
	jmp .skip_variable		; Unknown, skip variable

.ufixed:
	cmp eax, 16			; {w16}
	je .ufixed_word
	cmp eax, 8			; {w8}
	je .ufixed_byte
	jmp .skip_variable

.fixed_dword:
	mov eax, [bp]			; {q|w[32]}: get as 32 bit from stack
	jmp .fixed_precision

.fixed_word:
	movsx eax, word [bp]		; {w16}: sign-extend into eax
	jmp .fixed_precision

.fixed_byte:
	movsx eax, byte [bp]		; {w8}: sign-extend into eax
	jmp .fixed_precision

.ufixed_word:
	movzx eax, word [bp]		; {q16}: zero-extend into eax
	jmp .fixed_precision

.ufixed_byte:
	movzx eax, byte [bp]		; {q8}: zero-extend into eax

.fixed_precision:
	mov ebx, eax

.skip_bit_length:
	a32 lodsb			; Skip bit length specifier
	test al, al
	jz .done
	cmp al, ':'
	jne .skip_bit_length

	push ebx			; Save variable value

	mov ah, '.'			; Check presence of precision
	mov ecx, 0xff
	push cs				; Simulate far call
	call sys_str_char_pos
	mov dh, 0xff			; DH: fixed-point precision
	mov bh, 0xff			; BH: max length of base
	jc .fixed_base			; No precision specified, use maximum

	mov bh, al			; BH: max length of base
	mov ecx, esi			; Save string pointer
	add esi, eax
	inc esi
	push bx
	mov bx, 0xff * 256 + '}'	; Get precision
	push cs				; Simulate far call
	call sys_str_parse_int
	pop bx
	mov esi, ecx			; Restore string pointer
	jc .fixed_base
	cmp eax, 0xff
	ja .fixed_base
	mov dh, al			; DH: requested precision

.fixed_base:
	mov bl, '}'			; Get fixed-point base
	push cs				; Simulate far call
	call sys_str_parse_int
	pop ebx				; Restore variable value into EBX
	jc .skip_variable
	test eax, eax			; No base, invalid, skip
	jz .skip_variable

	mov ecx, eax			; ECX: base
	mov eax, ebx			; EAX: variable value
	mov bx, dx			; Convert number to string
	mov edx, esi			; Save string pointer
	mov esi, edi
	push cs				; Simulate far call
	call sys_str_fixed
	mov esi, edx			; Restore string pointer
	call .skip_dest_to_end		; Skip to end of result string
	jmp .skip_variable

.hex:

	;----------------------------------------------------------------------
	; {x|X[8|16|32]}: lowercase/uppercase hexadecimal of bit length

	mov bx, 0xff * 256 + '}'	; Get hexadecimal bit length
	push cs				; Simulate far call
	call sys_str_parse_int
	mov bl, cl			; BL: lowercase/uppercase
	mov bh, 8			; BH: number of nibbles
	test eax, eax			; {x|X}: 32 bit
	je .str_hex
	cmp eax, 32			; {x32|X32}: 32 bit
	je .str_hex
	sub bh, 4
	cmp eax, 16			; {x16|X16}: 16 bit
	je .str_hex
	sub bh, 2
	cmp eax, 8			; {x8|X8}: 8 bit
	jne .skip_variable		; Unknown, skip variable

.str_hex:
	mov eax, [bp]
	mov edx, esi			; Save string pointer
	mov esi, edi
	push cs				; Simulate far call
	call sys_str_hex
	mov esi, edx			; Restore string pointer
	call .skip_dest_to_end		; Skip to end of result string
	jmp .skip_variable

.char:

	;----------------------------------------------------------------------
	; {c} - Single character

	mov al, [bp]
	mov [edi], al
	inc edi
	jmp .skip_variable

.string:

	;----------------------------------------------------------------------
	; {s[:length]} - String, variable value is pointer to string in DS
	; segment

	mov ecx, -1			; ECX: maximum number of chars to copy
	cmp byte [esi], ':'
	jne .copy_string
	inc esi
	mov bx, 0xff * 256 + '}'	; Get maximum number of chars to copy
	push cs				; Simulate far call
	call sys_str_parse_int
	jc .skip_variable
	cmp eax, 0
	jl .skip_variable
	mov ecx, eax

.copy_string:
	mov ebx, esi			; Save string pointer
	mov esi, [bp]

.copy_string_loop:
	a32 lodsb			; Get next character
	test al, al
	jz .string_end			; End of string
	mov [edi], al			; Copy to target string
	inc edi
	loop .copy_string_loop, ecx

.string_end:
	mov esi, ebx			; Restore string pointer

.skip_variable:

	; Skip after end of variable (closing curly bracket)

	a32 lodsb
	test al, al
	jz .done
	cmp al, '}'
	jne .skip_variable
	jmp .printf_loop


;------------------------------------------------------------------------------
; Skip result string pointer to its end.
;------------------------------------------------------------------------------
; -> DS:EDI - Result ASCIIZ string pointer
; <- AL - Destroyed
;    DS:EDI - Pointer to terminator NUL of result string
;------------------------------------------------------------------------------

.skip_dest_to_end:
	mov al, [edi]
	inc edi
	test al, al
	jnz .skip_dest_to_end
	dec edi
	retn


;------------------------------------------------------------------------------
; Convert an ASCIIZ string containing a 32-bit signed decimal number into an
; actual number.
;------------------------------------------------------------------------------
; -> BL - Terminator character (set to 0 to terminate at the end of string only)
;    BH - Maximum number of characters to convert
;    DS:ESI - Buffer containing decimal numeric string
; <- CF - Set if error (invalid characters found)
;    EAX - Converted number
;------------------------------------------------------------------------------

global sys_str_parse_int
sys_str_parse_int:
	push bx
	push cx
	push edx
	push esi
	push eax

	cld

	movzx cx, bh
	xor edx, edx			; EDX: result number
	xor bh, bh			; BH: sign, 0 = positive, 1 = negative

	test cx, cx			; Count = 0, nothing to convert
	jz .done

	; Check sign (if any)

	a32 lodsb			; Get first character
	cmp al, '+'
	je .convert_loop		; +: ignore
	cmp al, '-'
	jne .check_digit		; Neither +, nor -, treat as digit
	inc bh				; -: set sign flag

.convert_loop:
	a32 lodsb			; Get next character

.check_digit:
	test al, al			; End of string
	jz .done
	cmp al, bl			; Terminator character reached?
	je .done
	cmp al, '0'			; Check for non-numeric characters
	jb .error
	cmp al, '9'
	ja .error

	; Convert digit

	movzx eax, al			; Zero-extend
	cmp edx, 214748364
	ja .error			; Will overflow
	lea edx, [edx * 4 + edx]
	add edx, edx
	sub al, '0'			; Convert to number
	add edx, eax			; Result * 10 + current character
	cmp edx, 0x80000000		; Overflow
	jae .error
	loop .convert_loop, cx

.done:
	add sp, 4			; Discard EAX from stack
	mov eax, edx
	test bh, bh
	jz .positive
	neg eax

.positive:
	clc

.exit:
	pop esi
	pop edx
	pop cx
	pop bx
	retf

.error:
	stc
	pop eax
	jmp .exit


;------------------------------------------------------------------------------
; Convert an ASCIIZ string containing a 32-bit signed floating point decimal
; number into an actual fixed point number.
;------------------------------------------------------------------------------
; -> BL - Terminator character (set to 0 to terminate at the end of string only)
;    BH - Maximum number of characters to convert
;    ECX - Fixed point base (value representing 1)
;    DS:ESI - Buffer containing decimal numeric string
; <- CF - Set if error (invalid characters found)
;    EAX - Converted number
;------------------------------------------------------------------------------

global sys_str_parse_fixed
sys_str_parse_fixed:
	push bx
	push edx
	push esi
	push edi
	push ebp
	push eax
	push ecx

	cld

	test ecx, ecx
	jz .error			; Base can't be 0

	;----------------------------------------------------------------------
	; Setup values

	; Calculate overflow limits

	mov eax, 0x80000000
	xor edx, edx
	div ecx
	mov ebp, eax			; EBP: integer overflow limit
	test edx, edx
	setnz dl			; DL: 1 when remainder > 0, else 0
	movzx edx, dl
	add ebp, edx			; EBP: ceil(0x80000000 / ECX)

	; Initialize registers

	movzx cx, bh			; CL: number of chars
					; CH: number of decimal digits
	xor edx, edx			; EDX: result integer part
	xor edi, edi			; EDI: result fraction part
	xor bh, bh			; BH: flags
					;     bit 0: negative (0x01)
					;     bit 1: fractions (0x02)

	test cl, cl			; Count = 0, nothing to convert
	jz .done

	;----------------------------------------------------------------------
	; String conversion

	; Check sign (if any)

	a32 lodsb			; Get first character
	cmp al, '+'
	je .convert_loop		; +: ignore
	cmp al, '-'
	jne .check_digit		; Neither +, nor -, treat as digit
	inc bh				; -: set sign flag

.convert_loop:
	a32 lodsb			; Get next character

.check_digit:
	test al, al			; End of string
	jz .done
	cmp al, bl			; Terminator character reached?
	je .done
	cmp al, '.'			; Decimal point
	je .decimal
	cmp al, '0'			; Check for non-numeric characters
	jb .error
	cmp al, '9'
	ja .error

	; Convert digit

	movzx eax, al			; Zero-extend
	sub al, '0'			; Convert to number
	test bh, 0x02
	jz .convert_integer

	; Add to fraction part

	cmp edi, 214748364
	ja .frac_will_overflow		; Fraction will overflow
	je .frac_may_overflow		; Fraction may overflow
	cmp ch, 9
	je .frac_will_overflow		; Fraction will overflow

.frac_no_overflow:
	lea edi, [edi * 4 + edi]
	add edi, edi
	add edi, eax			; Fraction * 10 + current character
	inc ch
	jmp .next_digit

.convert_integer:

	; Add to integer part

	cmp edx, 214748364
	ja .error			; Will overflow, error
	lea edx, [edx * 4 + edx]
	add edx, edx
	add edx, eax			; Result * 10 + current character
	cmp edx, ebp
	jae .error			; Integer overflow, error

.next_digit:
	dec cl
	jnz .convert_loop

.done:

	;----------------------------------------------------------------------
	; Convert to fixed point base, combine integer and fraction parts of
	; the parsed number

	; Calculate fraction 10-base divider (10 ^ (number of decimals))

	mov ebp, 1			; EBP: 10 ^ (number of decimals)
	test ch, ch
	jz .apply_base

.frac_tenbase_loop:
	lea ebp, [ebp * 4 + ebp]
	add ebp, ebp			; EBP *= 10
	dec ch
	jnz .frac_tenbase_loop

.apply_base:

	; Apply fixed point base on integer part

	mov eax, edx
	pop ecx				; ECX: fixed point base
	mul ecx				; EAX: integer * base
	mov esi, eax			; ESI: fixed point value integer part
	test bh, 0x02
	jz .integer

	; Apply fixed point base on fraction part

	mov eax, edi
	mul ecx
	div ebp
	shr ebp, 1			; Rounding
	cmp edx, ebp
	setae dl
	movzx edi, dl
	add edi, eax			; EDI: fixed point value fraction part
	add esi, edi

.integer:

	; Final overflow test, return parsed number if no overflow

	cmp esi, 0x80000000
	jae .error_final		; 32-bit overflow
	add sp, 4			; Discard EAX from stack
	mov eax, esi
	test bh, 0x01
	jz .positive
	neg eax				; Negate value when negative

.positive:
	clc

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop bx
	retf

	;----------------------------------------------------------------------
	; Fraction part handling

.decimal:

	; Handle decimal point character in string. Set decimal flag to treat
	; the rest of the string as fraction part.

	test bh, 0x02
	jnz .error			; More, than one decimal point
	or bh, 0x02			; Set fractions flag
	dec cl
	jnz .convert_loop
	jmp .done

.frac_will_overflow:

	; Fraction part will overflow with current digit. Use current digit to
	; round fraction value and stop further parsing.

	cmp al, 5
	setae al
	movzx eax, al
	add edi, eax
	jmp .done

.frac_may_overflow:

	; Fraction part may overflow if current digit is > 7. Apply digit if
	; <= 7 or round up fraction part and stop further parsing.

	cmp al, 7
	jbe .frac_no_overflow
	inc edi
	jmp .done

.error:
	pop ecx

.error_final:
	stc
	pop eax
	jmp .exit


;------------------------------------------------------------------------------
; Convert an ASCIIZ string containing a 32-bit hexadecimal number into an
; actual number.
;------------------------------------------------------------------------------
; -> BL - Terminator character (set to 0 to terminate at the end of string only)
;    BH - Maximum number of characters to convert
;    DS:ESI - Buffer containing hexadecimal numeric string
; <- CF - Set if error (invalid characters found)
;    EAX - Converted number
;------------------------------------------------------------------------------

global sys_str_parse_hex
sys_str_parse_hex:
	push bx
	push cx
	push edx
	push esi
	push eax

	cld

	movzx cx, bh
	xor edx, edx			; EDX: result number
	xor bh, bh			; BH: sign, 0 = positive, 1 = negative

	test cx, cx			; Count = 0, nothing to convert
	jz .done

.convert_loop:
	a32 lodsb			; Get next character
	test al, al			; End of string
	jz .done
	cmp al, bl			; Terminator character reached?
	je .done
	cmp al, '0'			; Check for non-numeric characters
	jb .error
	cmp al, '9'
	jbe .number
	cmp al, 'A'
	jb .error
	cmp al, 'F'
	jbe .hex
	cmp al, 'a'
	jb .error
	cmp al, 'f'
	ja .error
	sub al, 'a' - 10		; a - z: lowercase hexadecimal digit
	jmp .add_digit

.hex:
	sub al, 'A' - 10		; A - Z: uppercase hexadecimal digit
	jmp .add_digit

.number:
	sub al, '0'			; 0 - 9: number digit

.add_digit:

	; Convert digit

	test edx, 0xf0000000
	jnz .error			; Overflow
	shl edx, 4
	movzx eax, al			; Zero-extend
	add edx, eax			; Result * 10 + current character
	loop .convert_loop, cx

.done:
	add sp, 4			; Discard EAX from stack
	mov eax, edx
	clc

.exit:
	pop esi
	pop edx
	pop cx
	pop bx
	retf

.error:
	stc
	pop eax
	jmp .exit


;==============================================================================
; Data area
;==============================================================================

		alignb 4
intlentab	dd 9, 99, 999, 9999, 99999, 999999,
		dd 9999999, 99999999, 999999999, 0
