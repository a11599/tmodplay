;==============================================================================
; MOD player GUI - Drawing routines
;------------------------------------------------------------------------------
; Supports EGA/VGA 16-color planar video modes as long as they fit into the
; 0xA000 segment.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/string.inc"
%include "rtl/api/log.inc"

%include "gui/config.inc"
%include "gui/api/setup.inc"
%include "gui/consts/public.inc"


;------------------------------------------------------------------------------
; Fill middle (full pixels) of box - used by gui_draw_box
;------------------------------------------------------------------------------
; -> EDI - Video memory offset for top-left pixel character
;    EBX - Box height
;    EDX - Increment to advance to beginning of next row
;    ESI - Number of full words to fill in the middle of the box
;    %1 - Number of characters to write to video memory before word fill
;    %2 - 1 = Write one word, 2 = write SI number of words to video memory
;    %3 - Number of characters to write to video memory after word fill
; <- EAX, ECX, EDI (if %2 = 1) - Destroyed
;------------------------------------------------------------------------------

%macro	fill_mid 3

	mov eax, ebx			; EAX: row counter

%%fill_mid_loop:
	sub eax, FILL_UNROLL		; Fill rows in bulk when possible
	js %%fill_mid_rest

	%rep FILL_UNROLL

	; Character-fill start

	%rep %1
	stosb
	%endrep

	; Fill one or more words

	%if %2 = 1
	stosw
	%elif %2 = 2
	mov ecx, esi
	rep stosw
	%endif

	; Character-fill end

	%rep %3
	stosb
	%endrep

	add edi, edx

	%endrep

	jmp %%fill_mid_loop

%%fill_mid_rest:
	add eax, FILL_UNROLL
	jz draw_box_fill_mid_done

%%fill_mid_rest_loop:

	; Character-fill start

	%rep %1
	stosb
	%endrep

	; Fill words

	%if %2 = 1
	stosw
	%elif %2 = 2
	mov ecx, esi
	rep stosw
	%endif

	; Character-fill end

	%rep %3
	stosb
	%endrep

	add edi, edx
	dec eax
	jnz %%fill_mid_rest_loop
	jmp draw_box_fill_mid_done

%endmacro


;------------------------------------------------------------------------------
; Draw a filled box to the screen.
;------------------------------------------------------------------------------
; -> EBX - Height of box in pixels (signed)
;    ECX - Width of box in pixels (signed)
;    DL - Color of the box (0 - 15)
;    ESI - X coordinate of top left point in pixels (signed)
;    EDI - Y coordinate of top left point in pixels (signed)
;------------------------------------------------------------------------------
; Coordinates and dimensions are signed and may be negative. A negative
; coordinate means the box is shifted upwards and/or left by the negative
; amount given in the coordinate relative to the top-left of the screen. A
; negative dimension means the box is drawn upwards and/or to the left of the
; start coordinate. The box is clipped to stay within the screen's physical
; width and height.
;------------------------------------------------------------------------------

	align 4

global gui_draw_box
gui_draw_box:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	;----------------------------------------------------------------------
	; Box boundary checks

	cmp ebx, 0
	je draw_box_done		; No height, done
	jg .check_width
	add edi, ebx			; Move top by negative height
	neg ebx				; Turn height into positive

.check_width:
	cmp ecx, 0
	je draw_box_done		; No width, done
	jg .check_top
	add esi, ecx			; Move left by negative width
	neg ecx				; Turn width into positive

.check_top:
	cmp edi, 0
	jge .check_left
	add ebx, edi			; Decrease height by negative top
	jle draw_box_done		; Still negative or 0, done
	xor edi, edi			; Top is now at 0

.check_left:
	cmp esi, 0
	jge .check_bottom
	add ecx, esi			; Decrease width by negative left
	jle draw_box_done		; Still negative or 0, done
	xor esi, esi			; Left is now at 0

.check_bottom:
	mov eax, [gui_scr_height]
	cmp edi, eax
	jge draw_box_done		; Beyond bottom, done
	sub eax, edi
	cmp ebx, eax
	jle .check_right
	mov ebx, eax			; Clamp height to stay within bottom

.check_right:
	mov eax, [gui_scr_width]
	cmp esi, eax
	jge draw_box_done		; Beyond right, done
	sub eax, esi
	cmp ecx, eax
	jle .draw_box
	mov ecx, eax			; Clamp width to stay within right

.draw_box:
	set_graph_reg 0, dl		; Set/Reset register: fill color
	set_graph_reg 1, 0x0f		; Enable Set/Reset on all bitplanes
	set_graph_reg 3, 0		; Ignore latched data, don't rotate
	set_sequencer_reg 2, 0x0f	; Enable writes to all planes

	xor eax, eax
	mov eax, edi
	mov edi, [gui_rowtab + eax * 4]
	mov edx, esi			; DX: left start coordinate
	sar edx, 3
	lea edi, [edi + edx + 0xa0000]	; EDI: start address in video RAM

	;----------------------------------------------------------------------
	; Calculate pixels to fill

	mov edx, esi
	and edx, 0x0007			; EDX: first pixel from start address
	lea esi, [ecx + edx]		; ESI: last pixel from start address
	cmp esi, 8
	jbe draw_box_fill_single	; Fill affects one character only

	mov al, [fill_startmask + edx]
	and esi, 0x0007			; AL: start character pixel fill mask
	mov ah, [fill_endmask + esi]	; AH: end character pixel fill mask

	mov esi, 8
	sub esi, edx			; ESI: number of pixels in start char
	sub ecx, esi			; ECX: width - pixels in start character
	shr ecx, 3			; ECX: number of full characters to fill
	mov edx, eax			; DX: pixel fill masks

	; EDI: video memory offset for top-left pixel character
	; EBX: box height
	; ECX: number of characters to fill in the middle of the box
	; DL: start character pixel fill mask, 0 if no partial start char fill
	; DH: end character pixel fill mask, 0 if no partial end char fill
	; EBP: video memory offset backup

	mov ebp, edi			; EBP: video memory offset backup
	mov esi, [gui_plane_chars]
	cmp dl, 0xff
	jne .fill_start
	inc ecx				; First is also a full character
	jmp .fill_mid

.fill_start:

	;----------------------------------------------------------------------
	; Fill start character of box vertically
	; EDI: video memory offset for top-left pixel character
	; EBX: box height
	; DL: start character pixel fill mask
	;     (destroyed by this fill routine)

	push edx
	set_graph_reg 8, dl		; Set start character pixel bitmask
	pop edx
	mov eax, ebx			; EAX: row counter

.fill_start_loop:
	sub eax, FILL_UNROLL		; Fill rows in bulk when possible
	js .fill_start_rest
	%rep FILL_UNROLL
	or byte [edi], al		; Set pixels, value doesn't matter
	add edi, esi			; Next row
	%endrep
	jmp .fill_start_loop

	align 4

.fill_start_rest:
	add eax, FILL_UNROLL		; Fill rest of rows
	jz .fill_start_done

.fill_start_rest_loop:
	or byte [edi], al		; Set pixels, value doesn't matter
	add edi, esi			; Next row
	dec eax
	jnz .fill_start_rest_loop

.fill_start_done:
	inc ebp				; Go to next character in video memory
	mov edi, ebp

.fill_mid:
	add ebp, ecx			; Set to char after mid area in vmem

	;----------------------------------------------------------------------
	; Fill middle of box
	; EDI: video memory offset for top-left full pixel character
	; EBX: box height
	; ECX: number of characters to fill in the middle of the box
	;     (destroyed by this fill routine)
	; DH: end character pixel fill mask, 0 if no partial end character fill

	push edx
	set_graph_reg 8, 0xff		; Set all pixels in character
	pop edx

	cmp ecx, 1
	jb fill_end			; No middle part, fill last one
	push edx			; Save pixel fill mask
	mov edx, esi
	je .fill_mid_single		; Fill only a single character

	sub edx, ecx			; EDX: pointer adjustment for next row
	mov esi, ecx
	shr esi, 1			; ESI: number of words to fill

	xor eax, eax			; Calculate bits for optimal fillroutine
	and cl, 1
	add cl, cl
	test edi, 1
	setnz al			; EAX bit 0: 1 if odd-aligned
	or al, cl			; EAX bit 1: 1 if odd number of bytes
	cmp esi, 2
	setae cl
	shl cl, 2
	add al, cl			; EAX bit 2: 1 if word count >= 2
	jmp [.fill_mids + eax * 4]

	align 4

.fill_mid_single:
	sub edx, ecx			; EDX: pointer adjustment for next row
	fill_mid 1, 0, 0		; Fill single character (8 pixels)

	align 4

.fill_mids:
	dd .fill_mid_1w_even_even	; 000 - 1-word, even-aligned, even count
	dd .fill_mid_1w_odd_even	; 001 - 1-word, odd-aligned, even count
	dd .fill_mid_1w_even_odd	; 010 - 1-word, even-aligned, odd count
	dd .fill_mid_1w_odd_odd		; 011 - 1-word, odd-aligned, odd count
	dd .fill_mid_nw_even_even	; 100 - N-word, even-aligned, even count
	dd .fill_mid_nw_odd_even	; 101 - N-word, odd-aligned, even count
	dd .fill_mid_nw_even_odd	; 110 - N-word, even-aligned, odd count
	dd .fill_mid_nw_odd_odd		; 111 - N-word, odd-aligned, odd count

	align 4

.fill_mid_1w_even_even:			; 1-word, even-aligned, even count (2B)
.fill_mid_1w_odd_even:			; 1-word, odd-aligned, even count (2B)
	fill_mid 0, 1, 0		; Fill: word

	align 4

.fill_mid_1w_even_odd:			; 1-word, even-aligned, odd count (3B)
	fill_mid 0, 1, 1		; Fill: word, byte

	align 4

.fill_mid_1w_odd_odd:			; 1-word, odd-aligned, odd count (3B)
	fill_mid 1, 1, 0		; Fill: byte, word

	align 4

.fill_mid_nw_even_even:			; N-word, even-aligned, even count
	fill_mid 0, 2, 0		; Fill: word+

	align 4

.fill_mid_nw_odd_even:			; N-word, odd-aligned, even count
	dec esi				; One less word due to byte paddings
	fill_mid 1, 2, 1		; Fill: byte, word+, byte

	align 4

.fill_mid_nw_even_odd:			; N-word, even-aligned, odd count
	fill_mid 0, 2, 1		; Fill: word+, byte

	align 4

.fill_mid_nw_odd_odd:			; N-word, odd-aligned, odd count
	fill_mid 1, 2, 0		; Fill: byte, word+

	align 4

draw_box_fill_mid_done:
	pop edx				; Restore pixel fill mask

	test dh, dh
	jz draw_box_done		; No partial end fill
	mov edi, ebp			; Go to last pixel mask character

fill_end:

	;----------------------------------------------------------------------
	; Fill last (or only) column of box vertically
	; ES:DI: video memory offset for top-right pixel character
	; BX: box height
	; DH: end character pixel fill mask

	set_graph_reg 8, dh		; Set last pixel bitmask

	mov eax, ebx
	mov esi, [gui_plane_chars]

.fill_end_loop:
	sub eax, FILL_UNROLL		; Fill rows in bulk when possible
	js .fill_end_rest
	%rep FILL_UNROLL
	or byte [edi], al		; Set pixels, value doesn't matter
	add edi, esi			; Next row
	%endrep
	jmp .fill_end_loop

	align 4

.fill_end_rest:
	add eax, FILL_UNROLL		; Fill rest of rows
	jz draw_box_done

.fill_end_rest_loop:
	or byte [edi], al		; Set pixels, value doesn't matter
	add edi, esi			; Next row
	dec eax
	jnz .fill_end_rest_loop

draw_box_done:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret

	align 4

draw_box_fill_single:
	mov dh, [fill_startmask + edx]
	and dh, [fill_endmask + esi]
	jmp fill_end


;------------------------------------------------------------------------------
; Blit an interleave of the bitmap to video RAM - used by gui_blit_bitmap
;------------------------------------------------------------------------------
; -> ESI - Interleaved bitmap data
;    EDI - Video memory offset for top-left bitmap character
;    BL - Bitmap height in pixels
;    CL - Bitmap width in 64 pixel units (8 characters/bytes)
;    EDX - Increment to advance to beginning of next row
;    %1 - Interleave index to blit (0 or 1)
; <- EAX, EBX, CH, ESI, EDI - Destroyed
;------------------------------------------------------------------------------

%macro	blit_bitmap_ilv 1

	%assign plane %1

	mov bl, cl			; CH: bitmap width in 64 pixel units
	dec edi				; Prepare for pre-incrementing

	align 4

%%octa_char_loop:

	; The 64-pixel units are drawn in an unrolled loop to minimize branches
	; as much as possible.

	%assign setchar 0
	%assign unroll 8
	%assign reps unroll

	%rep unroll + 1

	align 4

	; For all, but the first repeat, define a %%set_char_N label to jump to
	; if any of the interleaves contained pixel data. Set the character in
	; video RAM and clear the character in the other interleave.

	%if (setchar > 0)

%%set_char_ %+ setchar:
	%if (%1 = 0)
	mov [edi], al			; AL: character data (interleave 0)
	mov byte [esi - 1], 0		; Clear character in interleave 1
	%else
	mov [edi], ah			; AH: character data (interleave 1)
	mov byte [esi - 2], 0		; Clear character in interleave 0
	%endif

	%endif

	; For all, but the last repeat, get the next characters from the
	; interleave, adjust interleave offset and video RAM destination address
	; and compare interleave characters. If they are different, branch out
	; to %%set_char_N to set the character in video RAM and to clear the
	; other interleave. If they are the same, continue with the next
	; character in the interleave buffer.

	%if (reps > 0)

	%assign char setchar + 1
	%rep reps

	mov ax, [esi]			; Get characters from both interleaves
	inc edi				; Pre-increment video memory address
	add esi, 2			; Go to next character in interleave
	%if (%1 = 0)
	or ah, al			; Check if any interleave has data
	%else
	or al, ah
	%endif
	jnz %%set_char_ %+ char		; Yes, blit and clear other interleave

	%assign char char + 1
	%endrep

	%endif

	; Continue drawing the next 64 pixels and all rows of the bitmap. Jump
	; to the given address if the entire bitmap has been blitted.

	dec cl
	jnz %%octa_char_loop

	mov cl, bl			; CL: number of octa-characters
	add edi, edx			; Adjust video RAM address to next row
	dec ch
	jnz %%octa_char_loop

	jmp .done

	%assign setchar setchar + 1
	%assign reps reps - 1
	%endrep

%endmacro


;------------------------------------------------------------------------------
; Blits a monochrome bitmap on top of an existing one by overwriting only
; changed characters.
;------------------------------------------------------------------------------
; -> EAX - Interleaved bitmap data
;    CL - Bitmap width in 64 pixel units (8 characters/bytes)
;    CH - Bitmap height in pixels
;    DL - Target bitplane mask (0 - 15, color), won't change other planes
;    DH - Target bitmap image data interleave (0 or 1) to blit
;    ESI - X coordinate of top left point in pixels (signed, character aligned)
;    EDI - Y coordinate of top left point in pixels (signed)
; <- EAX - End of interleaved bitmap data
;------------------------------------------------------------------------------
; Bitmap data is a monochromatic (8 pixel per byte/character) image where the
; previous and current character values are interleaved. The bitmap must fit to
; the screen dimensions, no clipping is done!
;------------------------------------------------------------------------------

	align 4

global gui_blit_bitmap_interleave
gui_blit_bitmap_interleave:
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	cmp edi, [gui_scr_height]	; Safeguard against out of bound row
	jae .done

	mov ebp, eax			; EBP: Interleaved bitmap
	mov edi, [gui_rowtab + edi * 4]
	mov ebx, edx			; BL: color, BH: target interleave
	shr esi, 3
	mov al, bh			; AL: target interleave
	lea edi, [edi + esi + 0xa0000]	; EDI: start address in video RAM

	set_sequencer_reg 2, bl		; Write to specific planes (set color)
	set_graph_reg 1, 0		; Disable set/reset
	set_graph_reg 3, 0		; Ignore latched data, don't rotate
	set_graph_reg 8, 0xff		; Use all 8 bits of data written

	movzx esi, cl
	mov edx, [gui_plane_chars]
	shl esi, 3
	sub edx, esi			; DX: number of characters to next row

	mov esi, ebp			; ESI: interleaved bitmap data
	mov bl, ch			; BL: bitmap height
	test bh, bh
	jnz .interleave_1

	blit_bitmap_ilv 0		; Jumps to .done when ready

	align 4

.interleave_1:
	blit_bitmap_ilv 1

.done:
	mov eax, esi			; Advance bitmap offset
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret


;------------------------------------------------------------------------------
; Returns the width of a text when rendered to the screen.
;------------------------------------------------------------------------------
; -> EBX - Pointer to ASCIIZ string with text to render
;    EBP - Pointer to font definition
; <- ECX - Width of text in pixels
;------------------------------------------------------------------------------

	align 4

global gui_text_width
gui_text_width:
	push eax
	push ebx
	push esi
	push edx
	push edi

	mov ax, [ebp]			; AL, AH: first/last char code in font
	xor ecx, ecx			; ECX: text width in pixels
	mov edi, ebx			; EDI: text pointer

	; Get width of first character

	xor edx, edx
	mov dl, [edi]			; DL: character
	test dl, dl
	jz .done
	movzx esi, byte [ebp + 4]	; ESI: letter spacing
	cmp dl, al			; Check glyph range
	jb .no_glyph_first
	cmp dl, ah
	ja .no_glyph_first
	sub dl, al
	mov dx, [ebp + edx * 2 + 8]	; EDX: offset of glyph definition
	test edx, edx
	jz .no_glyph_first
	mov cl, [ebp + edx + 1]		; ECX: width of first character
	jmp .char_width_loop

.no_glyph_first:
	mov cl, [ebp + 3]		; ECX: space width

	; Get width of subsequent characters (also add letter spacing)

.char_width_loop:
	inc edi
	xor edx, edx
	mov dl, [edi]			; DL: character
	test dl, dl
	jz .done
	cmp dl, al			; Check glyph range
	jb .no_glyph
	cmp dl, ah
	ja .no_glyph
	sub dl, al
	mov ebx, esi			; BX: letter spacing
	mov dx, [ebp + edx * 2 + 8]	; EDX: offset of glyph definition
	test edx, edx
	jz .no_glyph
	add bl, [ebp + edx + 1]		; BX: letter spacing + character width
	add ecx, ebx
	jmp .char_width_loop

.no_glyph:
	add bl, [ebp + 3]		; BX: letter spacing + space width
	add ecx, ebx
	jmp .char_width_loop

.done:
	pop edi
	pop edx
	pop esi
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Render text to the screen.
;------------------------------------------------------------------------------
; -> AL - Text alignment (GUI_AL_*)
;    EBX - Pointer to ASCIIZ string with text to render
;    ECX - Width of container for rendered text
;    DL - Color of the text (0 - 15)
;    DH - Background color (0 - 15)
;    ESI - X coordinate of render origin in pixels (signed)
;    EDI - Y coordinate of top of render origin in pixels (signed)
;    EBP - Pointer to font definition
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------
; The text will be rendered into the given container. The container is filled
; with the background color provided in DH. Excess text is cut off.
;------------------------------------------------------------------------------

	align 4

global gui_draw_text
gui_draw_text:
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push eax

	cmp byte [ebp + 2], MAX_FONT_HEIGHT
	jae .error_font

	cmp ecx, 0
	jle .done			; Maximum width reached

	push ecx
	push edx
	push esi
	push edi

	;----------------------------------------------------------------------
	; Clear one line of text in render buffer

	push eax
	push ecx
	push edi

	movzx ecx, byte [ebp + 2]
	mov ecx, [gui_buf_rowtab + ecx * 4]
	xor eax, eax
	mov edi, [gui_buf_addr]
	shr ecx, 2
	rep stosd

	pop edi
	pop ecx
	pop eax

	;----------------------------------------------------------------------
	; Render text to memory render buffer

	; Handle text alignment

	cmp al, GUI_AL_RIGHT
	jb .render_text			; Align left
	je .align_right

	; Align center

	mov eax, ecx			; EAX: container width
	call gui_text_width
	sub eax, ecx			; EAX: container width - text width
	sar eax, 1			; EAX: (container - text width) / 2
	add esi, eax			; ESI: X + (container - text width) / 2
	jmp .clip_text_left

.align_right:

	; Align right

	mov eax, ecx			; EAX: container width
	call gui_text_width
	add esi, eax			; ESI: X + container width
	sub esi, ecx			; ESI: X + container width - text width

.clip_text_left:

	; Clip text from left to avoid buffer underrun

	cmp esi, 0
	jge .render_text

	xor edx, edx
	mov ax, [ebp]			; AL, AH: first/last char code in font
	mov dl, [ebx]			; EDX: character code
	test dl, dl
	jz .render_text			; End of string, start rendering
	cmp dl, al
	jb .clip_text_no_glyph		; Below first glyph, render space
	cmp dl, ah
	ja .clip_text_no_glyph		; Above last glyph, render space
	sub dl, al
	mov dx, [ebp + edx * 2 + 8]	; EDX: index to glyph
	test edx, edx
	jz .clip_text_no_glyph
	movzx edx, byte [ebp + edx + 1]	; EDX: width of character
	jmp .clip_text_check_bound

.clip_text_no_glyph:
	xor edx, edx
	mov dl, [ebp + 3]		; EDX: width of space character

.clip_text_check_bound:
	add edx, esi			; X coordinate would be positive?
	jns .render_text		; Yes, start rendering
	mov esi, edx			; No, adjust start closer to 0
	movzx edx, byte [ebp + 4]	; Add letter spacing
	add esi, edx
	inc ebx				; Skip character
	jmp .clip_text_left

.render_text:

	; Determine start X coordinate

	mov eax, ecx			; EAX: maximum text width in pixels
	mov ecx, esi
	and cl, 0x07			; CL: bit shift
	sar esi, 3
	add esi, [gui_buf_addr]		; ESI: render buffer address
	add esi, BUF_OFLOW_CHARS / 2	; Add padding
	mov edi, [gui_plane_chars]
	add edi, BUF_OFLOW_CHARS	; EDI: bytes to add for next row

	push esi

.render_text_loop:

	; Register usage
	; EAX: maximum text width
	; EBX: ASCIIZ string current character pointer
	; CL: glyph bitmap rotation
	; CH: glyph height
	; EDX: glyph definition
	; ESI: render buffer pointer
	; EDI: bytes to add for next row in render buffer
	; EBP: font definition

	xor edx, edx
	mov dl, [ebx]
	test dl, dl			; DL: character to render
	jz .blit_text			; End of string

	sub dl, [ebp]			; Check font character code boundaries
	jc .no_glyph
	cmp dl, [ebp + 1]
	ja .no_glyph

	mov dx, [ebp + edx * 2 + 8]
	test edx, edx
	jz .no_glyph			; No glyph, render space
	add edx, ebp			; EDX: glyph definition
	mov ch, [edx + 2]		; CH: glyph height
	test ch, ch
	jz .render_empty		; No height, skip pixels

	; Render glyph

	push eax
	push ebx
	push edx
	push esi

	; Register usage
	; AL, AH: glyph bitmap, glyph bitmap overflow (after rotate)
	; BL, BH: bytes per row
	; CL: glyph bitmap rotation
	; CH: glyph height
	; EDX: glyph definition
	; ESI: render buffer pointer
	; EDI: bytes to add for next row in render buffer
	; EBP: font definition

	xor eax, eax
	mov al, [edx + 3]		; AL: rows to skip
	mov eax, [gui_buf_rowtab + eax * 4]
	add esi, eax			; ESI: adjust for skipped rows

	xor ebx, ebx
	mov bl, [edx]			; BL: bytes per row
	sub edi, ebx			; EDI: bytes to add for next row - width
	add edx, 4			; EDX: glyph bitmap
	mov bh, bl			; BH: bytes per row

.render_glyph_row_loop:
	xor eax, eax
	mov al, [edx]			; AL: glyph bitmap
	ror ax, cl			; AH: overflow for next character
	or [esi], ax			; Add pixels to render buffer
	inc edx
	inc esi
	dec bl
	jnz .render_glyph_row_loop

	add esi, edi			; Next row
	mov bl, bh
	dec ch
	jnz .render_glyph_row_loop

	xor bh, bh
	add edi, ebx			; EDI: restore bytes to add for next row

	pop esi
	pop edx
	pop ebx
	pop eax

.render_empty:
	mov ch, [edx + 1]		; CH: glyph width

.next_char:
	add ch, [ebp + 4]		; Character spacing
	xor edx, edx
	mov dl, ch
	and ch, 0x07			; CH: glyph width % 8
	sub eax, edx			; EAX: maximum width left
	jle .blit_text			; Maximum width reached
	shr dl, 3			; EDX: glyph byte count
	add cl, ch
	inc ebx				; EBX: next character in string
	shl cl, 5			; Bit shift overflow (3rd bit) to CF
	adc esi, edx			; ESI: adjust render buffer pointer
	shr cl, 5			; CL: adjusted bit shift
	jmp .render_text_loop

.no_glyph:
	mov ch, [ebp + 3]		; CH: space width
	jmp .next_char

.blit_text:
	pop eax				; EAX: render buffer start address
	pop edi
	pop esi

	; Calculate render buffer start offset for blitting

	mov eax, esi
	sar eax, 3
	add eax, [gui_buf_addr]		; EAX: render buffer address
	add eax, BUF_OFLOW_CHARS / 2	; Add padding

	pop ebx				; BL: text color, BH: background color
	pop ecx
	push eax			; Render buffer start address

	;----------------------------------------------------------------------
	; Copy text from render buffer to video memory

	set_graph_reg 0, bh		; Set/Reset register: background color
	set_graph_reg 1, 0x0f		; Enable Set/Reset on all bitplanes
	set_graph_reg 3, 0		; Ignore latched data, don't rotate
	set_sequencer_reg 2, 0x0f	; Enable writes to all planes

	mov edi, [gui_rowtab + edi * 4]
	mov edx, esi			; EDX: left start coordinate
	sar edx, 3
	lea edi, [edi + edx + 0xa0000]	; EDI: start address in video RAM
	xor bl, bh
	xor bl, 0x0f			; BL: text XNOR background color

	; Calculate pixels to fill

	mov edx, esi
	and edx, 0x0007			; EDX: first pixel from start address
	mov esi, ecx
	add esi, edx			; ESI: last pixel from start address
	cmp esi, 8
	jbe .blit_single_char

	mov al, [fill_startmask + edx]
	and esi, 0x0007			; AL: start character pixel fill mask
	mov ah, [fill_endmask + esi]	; AH: end character pixel fill mask

	;----------------------------------------------------------------------
	; Create pixels for latch data in offscreen video memory
	;----------------------------------------------------------------------
	; Background color is first set in offscreen video memory for the first,
	; middle and last characters. The color is set to the Set/Reset register
	; and the mask register is used to overwrite pixels in the character.

	; EDI: video memory offset for top-left pixel character
	; BL: text color mask (text color XNOR background color)
	; CL: number of characters to fill in the middle of the box
	; CH: font height
	; DL: start character pixel fill mask, 0 if no partial start char fill
	; DH: end character pixel fill mask, 0 if no partial end char fill
	; EBP: video memory offscreen address

	mov esi, 8
	sub esi, edx			; ESI: number of pixels in start char
	sub ecx, esi			; ECX: width - pixels in start character
	shr ecx, 3			; CL: number of full characters to fill
	mov edx, eax			; DX: pixel fill masks
	mov ch, [ebp + 2]		; CH: font height
	mov ebp, [gui_offscr_addr]	; EBP: offscreen address in video RAM
	mov esi, [gui_plane_chars]	; ESI: number of characters per row

	; Draw background characters in offscreen video memory to be used as
	; latch data for text blitting

	push edx
	cmp dl, 0xff
	je .skip_first_char_bg		; No partial first character, skip
	set_graph_reg 8, dl		; Set start character pixel bitmask
	mov ah, ch

.first_char_bg_loop:
	mov al, [edi]
	mov [ebp], al			; Draw background for first partial char
	add edi, esi
	inc ebp
	dec ah
	jnz .first_char_bg_loop

.mid_char_bg:
	test cl, cl
	jz .last_char_bg
	set_graph_reg 8, 0xff		; Set middle character pixel bitmask
	mov byte [ebp], 0xff		; Draw background for middle characters

.last_char_bg:
	pop edx
	test dh, dh
	jz .skip_last_char_bg
	push edx
	set_graph_reg 8, dh		; Set start character pixel bitmask

	xor eax, eax
	mov al, cl
	add edi, eax
	mov ah, ch

.last_char_bg_loop:
	sub edi, esi			; Bottom to top!
	inc ebp
	mov al, [edi]
	mov [ebp], al			; Draw background for last partial char
	dec ah
	jnz .last_char_bg_loop

	xor eax, eax
	mov al, cl
	sub edi, eax			; EDI: video memory target start address

	pop edx

.first_char_text:

	;----------------------------------------------------------------------
	; Blit text on top of background color
	;----------------------------------------------------------------------
	; The latch is filled from the offscreen video memory prepared in the
	; previous step and the text bitmap is applied using an XOR operation.
	; The Set/Reset register is programmed with zeroes and planes where
	; the background and foreground color's bits are the same are taken from
	; the Set/Reset register to keep the plane's pixels unaltered (Enable
	; Set/Reset is programmed with text color XNOR background color). The
	; rest of the planes are XORed with the rendered text bitmap data.

	pop esi				; ESI: render buffer start address

	push edx
	cmp dl, 0xff
	je .skip_first_char_text
	set_graph_reg 8, dl		; Set start character pixel bitmask
	set_graph_reg 0, 0		; Set/Reset register: all zero
	set_graph_reg 1, bl		; Enable Set/Reset on color mask
	set_graph_reg 3, 11000b		; XOR, no rotate

	mov ebp, [gui_offscr_addr]	; EBP: offscreen address in video RAM
	mov ebx, [gui_plane_chars]	; EBX: number of characters per row

	push esi
	push edi
	mov ah, ch

.first_char_text_loop:
	mov al, [ebp]			; Fill latch
	mov al, [esi]
	mov [edi], al			; Draw first partial character
	add edi, ebx
	add esi, ebx
	inc ebp
	add esi, BUF_OFLOW_CHARS
	dec ah
	jnz .first_char_text_loop

	pop edi
	pop esi
	inc edi
	inc esi

.mid_char_text:
	test cl, cl
	jz .skip_mid_char_text
	set_graph_reg 8, 0xff		; Set mid character pixel bitmask
	mov al, [ebp]			; Fill latch
	mov ah, ch			; AH: font height
	mov al, ch
	movzx ecx, cl
	sub ebx, ecx
	mov edx, ecx

.mid_char_text_loop:
	rep movsb			; Draw middle full characters
	add edi, ebx
	add esi, ebx
	mov ecx, edx
	add esi, BUF_OFLOW_CHARS
	dec al
	jnz .mid_char_text_loop

	sub edi, ebx
	sub esi, ebx
	sub esi, BUF_OFLOW_CHARS
	add ebx, edx

.last_char_text:
	pop edx
	test dh, dh
	jz .done
	mov ch, ah
	set_graph_reg 8, dh		; Set last character pixel bitmask

.last_char_text_loop:
	inc ebp
	mov al, [ebp]
	mov al, [esi]
	mov [edi], al			; Draw last partial char
	sub esi, ebx
	sub edi, ebx			; Bottom to top!
	sub esi, BUF_OFLOW_CHARS
	dec ch
	jnz .last_char_text_loop

	jmp .done

.blit_single_char:

	mov dh, [fill_startmask + edx]
	and dh, [fill_endmask + esi]
	set_graph_reg 8, dh		; Set character pixel bitmask
	mov ch, [ebp + 2]		; CH: font height
	mov edx, [gui_plane_chars]	; EDX: number of characters per row
	mov ebp, [gui_offscr_addr]	; EBP: offscreen address in video RAM

	mov ah, ch

.single_char_bg_loop:
	mov al, [edi]
	mov [ebp], al			; Draw background
	add edi, edx
	inc ebp
	dec ah
	jnz .single_char_bg_loop

	pop esi
	push edx
	set_graph_reg 0, 0		; Set/Reset register: all zero
	set_graph_reg 1, bl		; Enable Set/Reset on color mask
	set_graph_reg 3, 11000b		; XOR, no rotate
	pop edx

	movzx eax, ch
	mov eax, [gui_buf_rowtab + eax * 4]
	add esi, eax

.single_char_text_loop:
	mov al, [esi]
	dec ebp
	sub esi, edx
	mov bl, [ebp]
	sub edi, edx
	sub esi, BUF_OFLOW_CHARS
	mov [edi], al
	dec ch
	jnz .single_char_text_loop

	jmp .done

.done:
	pop eax

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

.skip_first_char_bg:
	movzx eax, ch
	add edi, [gui_rowtab + eax * 4]
	inc cl
	jmp .mid_char_bg

.skip_last_char_bg:
	movzx eax, ch
	sub edi, [gui_rowtab + eax * 4]
	jmp .first_char_text

.skip_first_char_text:
	set_graph_reg 0, 0		; Set/Reset register: all zero
	set_graph_reg 1, bl		; Enable Set/Reset on color mask
	set_graph_reg 3, 11000b		; XOR, no rotate
	mov ebp, [gui_offscr_addr]	; EBP: offscreen address in video RAM
	mov ebx, [gui_plane_chars]	; EBX: number of characters per row
	jmp .mid_char_text

.skip_mid_char_text:
	cmp dl, 0xff
	je .last_char_text
	movzx eax, ch
	dec eax
	add edi, [gui_rowtab + eax * 4]
	mov eax, [gui_buf_rowtab + eax * 4]
	add esi, eax
	mov ah, ch
	jmp .last_char_text

.error_font:
	mov eax, GUI_ERR_FONT
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;==============================================================================
; Data area
;==============================================================================

section .data

		; Table for partial pixel fills at start and end of fill area.
		; First entry is zero since this is an indicator only to skip
		; partial fill operations.

fill_startmask	db 1111_1111b, 0111_1111b, 0011_1111b, 0001_1111b,
		db 0000_1111b, 0000_0111b, 0000_0011b, 0000_0001b
fill_endmask	db 0000_0000b, 1000_0000b, 1100_0000b, 1110_0000b,
		db 1111_0000b, 1111_1000b, 1111_1100b, 1111_1110b,
		db 1111_1111b 		; For single-character fills
