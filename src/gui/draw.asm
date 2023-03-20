;==============================================================================
; MOD player GUI - Drawing routines
;------------------------------------------------------------------------------
; Supports EGA/VGA 16-color planar video modes as long as they fit into the
; 0xA000 segment.
;==============================================================================

cpu 386

%include "gui/api/setup.inc"
%include "gui/consts/global.inc"
%include "gui/consts/public.inc"
%include "system/api/memory.inc"
%include "debug/log.inc"

segment gui public use16 class=CODE align=16
segment gui


;------------------------------------------------------------------------------
; Fill middle (full pixels) of box - used by gui_draw_box
;------------------------------------------------------------------------------
; -> ES:DI - Video memory offset for top-left pixel character
;    BX - Box height
;    DX - Increment to advance to beginning of next row
;    SI - Number of full words to fill in the middle of the box
;    %1 - Number of characters to write to video memory before word fill
;    %2 - 1 = Write one word, 2 = write SI number of words to video memory
;    %3 - Number of characters to write to video memory after word fill
; <- AX, CX, DI (if %2 = 1) - Destroyed
;------------------------------------------------------------------------------

%macro	fill_mid 3

	mov ax, bx			; AX: row counter

%%fill_mid_loop:
	sub ax, FILL_UNROLL		; Fill rows in bulk when possible
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
	mov cx, si
	rep stosw
	%endif

	; Character-fill end

	%rep %3
	stosb
	%endrep

	add di, dx

	%endrep

	jmp %%fill_mid_loop

%%fill_mid_rest:
	add ax, FILL_UNROLL
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
	mov cx, si
	rep stosw
	%endif

	; Character-fill end

	%rep %3
	stosb
	%endrep

	add di, dx
	dec ax
	jnz %%fill_mid_rest_loop
	jmp draw_box_fill_mid_done

%endmacro


;------------------------------------------------------------------------------
; Draw a filled box to the screen.
;------------------------------------------------------------------------------
; -> BX - Height of box in pixels (signed)
;    CX - Width of box in pixels (signed)
;    DL - Color of the box (0 - 15)
;    SI - X coordinate of top left point in pixels (signed)
;    DI - Y coordinate of top left point in pixels (signed)
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
	push bx
	push cx
	push edx
	push esi
	push di
	push bp
	push es

	;----------------------------------------------------------------------
	; Box boundary checks

	cmp bx, 0
	je draw_box_done		; No height, done
	jg .check_width
	add di, bx			; Move top by negative height
	neg bx				; Turn height into positive

.check_width:
	cmp cx, 0
	je draw_box_done		; No width, done
	jg .check_top
	add si, cx			; Move left by negative width
	neg cx				; Turn width into positive

.check_top:
	cmp di, 0
	jge .check_left
	add bx, di			; Decrease height by negative top
	jle draw_box_done		; Still negative or 0, done
	xor di, di			; Top is now at 0

.check_left:
	cmp si, 0
	jge .check_bottom
	add cx, si			; Decrease width by negative left
	jle draw_box_done		; Still negative or 0, done
	xor si, si			; Left is now at 0

.check_bottom:
	mov ax, cs:[gui_scr_height]
	cmp di, ax
	jge draw_box_done		; Beyond bottom, done
	sub ax, di
	cmp bx, ax
	jle .check_right
	mov bx, ax			; Clamp height to stay within bottom

.check_right:
	mov ax, cs:[gui_scr_width]
	cmp si, ax
	jge draw_box_done		; Beyond right, done
	sub ax, si
	cmp cx, ax
	jle .draw_box
	mov cx, ax			; Clamp width to stay within right

.draw_box:
	mov ax, 0xa000
	mov es, ax			; ES: video RAM segment

	set_graph_reg 0, dl		; Set/Reset register: fill color
	set_graph_reg 1, 0x0f		; Enable Set/Reset on all bitplanes
	set_graph_reg 3, 0		; Ignore latched data, don't rotate
	set_sequencer_reg 2, 0x0f	; Enable writes to all planes

	xor eax, eax
	mov ax, di
	mov di, cs:[gui_rowtab + eax * 2]
	mov dx, si			; DX: left start coordinate
	shr dx, 3
	add di, dx			; DI: start address in video RAM

	;----------------------------------------------------------------------
	; Calculate pixels to fill

	movzx edx, si
	and dx, 0x0007			; EDX: first pixel from start address
	movzx esi, cx
	add si, dx			; ESI: last pixel from start address
	cmp si, 8
	jbe draw_box_fill_single	; Fill affects one character only

	mov al, cs:[fill_startmask + edx]
	and si, 0x0007			; AL: start character pixel fill mask
	mov ah, cs:[fill_endmask + esi]	; AH: end character pixel fill mask

	mov si, 8
	sub si, dx			; SI: number of pixels in start char
	sub cx, si			; CX: width - pixels in start character
	shr cx, 3			; CX: number of full characters to fill
	mov dx, ax			; DX: pixel fill masks

	; ES:DI: video memory offset for top-left pixel character
	; BX: box height
	; CX: number of characters to fill in the middle of the box
	; DL: start character pixel fill mask, 0 if no partial start char fill
	; DH: end character pixel fill mask, 0 if no partial end char fill
	; BP: video memory offset backup

	mov bp, di			; BP: video memory offset backup
	mov si, cs:[gui_plane_chars]
	cmp dl, 0xff
	jne .fill_start
	inc cx				; First is also a full character
	jmp .fill_mid

.fill_start:

	;----------------------------------------------------------------------
	; Fill start character of box vertically
	; ES:DI: video memory offset for top-left pixel character
	; BX: box height
	; DL: start character pixel fill mask
	;     (destroyed by this fill routine)

	push dx
	set_graph_reg 8, dl		; Set start character pixel bitmask
	pop dx
	mov ax, bx			; AX: row counter

.fill_start_loop:
	sub ax, FILL_UNROLL		; Fill rows in bulk when possible
	js .fill_start_rest
	%rep FILL_UNROLL
	or byte es:[di], al		; Set pixels, value doesn't matter
	add di, si			; Next row
	%endrep
	jmp .fill_start_loop

	align 4

.fill_start_rest:
	add ax, FILL_UNROLL		; Fill rest of rows
	jz .fill_start_done

.fill_start_rest_loop:
	or byte es:[di], al		; Set pixels, value doesn't matter
	add di, si			; Next row
	dec ax
	jnz .fill_start_rest_loop

.fill_start_done:
	inc bp				; Go to next character in video memory
	mov di, bp

.fill_mid:
	add bp, cx			; Set to char after mid area in vmem

	;----------------------------------------------------------------------
	; Fill middle of box
	; ES:DI: video memory offset for top-left full pixel character
	; BX: box height
	; CX: number of characters to fill in the middle of the box
	;     (destroyed by this fill routine)
	; DH: end character pixel fill mask, 0 if no partial end character fill

	push dx
	set_graph_reg 8, 0xff		; Set all pixels in character
	pop dx

	cmp cx, 1
	jb fill_end			; No middle part, fill last one
	push dx				; Save pixel fill mask
	mov dx, si
	je .fill_mid_single		; Fill only a single character

	sub dx, cx			; DX: pointer adjustment for next row
	mov si, cx
	shr si, 1			; SI: number of words to fill

	xor eax, eax			; Calculate bits for optimal fillroutine
	and cl, 1
	add cl, cl
	test di, 1
	setnz al			; EAX bit 0: 1 if odd-aligned
	or al, cl			; EAX bit 1: 1 if odd number of bytes
	cmp si, 2
	setae cl
	shl cl, 2
	add al, cl			; EAX bit 2: 1 if word count >= 2
	jmp [cs:.fill_mids + eax * 2]

	align 4

.fill_mid_single:
	sub dx, cx			; DX: pointer adjustment for next row
	fill_mid 1, 0, 0		; Fill single character (8 pixels)

	align 4

.fill_mids:
	dw .fill_mid_1w_even_even	; 000 - 1-word, even-aligned, even count
	dw .fill_mid_1w_odd_even	; 001 - 1-word, odd-aligned, even count
	dw .fill_mid_1w_even_odd	; 010 - 1-word, even-aligned, odd count
	dw .fill_mid_1w_odd_odd		; 011 - 1-word, odd-aligned, odd count
	dw .fill_mid_nw_even_even	; 100 - N-word, even-aligned, even count
	dw .fill_mid_nw_odd_even	; 101 - N-word, odd-aligned, even count
	dw .fill_mid_nw_even_odd	; 110 - N-word, even-aligned, odd count
	dw .fill_mid_nw_odd_odd		; 111 - N-word, odd-aligned, odd count

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
	dec si				; One less word due to byte paddings
	fill_mid 1, 2, 1		; Fill: byte, word+, byte

	align 4

.fill_mid_nw_even_odd:			; N-word, even-aligned, odd count
	fill_mid 0, 2, 1		; Fill: word+, byte

	align 4

.fill_mid_nw_odd_odd:			; N-word, odd-aligned, odd count
	fill_mid 1, 2, 0		; Fill: byte, word+

	align 4

draw_box_fill_mid_done:
	pop dx				; Restore pixel fill mask

	test dh, dh
	jz draw_box_done		; No partial end fill
	mov di, bp			; Go to last pixel mask character

fill_end:

	;----------------------------------------------------------------------
	; Fill last (or only) column of box vertically
	; ES:DI: video memory offset for top-right pixel character
	; BX: box height
	; DH: end character pixel fill mask

	set_graph_reg 8, dh		; Set last pixel bitmask

	mov ax, bx
	mov si, cs:[gui_plane_chars]

.fill_end_loop:
	sub ax, FILL_UNROLL		; Fill rows in bulk when possible
	js .fill_end_rest
	%rep FILL_UNROLL
	or byte es:[di], al		; Set pixels, value doesn't matter
	add di, si			; Next row
	%endrep
	jmp .fill_end_loop

	align 4

.fill_end_rest:
	add ax, FILL_UNROLL		; Fill rest of rows
	jz draw_box_done

.fill_end_rest_loop:
	or byte es:[di], al		; Set pixels, value doesn't matter
	add di, si			; Next row
	dec ax
	jnz .fill_end_rest_loop

draw_box_done:
	pop es
	pop bp
	pop di
	pop esi
	pop edx
	pop cx
	pop bx
	pop eax
	retf

	align 4

draw_box_fill_single:
	mov dh, cs:[fill_startmask + edx]
	and dh, cs:[fill_endmask + esi]
	jmp fill_end


;------------------------------------------------------------------------------
; Blit an interleave of the bitmap to video RAM - used by gui_blit_bitmap
;------------------------------------------------------------------------------
; -> DS:SI - Interleaved bitmap data
;    ES:DI - Video memory offset for top-left bitmap character
;    BL - Bitmap height in pixels
;    CL - Bitmap width in 64 pixel units (8 characters/bytes)
;    DX - Increment to advance to beginning of next row
;    %1 - Interleave index to blit (0 or 1)
; <- AX, BX, CH, SI, DI - Destroyed
;------------------------------------------------------------------------------

%macro	blit_bitmap_ilv 1

	%assign plane %1

	mov bl, cl			; CH: bitmap width in 64 pixel units
	dec di				; Prepare for pre-incrementing

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
	mov es:[di], al			; AL: character data (interleave 0)
	mov byte [si - 1], 0		; Clear character in interleave 1
	%else
	mov es:[di], ah			; AH: character data (interleave 1)
	mov byte [si - 2], 0		; Clear character in interleave 0
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

	mov ax, [si]			; Get characters from both interleaves
	inc di				; Pre-increment video memory address
	add si, 2			; Go to next character in interleave
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
	add di, dx			; Adjust video RAM address to next row
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
; -> DS:AX - Interleaved bitmap data
;    CL - Bitmap width in 64 pixel units (8 characters/bytes)
;    CH - Bitmap height in pixels
;    DL - Target bitplane mask (0 - 15, color), won't change other planes
;    DH - Target bitmap image data interleave (0 or 1) to blit
;    SI - X coordinate of top left point in pixels (signed, character aligned)
;    DI - Y coordinate of top left point in pixels (signed)
; <- DS:AX - End of interleaved bitmap data
;------------------------------------------------------------------------------
; Bitmap data is a monochromatic (8 pixel per byte/character) image where the
; previous and current character values are interleaved. The bitmap must fit to
; the screen dimensions, no clipping is done!
;------------------------------------------------------------------------------

	align 4

global gui_blit_bitmap_interleave
gui_blit_bitmap_interleave:
	push bx
	push cx
	push dx
	push si
	push di
	push bp
	push es

	cmp di, cs:[gui_scr_height]	; Safeguard against out of bound row
	jae .done

	mov bp, ax			; DS:BP: Interleaved bitmap
	mov ax, 0xa000
	mov es, ax			; ES: video RAM segment

	mov bx, di
	mov di, cs:[gui_rowtab + di + bx]
	mov bx, dx			; BL: color, BH: target interleave
	shr si, 3
	mov al, bh			; AL: target interleave
	add di, si			; ES:DI: start address in video RAM

	set_sequencer_reg 2, bl		; Write to specific planes (set color)
	set_graph_reg 1, 0		; Disable set/reset
	set_graph_reg 3, 0		; Ignore latched data, don't rotate
	set_graph_reg 8, 0xff		; Use all 8 bits of data written

	movzx si, cl
	mov dx, word cs:[gui_plane_chars]
	shl si, 3
	sub dx, si			; DX: number of characters to next row

	mov si, bp			; DS:SI: interleaved bitmap data
	mov bl, ch			; BL: bitmap height
	test bh, bh
	jnz .interleave_1

	blit_bitmap_ilv 0		; Jumps to .done when ready

	align 4

.interleave_1:
	blit_bitmap_ilv 1

.done:
	mov ax, si			; Advance bitmap offset
	pop es
	pop bp
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	retf


;------------------------------------------------------------------------------
; Returns the width of a text when rendered to the screen.
;------------------------------------------------------------------------------
; -> DS:EBX - Pointer to ASCIIZ string with text to render
; <- CX - Width of text in pixels
;------------------------------------------------------------------------------

	align 4

global gui_text_width
gui_text_width:
	push ax
	push bx
	push si
	push edx
	push edi

	mov al, ds:[ebp]		; AL: first character code in font
	mov ah, ds:[ebp + 1]		; AH: last character code in font
	xor cx, cx			; CX: text width in pixels
	mov edi, ebx			; DS:EDI: text pointer

	; Get width of first character

	xor edx, edx
	mov dl, [edi]			; DL: character
	test dl, dl
	jz .done
	movzx si, byte ds:[ebp + 4]	; SI: letter spacing
	cmp dl, al			; Check glyph range
	jb .no_glyph_first
	cmp dl, ah
	ja .no_glyph_first
	sub dl, al
	mov dx, ds:[ebp + edx * 2 + 8]	; EDX: offset of glyph definition
	test dx, dx
	jz .no_glyph_first
	mov cl, ds:[ebp + edx + 1]	; CX: width of first character
	jmp .char_width_loop

.no_glyph_first:
	mov cl, ds:[ebp + 3]		; CX: space width

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
	mov bx, si			; BX: letter spacing
	mov dx, ds:[ebp + edx * 2 + 8]	; EDX: offset of glyph definition
	test dx, dx
	jz .no_glyph
	add bl, ds:[ebp + edx + 1]	; BX: letter spacing + character width
	add cx, bx
	jmp .char_width_loop

.no_glyph:
	add bl, ds:[ebp + 3]		; BX: letter spacing + space width
	add cx, bx
	jmp .char_width_loop

.done:
	pop edi
	pop edx
	pop si
	pop bx
	pop ax
	retf


;------------------------------------------------------------------------------
; Render text to the screen.
;------------------------------------------------------------------------------
; -> AL - Text alignment (GUI_AL_*)
;    DS:EBX - Pointer to ASCIIZ string with text to render
;    CX - Width of container for rendered text
;    DL - Color of the text (0 - 15)
;    DH - Background color (0 - 15)
;    SI - X coordinate of render origin in pixels (signed)
;    DI - Y coordinate of top of render origin in pixels (signed)
;    DS:EBP - Pointer to font definition
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
	push es
	push eax

	cmp byte ds:[ebp + 2], MAX_FONT_HEIGHT
	jae .error_font

	cmp cx, 0
	jle .done			; Maximum width reached

	push cx
	push dx
	push si
	push di

	;----------------------------------------------------------------------
	; Clear one line of text in render buffer

	push eax
	push ecx
	push edi

	movzx ecx, byte ds:[ebp + 2]
	mov cx, cs:[gui_buf_rowtab + ecx * 2]
	xor eax, eax
	mov edi, cs:[gui_buf_addr]
	shr cx, 2
	mov es, ax
	a32 rep stosd

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

	mov ax, cx			; AX: container width
	push cs
	call gui_text_width
	sub ax, cx			; AX: container width - text width
	shr ax, 1			; AX: (container width - text width) / 2
	add si, ax			; SI: X + (container - text width) / 2
	jmp .render_text

.align_right:

	; Align right

	mov ax, cx			; AX: container width
	push cs
	call gui_text_width
	add si, ax			; SI: X + container width
	sub si, cx			; SI: X + container width - text width

.render_text:

	; Determine start X coordinate

	movzx esi, si
	mov ax, cx			; AX: maximum text width in pixels
	mov cx, si
	and cl, 0x07			; CL: bit shift
	shr si, 3
	add esi, cs:[gui_buf_addr]
	segment_address ds, edx
	sub esi, edx			; DS:ESI: render buffer address
	add esi, BUF_OFLOW_CHARS / 2	; Add padding
	mov edi, cs:[gui_plane_chars]
	add di, BUF_OFLOW_CHARS		; EDI: bytes to add for next row

	push esi

.render_text_loop:

	; Register usage
	; AX: maximum text width
	; DS:EBX: ASCIIZ string current character pointer
	; CL: glyph bitmap rotation
	; CH: glyph height
	; DS:EDX: glyph definition
	; DS:ESI: render buffer pointer
	; EDI: bytes to add for next row in render buffer

	xor edx, edx
	mov dl, [ebx]
	test dl, dl			; DL: character to render
	jz .blit_text			; End of string

	sub dl, ds:[ebp]		; Check font character code boundaries
	jc .no_glyph
	cmp dl, ds:[ebp + 1]
	ja .no_glyph

	movzx edx, word ds:[ebp + edx * 2 + 8]
	test dx, dx
	jz .no_glyph			; No glyph, render space
	add edx, ebp			; DS:EDX: glyph definition
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
	; DS:EDX: glyph definition
	; DS:ESI: render buffer pointer
	; EDI: bytes to add for next row in render buffer
	; DS:EBP: font definition

	xor eax, eax
	mov al, [edx + 3]		; AL: rows to skip
	mov ax, cs:[gui_buf_rowtab + eax * 2]
	add esi, eax			; ESI: adjust for skipped rows

	xor ebx, ebx
	mov bl, [edx]			; BL: bytes per row
	sub edi, ebx			; EDI: bytes to add for next row - width
	add edx, 4			; DS:EDX: glyph bitmap
	mov bh, bl			; BH: bytes per row

.render_glyph_row_loop:
	xor ax, ax
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
	add ch, ds:[ebp + 4]		; Character spacing
	movzx edx, ch
	and ch, 0x07			; CH: glyph width % 8
	sub ax, dx			; AX: maximum width left
	jle .blit_text			; Maximum width reached
	shr dl, 3			; EDX: glyph byte count
	add cl, ch
	inc ebx				; EBX: next character in string
	shl cl, 5			; Bit shift overflow (3rd bit) to CF
	adc esi, edx			; ESI: adjust render buffer pointer
	shr cl, 5			; CL: adjusted bit shift
	jmp .render_text_loop

.no_glyph:
	mov ch, ds:[ebp + 3]		; CH: space width
	jmp .next_char

.blit_text:
	pop eax				; EAX: render buffer start address
	pop di
	pop si

	; Calculate render buffer start offset for blitting

	movzx eax, si
	shr ax, 3
	add eax, cs:[gui_buf_addr]
	segment_address ds, edx		; EDX: segment address of DS
	sub eax, edx			; DS:ESI: render buffer address
	add eax, BUF_OFLOW_CHARS / 2	; Add padding

	pop bx				; BL: text color, BH: background color
	pop cx
	push eax			; Render buffer start address

	;----------------------------------------------------------------------
	; Copy text from render buffer to video memory

	set_graph_reg 0, bh		; Set/Reset register: background color
	set_graph_reg 1, 0x0f		; Enable Set/Reset on all bitplanes
	set_graph_reg 3, 0		; Ignore latched data, don't rotate
	set_sequencer_reg 2, 0x0f	; Enable writes to all planes

	mov ax, 0xa000
	mov es, ax

	movzx edi, di
	mov di, word cs:[gui_rowtab + edi * 2]
	mov dx, si			; DX: left start coordinate
	shr dx, 3
	add di, dx			; ES:EDI: start address in video RAM
	xor bl, bh
	xor bl, 0x0f			; BL: text XNOR background color

	; Calculate pixels to fill

	movzx edx, si
	and dx, 0x0007			; EDX: first pixel from start address
	movzx esi, cx
	add si, dx			; ESI: last pixel from start address
	cmp si, 8
	jbe .blit_single_char

	mov al, cs:[fill_startmask + edx]
	and si, 0x0007			; AL: start character pixel fill mask
	mov ah, cs:[fill_endmask + esi]	; AH: end character pixel fill mask

	;----------------------------------------------------------------------
	; Create pixels for latch data in offscreen video memory
	;----------------------------------------------------------------------
	; Background color is first set in offscreen video memory for the first,
	; middle and last characters. The color is set to the Set/Reset register
	; and the mask register is used to overwrite pixels in the character.

	; ES:DI: video memory offset for top-left pixel character
	; BL: text color mask (text color XNOR background color)
	; CL: number of characters to fill in the middle of the box
	; CH: font height
	; DL: start character pixel fill mask, 0 if no partial start char fill
	; DH: end character pixel fill mask, 0 if no partial end char fill
	; ES:BP: video memory offscreen address

	mov si, 8
	sub si, dx			; SI: number of pixels in start char
	sub cx, si			; CX: width - pixels in start character
	shr cx, 3			; CL: number of full characters to fill
	mov dx, ax			; DX: pixel fill masks
	mov ch, ds:[ebp + 2]		; CH: font height
	mov bp, cs:[gui_offscr_addr]	; ES:BP: offscreen address in video RAM
	mov si, cs:[gui_plane_chars]	; SI: number of characters per row

	; Draw background characters in offscreen video memory to be used as
	; latch data for text blitting

	push dx
	cmp dl, 0xff
	je .skip_first_char_bg		; No partial first character, skip
	set_graph_reg 8, dl		; Set start character pixel bitmask
	mov ah, ch

.first_char_bg_loop:
	mov al, es:[di]
	mov es:[bp], al			; Draw background for first partial char
	add di, si
	inc bp
	dec ah
	jnz .first_char_bg_loop

.mid_char_bg:
	test cl, cl
	jz .last_char_bg
	set_graph_reg 8, 0xff		; Set middle character pixel bitmask
	mov byte es:[bp], 0xff		; Draw background for middle characters

.last_char_bg:
	pop dx
	test dh, dh
	jz .skip_last_char_bg
	push dx
	set_graph_reg 8, dh		; Set start character pixel bitmask

	xor ax, ax
	mov al, cl
	add di, ax
	mov ah, ch

.last_char_bg_loop:
	sub di, si			; Bottom to top!
	inc bp
	mov al, es:[di]
	mov es:[bp], al			; Draw background for last partial char
	dec ah
	jnz .last_char_bg_loop

	xor ax, ax
	mov al, cl
	sub di, ax			; ES:DI: video memory target start addr.

	pop dx

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

	pop esi				; DS:ESI: render buffer start address

	push dx
	cmp dl, 0xff
	je .skip_first_char_text
	set_graph_reg 8, dl		; Set start character pixel bitmask
	set_graph_reg 0, 0		; Set/Reset register: all zero
	set_graph_reg 1, bl		; Enable Set/Reset on color mask
	set_graph_reg 3, 11000b		; XOR, no rotate

	mov bp, cs:[gui_offscr_addr]	; ES:BP: offscreen address in video RAM
	mov ebx, cs:[gui_plane_chars]	; SI: number of characters per row

	push esi
	push di
	mov ah, ch

.first_char_text_loop:
	mov al, es:[bp]			; Fill latch
	mov al, [esi]
	mov es:[di], al			; Draw first partial character
	add di, bx
	add esi, ebx
	inc bp
	add esi, BUF_OFLOW_CHARS
	dec ah
	jnz .first_char_text_loop

	pop di
	pop esi
	inc di
	inc esi

.mid_char_text:
	test cl, cl
	jz .skip_mid_char_text
	set_graph_reg 8, 0xff		; Set mid character pixel bitmask
	mov al, es:[bp]			; Fill latch
	mov ah, ch			; AH: font height
	mov al, ch
	movzx ecx, cl
	sub ebx, ecx
	mov edx, ecx

.mid_char_text_loop:
	a32 rep movsb			; Draw middle full characters
	add di, bx
	add esi, ebx
	mov cx, dx
	add esi, BUF_OFLOW_CHARS
	dec al
	jnz .mid_char_text_loop

	sub di, bx
	sub esi, ebx
	sub esi, BUF_OFLOW_CHARS
	add ebx, edx

.last_char_text:
	pop dx
	test dh, dh
	jz .done
	mov ch, ah
	set_graph_reg 8, dh		; Set last character pixel bitmask

.last_char_text_loop:
	inc bp
	mov al, es:[bp]
	mov al, [esi]
	mov es:[di], al			; Draw last partial char
	sub esi, ebx
	sub di, bx			; Bottom to top!
	sub esi, BUF_OFLOW_CHARS
	dec ch
	jnz .last_char_text_loop

	jmp .done

.blit_single_char:

	mov dh, cs:[fill_startmask + edx]
	and dh, cs:[fill_endmask + esi]
	set_graph_reg 8, dh		; Set character pixel bitmask
	mov ch, ds:[ebp + 2]		; CH: font height
	mov edx, cs:[gui_plane_chars]	; EDX: number of characters per row
	mov bp, cs:[gui_offscr_addr]	; ES:BP: offscreen address in video RAM

	mov ah, ch

.single_char_bg_loop:
	mov al, es:[di]
	mov es:[bp], al			; Draw background
	add di, dx
	inc bp
	dec ah
	jnz .single_char_bg_loop

	pop esi
	push dx
	set_graph_reg 0, 0		; Set/Reset register: all zero
	set_graph_reg 1, bl		; Enable Set/Reset on color mask
	set_graph_reg 3, 11000b		; XOR, no rotate
	pop dx

	movzx eax, ch
	movzx eax, word cs:[gui_buf_rowtab + eax * 2]
	add esi, eax

.single_char_text_loop:
	mov al, [esi]
	dec bp
	sub esi, edx
	mov bl, es:[bp]
	sub di, dx
	sub esi, BUF_OFLOW_CHARS
	mov es:[di], al
	dec ch
	jnz .single_char_text_loop

	jmp .done

.done:
	pop eax

.exit:
	pop es
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	retf

.skip_first_char_bg:
	movzx eax, ch
	add di, cs:[gui_rowtab + eax * 2]
	inc cl
	jmp .mid_char_bg

.skip_last_char_bg:
	movzx eax, ch
	sub di, cs:[gui_rowtab + eax * 2]
	jmp .first_char_text

.skip_first_char_text:
	set_graph_reg 0, 0		; Set/Reset register: all zero
	set_graph_reg 1, bl		; Enable Set/Reset on color mask
	set_graph_reg 3, 11000b		; XOR, no rotate
	mov bp, cs:[gui_offscr_addr]	; ES:BP: offscreen address in video RAM
	mov ebx, cs:[gui_plane_chars]	; SI: number of characters per row
	jmp .mid_char_text

.skip_mid_char_text:
	cmp dl, 0xff
	je .last_char_text
	movzx eax, ch
	dec eax
	add di, cs:[gui_rowtab + eax * 2]
	movzx eax, word cs:[gui_buf_rowtab + eax * 2]
	add esi, eax
	mov ah, ch
	jmp .last_char_text

.error_font:
	mov eax, GUI_ERR_FONT
	add sp, 4			; Discard EAX from stack
	stc
	jmp .exit


;==============================================================================
; Data area
;==============================================================================

		; Table for partial pixel fills at start and end of fill area.
		; First entry is zero since this is an indicator only to skip
		; partial fill operations.

fill_startmask	db 1111_1111b, 0111_1111b, 0011_1111b, 0001_1111b,
		db 0000_1111b, 0000_0111b, 0000_0011b, 0000_0001b
fill_endmask	db 0000_0000b, 1000_0000b, 1100_0000b, 1110_0000b,
		db 1111_0000b, 1111_1000b, 1111_1100b, 1111_1110b,
		db 1111_1111b 		; For single-character fills
