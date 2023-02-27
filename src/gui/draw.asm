;==============================================================================
; MOD player GUI - Drawing routines
;------------------------------------------------------------------------------
; Supports EGA/VGA 16-color planar video modes as long as they fit into the
; 0xA000 segment.
;==============================================================================

cpu 386

%include "gui/api/setup.inc"
%include "gui/consts/global.inc"
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
; -> SI - X coordinate of top left point in pixels (signed)
;    DI - Y coordinate of top left point in pixels (signed)
;    CX - Width of box in pixels (signed)
;    BX - Height of box in pixels (signed)
;    DL - Color of the box (0 - 15)
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

	mov ax, di
	mul word cs:[gui_plane_chars]
	mov di, ax
	mov dx, si			; DX: left start coordinate
	shr dx, 3
	add di, dx			; DI: start address in video RAM

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
	je .fill_mid			; No partial start by fill

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
;    %2 - Jump target if blit has finished
; <- AX, BX, CH, SI, DI - Destroyed
;------------------------------------------------------------------------------

%macro	blit_bitmap_ilv 2

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

	jmp %2

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
;    DL - Target bitplane mask (0 - 15, color)
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

blit_bitmap_exit:			; Defined here for blit_bitmap_ilv macro
	mov ax, si			; Advance bitmap offset
	pop es
	pop bp
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	retf

global gui_blit_bitmap
gui_blit_bitmap:
	push bx
	push cx
	push dx
	push si
	push di
	push bp
	push es

	mov bp, ax			; DS:BP: Interleaved bitmap
	mov ax, 0xa000
	mov es, ax			; ES: video RAM segment
	mov bx, dx			; BL: color, BH: target interleave

	mov ax, di
	mul word cs:[gui_plane_chars]
	shr si, 3
	mov di, ax
	mov al, bh			; AL: target interleave
	add di, si			; ES:DI: start address in video RAM

	set_sequencer_reg 2, bl		; Write to specific planes (set color)
	set_graph_reg 0, 0x00		; Set/Reset register: clear the rest
	mov ah, bl			; Using AH saves an instruction in
	not ah				; set_graph_reg
	and ah, 0x0f
	set_graph_reg 1, ah		; Enable Set/Reset on background planes
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

	blit_bitmap_ilv 0, blit_bitmap_exit

	align 4

.interleave_1:
	blit_bitmap_ilv 1, blit_bitmap_exit


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
