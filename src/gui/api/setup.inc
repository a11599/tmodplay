;==============================================================================
; MOD player GUI - Setup and initialization
;------------------------------------------------------------------------------
; API
;==============================================================================

extern gui_plane_chars
extern gui_scr_width
extern gui_scr_height


;------------------------------------------------------------------------------
; Set an EGA/VGA graphics register
;------------------------------------------------------------------------------
; -> %1 - Graphics register index (can't be AH)
;    %2 - New value
; <- AX, DX - Destroyed
;------------------------------------------------------------------------------

%macro	set_graph_reg 2

	%ifnidni %2, ah			; AH already contains new value
	mov ah, %2			; Use new value
	%endif
	%ifnidni %2, al			; AL already contains index
	mov al, %1
	%endif
	mov dx, 0x3ce
	out dx, ax			; Write new value

%endmacro


;------------------------------------------------------------------------------
; Set a bits of an EGA/VGA graphics register
;------------------------------------------------------------------------------
; -> %1 - Graphics register index
;    %2 - New value (can't be AL, DL or DH)
;    %3 - Mask of bits allowed to change (constant)
; <- AX, DX - Destroyed
;------------------------------------------------------------------------------

%macro	set_graph_reg_bits 3

	%ifnidni %2, al			; AL already contains index
	mov al, %1
	%endif
	mov dx, 0x3ce
	out dx, al			; Set index
	inc dx
	in al, dx			; Read current value
	and al, %3 ^ 0xff
	or al, %2			; Combine with new
	out dx, al			; Write new value

%endmacro


;------------------------------------------------------------------------------
; Set an EGA/VGA sequencer register
;------------------------------------------------------------------------------
; -> %1 - Sequencer register index (can't be AH)
;    %2 - New value
; <- AX, DX - Destroyed
;------------------------------------------------------------------------------

%macro	set_sequencer_reg 2

	%ifnidni %2, ah			; AH already contains new value
	mov ah, %2			; Use new value
	%endif
	%ifnidni %2, al			; AL already contains index
	mov al, %1
	%endif
	mov dx, 0x3c4
	out dx, ax			; Write new value

%endmacro
