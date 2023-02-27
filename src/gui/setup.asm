;==============================================================================
; MOD player GUI - Setup and initialization
;------------------------------------------------------------------------------
; Supports VGA 640x480 only, but can be extended relatively easily to other
; 16-color planar EGA/VGA modes.
;==============================================================================

cpu 386

%include "gui/consts/public.inc"

segment gui public use16 class=CODE align=16
segment gui


;------------------------------------------------------------------------------
; Initialize graphical user interface.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global gui_setup
gui_setup:
	push bx
	push ax

	; Save current video mode for shutdown

	mov ah, 0x0f
	int 0x10
	mov cs:[startup_mode], al

	; Set 640x480 16-color mode

	mov ax, 0x0012
	int 0x10
	mov ah, 0x0f			; Check if mode change succeeded
	int 0x10
	cmp al, 0x12
	jne .no_vga

	clc
	pop ax

.exit:
	pop bx
	retf

.no_vga:
	mov byte cs:[startup_mode], 0xff
	add sp, 2			; Discard AX from stack
	mov eax, VID_ERR_NOVGA
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown graphical user interface. Should be called before exiting to DOS.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

	align 4

global gui_shutdown
gui_shutdown:
	push ax

	; Reset video mode

	mov al, cs:[startup_mode]
	cmp al, 0xff
	je .done
	xor ah, ah
	int 0x10

.done:
	clc
	pop ax
	retf


;==============================================================================
; Data area
;==============================================================================

startup_mode	db 0xff			; Video mode before GUI setup

		alignb 4

		global gui_scr_width
gui_scr_width	dd 640			; Horizontal screen resolution
		global gui_scr_height
gui_scr_height	dd 480			; Vertical screen resolution
		global gui_plane_chars
gui_plane_chars	dd 80			; Horizontal plane width in bytes
