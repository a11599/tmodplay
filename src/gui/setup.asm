;==============================================================================
; MOD player GUI - Setup and initialization
;------------------------------------------------------------------------------
; Supports VGA 640x480 only, but can be extended relatively easily to other
; 16-color planar EGA/VGA modes.
;==============================================================================

cpu 386

%include "gui/consts/public.inc"
%include "gui/consts/global.inc"
%include "system/api/memory.inc"
%include "debug/log.inc"


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
	push ebx
	push eax

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

	; Allocate offscreen render buffer

	mov al, SYS_MEM_HI_LO
	mov ebx, cs:[gui_plane_chars]
	add ebx, BUF_OFLOW_CHARS	; Padding for overflow rendering
	imul ebx, MAX_FONT_HEIGHT
	call far sys_mem_alloc
	jc .error
	mov cs:[gui_buf_addr], eax
	log {'Allocated {u} bytes for offscreen render buffer @{X32}', 13, 10}, ebx, eax

	clc
	pop eax

.exit:
	pop ebx
	retf

.error:
	call far gui_shutdown		; Restore video state
	add sp, 4			; Discard EAX from stack
	stc
	jmp .exit

.no_vga:
	mov byte cs:[startup_mode], 0xff
	add sp, 4			; Discard EAX from stack
	mov eax, GUI_ERR_NOVGA
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
	push eax

	; Release offscreen render buffer

	mov eax, cs:[gui_buf_addr]
	test eax, eax
	jz .restore_vmode
	call far sys_mem_free
	mov dword cs:[gui_buf_addr], 0

.restore_vmode:

	; Reset video mode

	mov al, cs:[startup_mode]
	cmp al, 0xff
	je .done
	xor ah, ah
	int 0x10
	mov byte cs:[startup_mode], 0xff

.done:
	clc
	pop eax
	retf


;==============================================================================
; Data area
;==============================================================================

startup_mode	db 0xff			; Video mode before GUI setup

		alignb 2

		global gui_buf_rowtab
gui_buf_rowtab:
		%assign row 0
		%rep MAX_FONT_HEIGHT
		dw row * (80 + BUF_OFLOW_CHARS)
		%assign row row + 1
		%endrep

		global gui_rowtab
gui_rowtab:
		%assign row 0
		%rep 480
		dw row * 80
		%assign row row + 1
		%endrep

		alignb 4

		global gui_buf_addr
gui_buf_addr	dd 0			; Render buffer linear address
		global gui_scr_width
gui_scr_width	dd 640			; Horizontal screen resolution
		global gui_scr_height
gui_scr_height	dd 480			; Vertical screen resolution
		global gui_plane_chars
gui_plane_chars	dd 80			; Horizontal plane width in bytes
		global gui_offscr_addr
gui_offscr_addr	dd 80 * 480		; First offscreen byte in video memory
