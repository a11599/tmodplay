;==============================================================================
; MOD player GUI - Setup and initialization
;------------------------------------------------------------------------------
; Supports VGA 640x480 only, but can be extended relatively easily to other
; 16-color planar EGA/VGA modes.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/string.inc"
%include "rtl/api/log.inc"

%include "gui/config.inc"
%include "gui/consts/public.inc"


;------------------------------------------------------------------------------
; Initialize graphical user interface.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global gui_setup
gui_setup:
	push ebx
	push edx
	push eax

	; Save current video mode for shutdown

	call pmi(get_env_info)
	mov al, [ebx + pmi_env_info.startup_vmode]
	mov [startup_mode], al

	; Set 640x480 16-color mode

	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x10
	mov ax, 0x0012
	call pmi(call_rm)
	mov ah, 0x0f			; Check if mode change succeeded
	call pmi(call_rm)
	cmp al, 0x12
	lea esp, [esp + pmi_rm_call.strucsize]
	jne .no_vga

	; Set linear palette using BIOS

	call pmi(file_get_buf)		; Get I/O buffer address
	jc .error
	mov edx, eax			; EDX: address of I/O buffer
	mov dword [edx], 0x03020100	; Linear EGA -> VGA palette mapping
	mov dword [edx + 4], 0x07060504
	mov dword [edx + 8], 0x0b0a0908
	mov dword [edx + 12], 0x0f0e0d0c
	mov byte [edx + 16], 0x00	; Overscan: black
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x10
	shr eax, 4			; AX: real mode segment of buffer
	mov [esp + pmi_rm_call.es], ax
	and edx, 0xf
	mov eax, 0x1002
	call pmi(call_rm)
	lea esp, [esp + pmi_rm_call.strucsize]

	; Allocate offscreen render buffer

	mov al, PMI_MEM_HI_LO
	mov ecx, [gui_plane_chars]
	add ecx, BUF_OFLOW_CHARS	; Padding for overflow rendering
	imul ecx, MAX_FONT_HEIGHT
	call pmi(mem_alloc)
	jc .error
	mov [gui_buf_addr], eax
	log LOG_DEBUG, {'Allocated {u} bytes for offscreen render buffer at 0x{X}', 13, 10}, ecx, eax

	clc
	pop eax

.exit:
	pop edx
	pop ebx
	ret

.error:
	call gui_shutdown		; Restore video state
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit

.no_vga:
	mov byte [startup_mode], 0xff
	add esp, 4			; Discard EAX from stack
	mov eax, GUI_ERR_NOVGA
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown graphical user interface. Should be called before exiting to DOS.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

global gui_shutdown
gui_shutdown:
	push eax

	; Release offscreen render buffer

	mov eax, [gui_buf_addr]
	test eax, eax
	jz .restore_vmode
	log LOG_DEBUG, {'Disposing offscreen render buffer at 0x{X}', 13, 10}, eax
	call pmi(mem_free)
	mov dword [gui_buf_addr], 0

.restore_vmode:

	; Reset video mode

	mov al, [startup_mode]
	cmp al, 0xff
	je .done
	sub esp, pmi_rm_call.strucsize
	mov byte [esp + pmi_rm_call.flags], PMI_CALL_INT
	mov byte [esp + pmi_rm_call.int], 0x10
	xor ah, ah
	call pmi(call_rm)
	lea esp, [esp + pmi_rm_call.strucsize]
	mov byte [startup_mode], 0xff

.done:
	clc
	pop eax
	ret


;------------------------------------------------------------------------------
; Set entries in the VGA palette.
;------------------------------------------------------------------------------
; -> AL - First color to change
;    ESI - Linear address of palette with 3-byte R/G/B values for each color,
;          using 6-bit values (0 - 63)
;    ECX - Number of colors to change
;------------------------------------------------------------------------------

global gui_set_vga_palette
gui_set_vga_palette:
	push eax
	push ecx
	push esi

	mov edx, 0x3c8
	out dx, al
	inc edx
	lea ecx, [ecx * 2 + ecx]
	rep outsb

	pop esi
	pop ecx
	pop eax
	ret


;==============================================================================
; Data area
;==============================================================================

section .data

		global gui_buf_addr
gui_buf_addr	dd 0			; Render buffer linear address
		global gui_scr_width
gui_scr_width	dd 640			; Horizontal screen resolution
		global gui_scr_height
gui_scr_height	dd 480			; Vertical screen resolution
		global gui_plane_chars
gui_plane_chars	dd 80			; Horizontal plane width in bytes
		global gui_offscr_addr
gui_offscr_addr	dd (80 * 480 + 0xa0000)	; First offscreen byte in video memory

		global gui_buf_rowtab
gui_buf_rowtab:
		%assign row 0
		%rep MAX_FONT_HEIGHT
		dd row * (80 + BUF_OFLOW_CHARS)
		%assign row row + 1
		%endrep

		global gui_rowtab
gui_rowtab:
		%assign row 0
		%rep 480
		dd row * 80
		%assign row row + 1
		%endrep

startup_mode	db 0xff			; Video mode before GUI setup
