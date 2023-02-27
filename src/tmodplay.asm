;==============================================================================
; Therapy MOD player - quarter century later edition
; A tribute to my teenage self
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "system/api/file.inc"
%include "system/api/env.inc"
%include "system/api/string.inc"
%include "mod/api/player.inc"
%include "gui/api/public.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	set_file_fn(name, lbl) at mod_file_fns. %+ name, dw %+ (lbl), seg %+ (lbl)

; Flags for explicit playback parameters from command line arguments

ARG_IPOL_SET	EQU 0x01		; Interpolation method set by argument
ARG_PAN_SET	EQU 0x02		; Stereo mode set by argument

; Constants for scope rendering

SCOPE_H_SHIFTS	EQU 5			; >=3, width of scope, 2 ^ n characters
SCOPE_WIDTH	EQU (1 << SCOPE_H_SHIFTS) / 8
SCOPE_V_SHIFTS	EQU 1			; >=1, height of scope, 256 >> n pixels
SCOPE_HEIGHT	EQU 256 >> SCOPE_V_SHIFTS
SCOPE_COLOR	EQU 15			; Color of the scope
SCOPE_BMP_SIZE	EQU SCOPE_HEIGHT * SCOPE_WIDTH * 64 * 2 / 8
SCOPE_PADDING	EQU 2			; Scope padding in 16 pixels

segment app private use16 class=CODE align=16
segment app


;------------------------------------------------------------------------------
; Program entry point
;------------------------------------------------------------------------------

	align 4

..start:
	mov ax, app_data
	mov ds, ax			; DS: data segment

	mov ah, 0x40			; Show application header
	mov bx, 1
	mov cx, HEADER_SIZE
	mov dx, header
	int 21h

	; ---------------------------------------------------------------------
	; Check for 386+

	push sp				; Check for 286+
	pop ax
	cmp ax,sp
	jne .cpu_error
	sgdt cs:[.gdtr]			; Check for 386+
	mov al, byte cs:[.gdtr + 5]
	cmp byte cs:[.gdtr + 5], 0
	je .init_mem

.cpu_error:
	mov ah, 0x40			; Show CPU error
	mov bx, 1
	mov cx, ERR_CPU_SIZE
	mov dx, err_cpu
	int 21h
	jmp .terminate

.gdtr	db 6 dup (0)			; Temporary storage for GDTR

.init_mem:

	; ---------------------------------------------------------------------
	; Initialize system

	call far sys_mem_setup		; Memory management - first thing to do
	jnc .init_env
	mov esi, errtab_sys		; Display system error message
	call lookup_message
	call echo
	jmp .terminate

.init_env:
	mov [cmb_size], eax		; Save amount of available conventional
	mov [xmb_size], ebx		; and extended memory
	call far sys_env_setup		; Environment
	call far sys_file_setup		; File management
	jnc .init_log
	mov esi, errtab_sys		; Display system error message
	call lookup_message
	call echo
	jmp .exit_fail_sys_file

.init_log:
	xor eax, eax
	mov ax, ds
	mov es, ax			; ES: data segment
	shl eax, 4
	mov [data_seg_addr], eax

	call far sys_file_get_buf_addr	; Save I/O buffer address rel. to DS
	sub ebx, [data_seg_addr]
	mov [io_buf_addr], ebx

	; ---------------------------------------------------------------------
	; Initialize logging

	%ifdef __DEBUG__		; Initialize logging
	call far sys_env_get_exe_path	; Get path to running executable
	mov ah, '.'
	mov ecx, -1
	call far sys_str_char_rpos	; Find . character
	jc .set_logfile_name
	lea ecx, [eax + 1]		; Copy before . when found

.set_logfile_name:
	mov edi, [io_buf_addr]		; DS:EDI: temp buffer
	call far sys_str_copy		; Copy path before ., then append ".LOG"
	mov dword [edi + ecx - 1], '.LOG'
	mov byte [edi + ecx + 3], 0
	mov esi, edi			; DS:ESI: pointer to logfile name
	mov eax, LOG_FILE | LOG_AUTOCOMMIT
	call far log_setup		; Setup logging
	mov eax, [cmb_size]
	shr eax, 10
	adc eax, 0
	mov ebx, [xmb_size]
	shr ebx, 10
	adc ebx, 0
	log {'Available memory: {u32}k conventional, {u32}k extended', 13, 10}, eax, ebx
	%endif

	; ---------------------------------------------------------------------
	; Initialize player

	mov esi, arg_help		; Display usage if /? argument present
	call far sys_env_get_named_arg
	jnc .usage
	mov ebx, out_params		; Parse arguments
	call parse_args
	jc .exit
	o32 push ds
	push esi			; Save filename

	mov esi, outtab			; Display output device info
	movzx eax, ax
	call lookup_message		; DS:ESI: Output device info string
	mov bp, sp
	push dword [out_params + mod_out_params.port]
	push dword [out_params + mod_out_params.port + 2]
	push dword [out_params + mod_out_params.irq]
	push dword [out_params + mod_out_params.irq + 1]
	push dword [out_params + mod_out_params.dma]
	push dword [out_params + mod_out_params.dma + 1]
	mov edi, [io_buf_addr]
	call far sys_str_format
	mov sp, bp
	mov esi, edi
	call echo

	mov edi, file_fns		; Setup modplayer
	call far mod_setup
	jc .mod_error

	mov esi, msg_samplerate		; Show samplerate info
	mov edi, [io_buf_addr]
	mov bp, sp
	push eax
	call far sys_str_format
	mov sp, bp
	mov esi, edi
	call echo
	mov esi, msg_loading		; Display name of MOD file
	add bp, 8			; BP: pointer above pushed filename
	call far sys_str_format
	mov esi, edi
	call echo

	pop esi				; Restore filename
	o32 pop ds
	call far mod_load		; Load the MOD file
	jc .mod_error

	; Get mod info structure

	mov al, SYS_MEM_HI_LO
	call far mod_get_info
	jc .mod_error
	sub eax, [data_seg_addr]
	mov [mod_info_addr], eax

	movzx ebx, byte [eax + mod_info.num_channels]
	imul bx, mod_channel_info.strucsize
	mov al, SYS_MEM_HI_LO
	call far sys_mem_alloc
	jc .mod_error
	log {'Allocated {u} bytes for channel info buffer @{X32}', 13, 10}, ebx, eax

	sub eax, [data_seg_addr]
	mov [chn_info_addr], eax

	; ---------------------------------------------------------------------
	; Determine settings for auto stereo mode and interpolation

	; Set sample interpolation mode to linear for multichannel and
	; non-standard MOD files

	mov eax, [mod_info_addr]
	mov ecx, [eax + mod_info.flags]	; ECX: mod flags
	test byte [arg_flags], ARG_IPOL_SET
	jnz .auto_stereo_mode
	cmp byte [eax + mod_info.num_channels], 4
	jne .linear_interpolation
	test ecx, MOD_FLG_PAN | MOD_FLG_EXT_OCT
	jz .auto_stereo_mode

.linear_interpolation:
	log {'MOD probably composed on PC (multichannel and/or out of range notes), choosing linear interpolation', 13, 10}
	mov al, MOD_IPOL_LINEAR
	call far mod_set_interpolation

.auto_stereo_mode:

	; Set real stereo mode if pan effects are present

	test byte [arg_flags], ARG_PAN_SET
	jnz .init_player
	test ecx, MOD_FLG_PAN
	jz .init_player

	log {'MOD uses pan command, choosing real stereo mode', 13, 10}
	mov al, MOD_PAN_REAL
	call far mod_set_stereo_mode

.init_player:

	; Allocate memory buffer for oscilloscope bitmap

	mov al, SYS_MEM_LO
	mov ebx, SCOPE_BMP_SIZE * 2
	call far sys_mem_alloc
	jc .mod_error
	shr eax, 4
	mov [scope_bmp_seg], ax
	log {'Allocated {u} bytes for oscilloscope bitmap interleave @{x16}:0000', 13, 10}, ebx, ax

	; ---------------------------------------------------------------------
	; Start playback and run the user interface

	call far gui_setup
	jc .gui_error

	call far mod_play
	log {'Playback started', 13, 10}

	call run_ui

	log {'Esc pressed, exiting', 13, 10}

	; ---------------------------------------------------------------------
	; Stop playback and terminate

	call far mod_stop
	call far mod_unload
	call far mod_shutdown

	call far gui_shutdown

.exit:
	%ifdef __DEBUG__
	call far log_shutdown
	%endif

	call far sys_file_shutdown

.exit_fail_sys_file:
	call far sys_mem_shutdown

.terminate:
	mov ax, 0x4c00
	int 0x21

.usage:
	mov esi, usage			; Display usage
	call echo
	jmp .exit

.gui_error:
	call far gui_shutdown
	mov esi, errtab_gui		; Display GUI error messages
	call lookup_message
	call echo
	jmp .exit

.mod_error:
	mov esi, errtab_mod		; Display modplayer error messages
	call lookup_message
	call echo
	jmp .exit


;------------------------------------------------------------------------------
; Updates the UI and handles keyboard input during MOD playback.
;------------------------------------------------------------------------------
; -> DS - Application data segment
;    ES - Player instance segment
;------------------------------------------------------------------------------

	align 4

run_ui:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

.run_loop:
	mov dx, 0x3da

	; Wait for vertical retrace

.wait_retrace_start:
	in al, dx
	test al, 8
	jz .wait_retrace_start

.wait_retrace_end:
	in al, dx
	test al, 8
	jnz .wait_retrace_end

	; Get player channel and output information

	mov esi, [mod_info_addr]
	call far mod_get_channel_info
	mov esi, output_info
	call far mod_get_output_info

	; Draw scopes to the screen

	call draw_scopes

	; Render scopes to scope bitmap memory

	logframe 0, 0, 24, 24
	call render_scopes

	; Render audio (within same frame in best case)

	logframe 0, 0, 16, 32
	call far mod_render

	; Check pending keystroke

	logframe 0, 0, 0, 0

	mov ah, 0x01
	int 0x16
	jnz .check_key

	jmp .run_loop

	align 4

.check_key:

	; Handle keystrokes

	xor ah, ah
	int 0x16

	log {'Key pressed, scancode: {u8}, ASCII: {u8}', 13, 10}, ah, al

	cmp ah, 1			; Esc: exit
	je .exit
	jmp .run_loop

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Draw scopes to the GUI.
;------------------------------------------------------------------------------
; -> DS - Application data segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

draw_scopes:
	push ds
	push es

	mov bl, [output_info + mod_output_info.buffer_format]
	and bl, MOD_BUF_CHANNEL

	; Set common scope parameters

	mov dh, [scope_blit_ilv]	; DH: blit interleave
	mov ax, [scope_bmp_seg]
	mov ds, ax
	mov ax, seg gui_scr_width
	mov es, ax			; ES: GUI library segment
	xor ax, ax			; DS:AX: scope bitmap
	mov dl, SCOPE_COLOR		; DL: scope color
	mov si, es:[gui_scr_width]
	mov di, 100			; DI: Y coordinate
	shr si, 1			; SI: horizontal center X coordinate

	cmp bl, MOD_BUF_2CHN
	jne .mono

	; Draw two scopes in stereo output mode

	sub si, SCOPE_WIDTH * 64 + SCOPE_PADDING * 8
	mov cx, SCOPE_HEIGHT * 256 + SCOPE_WIDTH
	call far gui_blit_bitmap
	add si, SCOPE_WIDTH * 64 + SCOPE_PADDING * 16
	call far gui_blit_bitmap
	jmp .done

.mono:

	; Draw one larger scope in mono output mode

	sub si, SCOPE_WIDTH * 64
	mov cx, SCOPE_HEIGHT * 256 + SCOPE_WIDTH * 2
	call far gui_blit_bitmap

.done:
	pop es
	pop ds

	; Toggle blit interleave

	xor dh, 1
	mov [scope_blit_ilv], dh

	retn


;------------------------------------------------------------------------------
; Render output device buffer scopes to interleaved bitmap buffer.
;------------------------------------------------------------------------------
; -> DS - Application data segment
;    %1 - Buffer bitdepth (MOD_BUF_DEPTH constants)
;    %2 - Number of buffer channels (MOD_BUF_CHANNEL constants)
;    %3 - Sample range (signed/unsigned, MOD_BUF_RANGE constants)
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

%macro	render_scopes_for 3

	; Size of sample in bytes

	%assign sample_size 1
	%if (%1 = MOD_BUF_16BIT)
	%assign sample_size 2
	%elif (%1 = MOD_BUF_1632BIT)
	%assign sample_size 4
	%endif

	; Number of playback channels

	%assign channels 1
	%if (%2 = MOD_BUF_2CHN)
	%assign channels 2
	%endif

	; Number of bytes to add for next sample

	%assign sample_next sample_size * channels
	%if (%2 = MOD_BUF_2CHNL)
	%assign sample_next sample_size * 2
	%endif

	push es

	; Register usage
	; AL: pixel bit mask
	; AH: scope width in characters
	; BX: left channel sample
	; BP: right channel sample
	; DS:ECX: address of byte following output buffer
	; DS:EDX: start of output buffer
	; DS:ESI: output buffer position
	; ES:DI: scope bitmap top row position

	mov ax, [scope_bmp_seg]
	mov es, ax
	movzx di, byte [scope_blit_ilv]

	mov esi, [output_info + mod_output_info.buffer_addr]
	sub esi, [data_seg_addr]
	mov eax, esi
	mov edx, esi
	mov cs:[.buffer_start], esi
	mov cs:[.buffer_start_2], esi
	add eax, [output_info + mod_output_info.buffer_size]
	add esi, [output_info + mod_output_info.buffer_pos]
	mov ecx, eax
	mov cs:[.buffer_end], eax
	mov cs:[.buffer_end_2], eax

	; But first, let's try to find a crossing point where the waveform
	; crosses the centerline to stabilize the scope a little bit. This
	; simple algorithm is quite effective for "simple" waveforms and good
	; enough for the purpose.

	mov al, 200			; Maximum number of samples to check
	mov ebx, esi

.loop_search_pos:
	cmp esi, ecx			; Reached past output buffer end?
	jae .reset_search_pos		; Yes, reset position

.search_pos:
	cmp word [esi], 0x8000		; Sample above centerline?
	jae .found_pos
	add esi, sample_next
	dec ah				; Nope, check next one
	jnz .loop_search_pos
	mov esi, ebx			; Nothing above centerline, fallback
	jmp .start_render

.found_pos:
	add esi, sample_next		; Skip positive sample

.loop_search_neg:
	cmp esi, ecx			; Reached past output buffer end?
	jae .reset_search_neg		; Yes, reset position

.search_neg:
	cmp word [esi], 0x8000		; Sample below centerline?
	jbe .start_render
	add esi, sample_next
	dec ah				; Nope, check next one
	jnz .loop_search_neg
	mov esi, ebx			; Nothing below centerline, fallback

	; Now render the scopes to the interleaved bitmap buffer
	; AL: pixel bit mask
	; AH: scope width in characters
	; BX: left channel sample
	; BP: right channel sample
	; CX: used to draw vertical line to next left channel sample
	; DX: used to draw vertical line to next right channel sample
	; DS:ESI: output buffer position
	; ES:DI: scope bitmap top row position

.start_render:
	%if (channels = 1)
	mov ax, SCOPE_WIDTH * 8	* 256 * 2 + 0x80
	%else
	mov ax, SCOPE_WIDTH * 8	* 256 + 0x80
	%endif

.loop_render:
	cmp esi, 0x12345678		; Reached past output buffer end?
	.buffer_end EQU $ - 4
	jae .reset_position		; Yes, reset position

.render:
	%if (channels = 1)

	;----------------------------------------------------------------------
	; Read samples for mono output

	%if (%1 = MOD_BUF_8BIT)

	%if (%3 = MOD_BUF_UINT)
	xor ch, ch
	mov bl, [esi]
	%else
	movsx bx, byte [esi]		; BX: mono sample (signed)
	%endif
	lea edx, [esi + sample_next]
	%if (%3 = MOD_BUF_UINT)
	xor bh, bh			; BX: mono sample (unsigned)
	%endif
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	%if (%3 = MOD_BUF_UINT)
	mov cl, [edx]			; CX: next mono sample (unsigned)
	%else
	movsx cx, byte [edx]		; CX: next mono sample (signed)
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	mov bx, [esi]			; BX: mono sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov cx, [edx]			; CX: next mono sample

	%elif (%1 = MOD_BUF_1632BIT)

	mov ebx, [esi]			; EBX: mono sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov ecx, [edx]			; ECX: next mono sample

	%endif				; Bit depth

	%else				; Mono/stereo

	;----------------------------------------------------------------------
	; Read samples for stereo output

	%if (%1 = MOD_BUF_8BIT)

	mov bx, [esi]			; BL: left, BH: right channel sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov cx, [edx]			; CL: next left, CH: next right sample
	%if (%3 = MOD_BUF_UINT)
	movzx bp, bh			; BP: right channel sample (unsigned)
	movzx dx, ch			; DX: next right channel sample (unsign)
	xor bh, bh			; BX: left channel sample (unsigned)
	xor ch, ch			; CX: next left channel sample (unsignd)
	%else
	movsx bp, bh			; BP: right channel sample (signed)
	movsx dx, ch			; DX: next right channel sample (signed)
	movsx bx, bl			; BX: left channel sample (signed)
	movsx cx, cl			; CX: next left channel sample (signed)
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	mov ebx, [esi]			; BX: left channel sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov ecx, [edx]			; CX: next left channel sample
	shld ebp, ebx, 16		; BP: right channel sample
	shld edx, ecx, 16		; DX: next right channel sample

	%elif (%1 = MOD_BUF_1632BIT)

	mov ebx, [esi]			; EBX: left channel sample
	mov ebp, [esi + sample_size]	; EBP: right channel sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov ecx, [edx]			; ECX: next left channel sample
	mov edx, [edx + sample_size]	; EDX: next right channel sample

	%endif				; Bit depth

	%endif				; Mono/stereo

	;----------------------------------------------------------------------
	; Convert signed to unsigned, clip 32-bit values to 16-bit

	%if (%3 = MOD_BUF_INT)

	%if (%1 = MOD_BUF_8BIT)

	add bl, 0x80
	add cl, 0x80
	%if (channels = 2)
	add bp, 0x80
	add dl, 0x80
	and bp, 0xff
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	add bx, 0x8000
	add cx, 0x8000
	%if (channels = 2)
	add bp, 0x8000
	add dx, 0x8000
	%endif

	%elif (%1 = MOD_BUF_1632BIT)

	add ebx, 0x8080
	add ecx, 0x8080
	%if (channels = 2)
	add ebp, 0x8080
	add edx, 0x8080
	%endif

	; Clip 32-bit values to 16-bit

	test ebx, 0xffff0000
	jnz .clip_left
	sar ebx, SCOPE_V_SHIFTS + 8

.check_clip_next_left:
	test ecx, 0xffff0000
	jnz .clip_next_left
	sar ecx, SCOPE_V_SHIFTS + 8

.check_clip_next_right:
	%if (channels = 2)
	test edx, 0xffff0000
	jnz .clip_next_right
	sar edx, SCOPE_V_SHIFTS + 8

.check_clip_right:
	test ebp, 0xffff0000
	jnz .clip_right
	sar ebp, SCOPE_V_SHIFTS + 8

.check_clip_done:
	%endif				; Stereo

	%endif				; Bit depth

	%endif				; Signed/unsigned

	;----------------------------------------------------------------------
	; Convert sample values to bitmap offset

	%if (%1 = MOD_BUF_8BIT)

	shr bx, SCOPE_V_SHIFTS		; Truncate to scope height
	shr cx, SCOPE_V_SHIFTS
	%if (channels = 2)
	shr bp, SCOPE_V_SHIFTS
	shr dx, SCOPE_V_SHIFTS
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	shr bx, SCOPE_V_SHIFTS + 8	; Truncate to scope height
	shr cx, SCOPE_V_SHIFTS + 8
	%if (channels = 2)
	shr bp, SCOPE_V_SHIFTS + 8
	shr dx, SCOPE_V_SHIFTS + 8
	%endif

	%endif				; Bit depth

	sub cx, bx			; CX: next left channel sample diff
	sets ch
	%if (channels = 2)
	sub dx, bp			; DX: next right channel sample diff
	sets dh
	%endif
	add cl, ch			; CL: left sample diff for floor round

	%if (channels = 1)
	shl bx, SCOPE_H_SHIFTS + 2	; Multiply by width in bytes
	%else
	shl bx, SCOPE_H_SHIFTS + 1	; Multiply by width in bytes
	add dl, dh			; DL: right sample diff for floor round
	shl bp, SCOPE_H_SHIFTS + 1
	%endif

	or es:[di + bx], al		; Set pixel in scope bitmap
	sar cl, 1			; CL: left sample diff / 2
	jz .render_right		; Next sample is right above/below

	;----------------------------------------------------------------------
	; Render a vertical line towards the next sample pixel - left channel

	%if (channels = 2)
	push dx				; Use DX for temporary storage
	%endif

	setns dl			; DL: 1 if next sample lower
	mov ch, cl
	xor dh, dh
	sar ch, 7
	xor cl, ch
	sub cl, ch			; CL: abs(left sample diff / 2)
	dec dx
	setz ch				; CH: 1 if next sample higher
	add dl, ch			; DL: next sample > current ? 1 : -1

	%if (channels = 1)
	shl dx, SCOPE_H_SHIFTS + 2	; DX: bytes to add for next row
	%else
	shl dx, SCOPE_H_SHIFTS + 1	; DX: bytes to add for next row
	%endif

	mov ch, cl			; CH: abs(left sample diff / 2)

.render_left_line_loop:
	add bx, dx			; Adjust offset to next row
	or es:[di + bx], al		; Set pixel in scope bitmap
	dec cl
	jnz .render_left_line_loop
	ror al, 1			; Adjust pixel bitmask for next column
	jnc .render_left_line_2_loop
	cmp ah, 1			; Can render to next character?
	je .render_left_line_done
	add bx, 2			; Yes, adjust offset

.render_left_line_2_loop:
	add bx, dx			; Adjust offset to next row
	or es:[di + bx], al		; Set pixel in scope bitmap
	dec ch
	jnz .render_left_line_2_loop

.render_left_line_done:
	rol al, 1			; Restore pixel bitmask

	%if (channels = 2)
	pop dx
	%endif

.render_right:
	add esi, sample_next		; Adjust poitner to next sample

	%if (channels = 2)

	; Render right channel

	or es:[di + bp + SCOPE_BMP_SIZE], al
	sar dl, 1			; DL: right sample diff / 2
	jz .render_next_pixel		; Next sample is right above/below

	;----------------------------------------------------------------------
	; Render a vertical line towards the next sample pixel - right channel

	setns bl			; BL: 1 if next sample lower
	mov dh, dl
	xor bh, bh
	sar dh, 7
	xor dl, dh
	sub dl, dh			; DL: abs(right sample diff / 2)
	dec bx
	setz dh				; DH: 1 if next sample higher
	add bl, dh			; BL: next sample > current ? 1 : -1
	shl bx, SCOPE_H_SHIFTS + 1	; BX: bytes to add for next row
	mov dh, dl			; DH: abs(right sample diff / 2)

.render_right_line_loop:
	add bp, bx			; Adjust offset to next row
	or es:[di + bp + SCOPE_BMP_SIZE], al
	dec dl
	jnz .render_right_line_loop
	ror al, 1			; Adjust pixel bitmask for next column
	jnc .render_right_line_2_loop
	cmp ah, 1			; Can render to next character?
	je .render_right_line_done
	add bp, 2			; Yes, adjust offset

.render_right_line_2_loop:
	add bp, bx			; Adjust offset to next row
	or es:[di + bp + SCOPE_BMP_SIZE], al
	dec dh
	jnz .render_right_line_2_loop

.render_right_line_done:
	rol al, 1			; Restore pixel bitmask

	%endif

.render_next_pixel:

	;----------------------------------------------------------------------
	; Check to render next pixel/character in scope bitmap

	ror al, 1			; Next pixel in character
	jnc .loop_render

.render_next_char:
	add di, 2
	dec ah				; Next character
	jnz .loop_render

	pop es
	retn

	;----------------------------------------------------------------------
	; Output buffer position reset jump targets when end of buffer reached

.reset_search_pos:
	mov esi, edx
	jmp .search_pos

.reset_search_neg:
	mov esi, edx
	jmp .search_neg

.reset_position:
	mov esi, 0x12345678
	.buffer_start EQU $ - 4
	jmp .render

.wrap_next_sample:
	mov edx, 0x12345678
	.buffer_start_2 EQU $ - 4
	jmp .get_next_sample

	;----------------------------------------------------------------------
	; Clip 16-bit sample in 32-bit render buffer to 16-bit values

	%if (%1 = MOD_BUF_1632BIT && %3 = MOD_BUF_INT)

.clip_left:
	cmp ebx, 0
	setg bl				; BL: 1 if positive clip, else 0
	xor bh, bh
	neg bl				; BX: 255 if positive clip, else 0
	shr bx, SCOPE_V_SHIFTS
	jmp .check_clip_next_left

.clip_next_left:
	cmp ecx, 0
	setg cl				; CL: 1 if positive clip, else 0
	xor ch, ch
	neg cl				; CX: 255 if positive clip, else 0
	shr cx, SCOPE_V_SHIFTS
	jmp .check_clip_next_right

	%if (channels = 2)

.clip_next_right:
	cmp edx, 0
	setg dl				; DL: 1 if positive clip, else 0
	xor dh, dh
	neg dl				; DX: 255 if positive clip, else 0
	shr dx, SCOPE_V_SHIFTS
	jmp .check_clip_right

.clip_right:
	cmp ebp, 0
	setg dh				; DH: 1 if positive clip, else 0
	neg dh				; DH: 255 if positive clip, else 0
	movzx bp, dh			; BP: 255 if positive clip, else 0
	xor dh, dh
	shr bp, SCOPE_V_SHIFTS
	jmp .check_clip_done

	%endif				; Stereo

	%endif				; 16-bit in 32-bit render buffer

%endmacro


;------------------------------------------------------------------------------
; Render output devices scopes to interleaved bitmap buffer.
;------------------------------------------------------------------------------
; -> DS - Application data segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

render_scopes:
	mov ah, [output_info + mod_output_info.buffer_format]
	mov si, renderfntab		; DS:SI: render function jump table
	mov al, RENDERFNTAB_SIZE	; AL: entries in jump table

.renderfn_loop:
	cmp [si], ah			; Entry found in jump table?
	je .renderfn			; Yes, jump to render function
	add si, 8			; Go to next entry in jump table
	dec al
	jnz .renderfn_loop

	; No matching scope render function, don't render anything

	retn

.renderfn:
	jmp [si + 4]

render_scopes_8_m_u:
	render_scopes_for MOD_BUF_8BIT, MOD_BUF_1CHN, MOD_BUF_UINT

render_scopes_8_s_u:
	render_scopes_for MOD_BUF_8BIT, MOD_BUF_2CHN, MOD_BUF_UINT

render_scopes_16_m_u:
	render_scopes_for MOD_BUF_16BIT, MOD_BUF_1CHN, MOD_BUF_UINT

render_scopes_16_s_u:
	render_scopes_for MOD_BUF_16BIT, MOD_BUF_2CHN, MOD_BUF_UINT

render_scopes_32_m_s:
	render_scopes_for MOD_BUF_1632BIT, MOD_BUF_2CHNL, MOD_BUF_INT

render_scopes_32_s_s:
	render_scopes_for MOD_BUF_1632BIT, MOD_BUF_2CHN, MOD_BUF_INT


;------------------------------------------------------------------------------
; Write text to standard output.
;------------------------------------------------------------------------------
; -> DS - Application data segment
;    DS:ESI - Pointer to ASCIIZ string
;------------------------------------------------------------------------------

	align 4

echo:
	push eax
	push ebx
	push ecx
	push esi

	call far sys_str_len		; ECX: length of string
	add esi, [data_seg_addr]	; ESI: linear address of DS:ESI
	mov ebx, 1			; Write to file handle 1 (stdout)
	call far sys_file_write

	pop esi
	pop ecx
	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Print formatted string to stdout.
;------------------------------------------------------------------------------
; -> DS - Application data segment
;    DS:ESI - Source string to format
;    SS:BP - Pointer just above first variable value
;------------------------------------------------------------------------------
; See sys_str_format for usage details.
;------------------------------------------------------------------------------

	align 4

printf:
	push esi
	push edi

	mov edi, [io_buf_addr]
	call far sys_str_format
	mov esi, edi
	call echo

	pop edi
	pop esi
	retn


;------------------------------------------------------------------------------
; Get message from lookup table
;------------------------------------------------------------------------------
; -> EAX - Lookup code
;    DS:ESI - Code - message lookup table
; <- DS:ESI - Pointer to message
;------------------------------------------------------------------------------

	align 4

lookup_message:
	push eax
	push ebx

	cld

	mov ebx, eax			; EBX: lookup code

.find_message_loop:
	a32 lodsd
	test eax, eax
	jz .echo			; End of table, return fallback pointer
	cmp eax, ebx
	je .echo			; Code found
	add esi, 4
	jmp .find_message_loop

.echo:
	mov esi, [esi]			; Return message pointer

	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Parse command line arguments.
;------------------------------------------------------------------------------
; -> DS - Application data segment
;    DS:EBX - Pointer to mod_out_params structure to fill with parsed values
; <- CF - Set if a required parameter is missing
;    AH - Output device or 0 if not specified
;    AL - Output device type or 0 if not specified
;    CX - Amplification or 0 if not specified
;    EDX - Requested sample rate or 0 if not specified
;    DS:ESI - Pointer to ASCIIZ MOD filename
;------------------------------------------------------------------------------

	align 4

parse_args:
	push ebx
	push edi
	push ebp
	push es

	push eax
	push ecx
	push edx
	push esi

	xor ax, ax
	mov es, ax			; ES: zeropage

	; Set default output device parameters

	mov byte [ebx + mod_out_params.interpolation], MOD_IPOL_NN
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_CROSS
	mov byte [ebx + mod_out_params.initial_pan], 0x80
	mov word [ebx + mod_out_params.buffer_size], 17000

	; ---------------------------------------------------------------------
	; /ipol:mode - sample interpolation mode

	mov esi, arg_ipol
	call far sys_env_get_named_arg
	jc .check_device

	or byte [arg_flags], ARG_IPOL_SET
	mov edi, arg_ipol_linear	; /ipol:linear
	mov ecx, -1
	mov byte [ebx + mod_out_params.interpolation], MOD_IPOL_LINEAR
	call far sys_str_cmp
	jnc .check_device
	mov edi, arg_ipol_nn		; /ipol:nearest
	mov byte [ebx + mod_out_params.interpolation], MOD_IPOL_NN
	call far sys_str_cmp
	jnc .check_device

	mov bp, sp			; Invalid sample interpolation mode
	o32 push ds
	push esi
	o32 push ds
	push dword arg_ipol
	mov esi, err_arg_ipol
	call printf
	mov sp, bp
	jmp .error

.check_device:

	; ---------------------------------------------------------------------
	; /o:device - output device type

	mov esi, arg_out
	call far sys_env_get_named_arg
	jnc .check_out_device

	; No output device type, detect

	mov ah, MOD_OUT_SB		; Detect Sound Blaster
	call far mod_sb_detect
	jnc .check_port

	mov ah, MOD_OUT_DAC		; Nothing found, fallback to speaker
	mov al, MOD_DAC_SPEAKER
	jmp .check_port

.check_out_device:

	; /o:device - Parse output device type

	mov ah, MOD_OUT_SB		; /o:sb
	mov edi, arg_out_sb
	mov ecx, -1
	call far sys_str_cmp
	jc .check_out_sb16
	call far mod_sb_detect		; Detect SB type and parameters
	jc .sb_error			; No SB found, bail out
	jmp .check_port

.sb_error:
	mov esi, err_out_sb		; Can't find SB
	call echo
	jmp .error

.check_out_sb16:
	mov al, MOD_SB_16		; /o:sb16
	mov edi, arg_out_sb16
	mov ecx, -1
	call far sys_str_cmp
	jnc .check_sb_port
	mov al, MOD_SB_PRO		; /o:sbpro
	mov edi, arg_out_sbpro
	call far sys_str_cmp
	jnc .check_sb_port
	mov al, MOD_SB_2		; /o:sb2
	mov edi, arg_out_sb2
	call far sys_str_cmp
	jnc .check_sb_port
	mov al, MOD_SB_1		; /o:sb1
	mov edi, arg_out_sb1
	call far sys_str_cmp
	jnc .check_sb_port

	mov ah, MOD_OUT_DAC		; /o:lpt
	mov al, MOD_DAC_LPT
	mov edi, arg_out_lpt
	call far sys_str_cmp
	jnc .check_lpt_port
	mov al, MOD_DAC_LPTST		; /o:lptst
	mov edi, arg_out_lptst
	call far sys_str_cmp
	jnc .check_lpt_port
	mov al, MOD_DAC_SPEAKER		; /o:speaker
	mov edi, arg_out_speaker
	call far sys_str_cmp
	jnc .check_lpt_port

	mov bp, sp			; Invalid output device
	o32 push ds
	push esi
	o32 push ds
	push dword arg_out
	mov esi, err_arg_out
	call printf
	mov sp, bp
	jmp .error

.check_sb_port:

	; Set Sound Blaster default parameters

	mov word [ebx + mod_out_params.port], 0x220
	mov byte [ebx + mod_out_params.irq], 5
	mov byte [ebx + mod_out_params.dma], 1
	mov byte [ebx + mod_out_params.dma + 1], 5
	push ax
	call far mod_sb_detect		; Detect SB parameters
	pop ax
	jmp .check_port

.check_lpt_port:

	; Set LPT DAC default parameters

	mov cx, es:[0x408]		; LPT1 base address
	mov word [ebx + mod_out_params.port], cx

.check_port:
	mov bp, ax			; BP: output device / output device type

	; ---------------------------------------------------------------------
	; /p:port[,port2] - output device I/O port base address(es)

	mov esi, arg_port
	call far sys_env_get_named_arg
	jc .check_irq
	mov ah, ','
	mov ecx, -1
	call far sys_str_char_pos	; More, than one I/O port provided?
	jnc .multi_port

	; /p:port - single I/O port base address

	mov ecx, -1
	call .get_port_address
	jc .port_error
	mov [ebx + mod_out_params.port], dx
	jmp .check_irq

.port_error:
	mov bp, sp			; Invalid I/O port
	o32 push ds
	push esi
	o32 push ds
	push dword arg_port
	mov esi, err_arg_port
	call printf
	mov sp, bp
	jmp .error

.multi_port:

	; /p:port,port2 - two I/O port base addresses

	mov ecx, eax			; Get first up to separator (,)
	call .get_port_address
	jc .port_error
	mov [ebx + mod_out_params.port], dx

	add esi, ecx			; Get second up to end of argument
	inc esi
	mov ecx, -1
	call .get_port_address
	jc .port_error
	mov [ebx + mod_out_params.port + 2], dx

	; Change output device type to dual LPT DAC

	cmp bp, MOD_OUT_DAC * 256 + MOD_DAC_LPT
	jne .check_irq
	mov bp, MOD_OUT_DAC * 256 + MOD_DAC_LPTDUAL

.check_irq:

	; ---------------------------------------------------------------------
	; /i:irq - output device IRQ number

	mov esi, arg_irq
	call far sys_env_get_named_arg
	jc .check_dma
	push bx
	mov bx, 0xff00
	call far sys_str_parse_int	; EAX: IRQ number
	pop bx
	jc .irq_error
	cmp eax, 15			; IRQ number sanity check
	ja .irq_error
	mov [ebx + mod_out_params.irq], al
	jmp .check_dma

.irq_error:
	mov bp, sp			; Invalid IRQ number
	o32 push ds
	push esi
	o32 push ds
	push dword arg_irq
	mov esi, err_arg_irq
	call printf
	mov sp, bp
	jmp .error

.check_dma:

	; ---------------------------------------------------------------------
	; /d:dma[,dma16] - output device DMA channel(s)

	mov esi, arg_dma
	call far sys_env_get_named_arg
	jc .check_stereo
	mov ah, ','
	mov ecx, -1
	call far sys_str_char_pos	; More, than one I/O port provided?
	jnc .multi_dma

	; /d:dma - single DMA channel

	mov ecx, -1
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_out_params.dma], dl
	mov [ebx + mod_out_params.dma + 1], dl
	jmp .check_stereo

.dma_error:
	mov bp, sp			; Invalid DMA channel
	o32 push ds
	push esi
	o32 push ds
	push dword arg_dma
	mov esi, err_arg_dma
	call printf
	mov sp, bp
	jmp .error

.multi_dma:

	; /d:dma,dma16 - two DMA channels

	mov ecx, eax			; Get first up to separator (,)
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_out_params.dma], dl

	add esi, ecx			; Get second up to end of argument
	inc esi
	mov ecx, -1
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_out_params.dma + 1], dl

.check_stereo:

	; ---------------------------------------------------------------------
	; /s:mode[,initialpan%] - stereo rendering mode

	mov esi, arg_stereo
	call far sys_env_get_named_arg
	jc .check_samplerate

	or byte [arg_flags], ARG_PAN_SET
	mov edi, arg_stereo_mono	; /s:mono
	mov ecx, -1
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_MONO
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_hard	; /s:hard
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_HARD
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_x		; /s:cross
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_CROSS
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_real	; /s:real
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_REAL
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_rpan	; /s:real,initialpan%
	mov ecx, 5
	call far sys_str_cmp
	jc .stereo_error
	add esi, 5
	push bx
	mov bx, 0xff00
	call far sys_str_parse_int	; EAX: initial pan %
	pop bx
	jc .realpan_error
	cmp eax, 100
	ja .realpan_error
	mov ecx, 128			; Convert pan percentage to initial pan
	mul ecx				; EAX: initial pan % * 128
	mov ecx, 100
	div ecx				; EAX: floor(initial pan % * 128 / 100)
	cmp edx, 50			; Rounding
	setae dl			; DL: 1 if remainder >= 50, else 0
	movzx edx, dl
	add eax, edx			; EAX: round(initial pan % * 128 / 100)
	mov ecx, 128
	sub ecx, eax			; ECX: 128 - round(128 * initial pan %)
	mov byte [ebx + mod_out_params.initial_pan], cl
	jmp .check_samplerate

.stereo_error:
	mov bp, sp			; Invalid stereo mode
	o32 push ds
	push esi
	o32 push ds
	push dword arg_stereo
	mov esi, err_arg_stereo
	call printf
	mov sp, bp
	jmp .error

.realpan_error:
	mov bp, sp			; Invalid initial pan %
	o32 push ds
	push esi
	o32 push ds
	push dword arg_stereo
	o32 push ds
	push dword arg_stereo_real
	mov esi, err_arg_realpan
	call printf
	mov sp, bp
	jmp .error

.check_samplerate:

	; ---------------------------------------------------------------------
	; /sr:samplerate - Output samplerate

	mov edi, 44100			; EDI: default samplerate
	mov esi, arg_samplerate
	call far sys_env_get_named_arg
	jc .check_amp
	push bx
	mov bx, 0xff00
	call far sys_str_parse_int	; EAX: requested sample rate
	pop bx
	jc .samplerate_error
	cmp eax, 8000			; Minimum samplerate: 8000 Hz
	jl .samplerate_error
	mov edi, eax			; EDI: requested samplerate
	jmp .check_amp

.samplerate_error:
	mov bp, sp			; Invalid samplerate
	o32 push ds
	push esi
	o32 push ds
	push dword arg_samplerate
	mov esi, err_arg_smprate
	call printf
	mov sp, bp
	jmp .error

.check_amp:
	mov ax, 0x0100
	mov esi, arg_amp
	call far sys_env_get_named_arg
	jc .get_filename
	push bx
	mov bx, 0xff00
	mov ecx, 0x100
	call far sys_str_parse_fixed	; EAX: amplification
	pop bx
	jc .amp_error
	cmp eax, 0x400			; Maximum amplification: 4x
	ja .amp_error
	jmp .get_filename

.amp_error:
	mov bp, sp			; Invalid amplification
	o32 push ds
	push esi
	o32 push ds
	push dword arg_amp
	mov esi, err_arg_amp
	call printf
	mov sp, bp
	jmp .error

.get_filename:

	; ---------------------------------------------------------------------
	; Get filename

	xor cl, cl
	call far sys_env_get_arg	; DS:ESI: filename (first argument)
	jc .filename_error
	cmp byte [esi], '/'
	je .filename_error

	; Set output values

	add sp, 8			; Discard ESI and EDX from stack
	mov edx, edi			; EDX: requested samplerate
	pop ecx				; ECX: restore high word
	mov cx, ax			; CX: amplification
	pop eax				; EAX: restore high word
	mov ax, bp			; AH/AL: output device

	clc
	jmp .exit

.filename_error:
	mov esi, err_arg_fname		; Missing filename
	call echo
	jmp .error

.exit:
	pop es
	pop ebp
	pop edi
	pop ebx
	retn

.error:
	mov esi, err_args
	call echo

	pop esi
	pop edx
	pop ecx
	pop eax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Get port address from current argument value.
;------------------------------------------------------------------------------
; -> DS - Application data segment
;    ECX - Maximum number of argument characters to parse
;    DS:ESI - Pointer to argument value
;    ES - Zeropage
; <- CF - Set if error
;    DX - Parsed port address when no error
;------------------------------------------------------------------------------

	align 4

.get_port_address:
	push edi
	push dx

	mov dx, es:[0x408]		; DX: LPT1 port address
	mov edi, arg_port_lpt1		; /p:lpt1
	call far sys_str_cmp
	jnc .check_port_address
	mov dx, es:[0x40a]		; DX: LPT2 port address
	mov edi, arg_port_lpt2		; /p:lpt2
	call far sys_str_cmp
	jnc .check_port_address
	mov dx, es:[0x40c]		; DX: LPT3 port address
	mov edi, arg_port_lpt3		; /p:lpt3
	call far sys_str_cmp
	jnc .check_port_address
	mov dx, es:[0x40e]		; DX: LPT4 port address
	mov edi, arg_port_lpt4		; /p:lpt4
	call far sys_str_cmp
	jnc .check_port_address

	push eax
	push bx				; /p:XXXX
	mov bh, cl			; BH: number of characters to convert
	xor bl, bl			; BL: no terminator character
	call far sys_str_parse_hex	; EAX: port address
	mov edi, eax
	pop bx
	pop eax
	jc .error_port_address		; Invalid hexadecimal value
	test edi, 0xffff0000		; Invalid port address
	jnz .error_port_address
	mov dx, di			; DX: port address

.check_port_address:
	test dx, dx			; Port address sanity check
	jz .error_port_address
	add sp, 2			; Discard DX from stack
	clc

.exit_port_address:
	pop edi
	retn

.error_port_address:
	stc
	pop dx
	jmp .exit_port_address


;------------------------------------------------------------------------------
; Get DMA channel from current argument value.
;------------------------------------------------------------------------------
; -> ECX - Maximum number of argument characters to parse
;    DS:ESI - Pointer to argument value
; <- CF - Set if error
;    DL - Parsed DMA channel when no error
;------------------------------------------------------------------------------

	align 4

.get_dma_channel:
	push eax
	push bx
	push ecx

	mov bh, cl			; BH: number of characters to convert
	xor bl, bl			; BL: no terminator character
	call far sys_str_parse_int	; EAX: DMA channel
	jc .error_dma_channel		; Invalid decimal value
	cmp eax, 7			; Invalid DMA channel
	ja .error_dma_channel
	mov dl, al			; DL: DMA channel
	clc

.exit_dma_channel:
	pop ecx
	pop bx
	pop eax
	retn

.error_dma_channel:
	stc
	jmp .exit_dma_channel


;==============================================================================
; Data area
;==============================================================================

segment app_data public class=DATA align=16
segment app_data

		alignb 4

data_seg_addr	dd 0			; Linear address of data segment
io_buf_addr	dd 0			; Relative addr. of the file I/O buffer
cmb_size	dd 0			; Amount of available conventional mem.
xmb_size	dd 0			; Amount of available extended mem.
mod_info_addr	dd 0			; Relative address of mod_info structure
chn_info_addr	dd 0			; Relative address of mod_channel_info
scope_bmp_seg	dw 0			; Scope interleaved bitmap segment
scope_blit_ilv	db 0			; Current scope blit interleave number

header		db 'Therapy MOD player - quarter century later edition', 13, 10, 13, 10, 0
		HEADER_SIZE equ $ - header - 1

		; Command line usage

usage		db 'Usage: tmodplay <filename.mod> [options]',13, 10, 13, 10
		db 'Options: (case-sensitive)', 13, 10, 13, 10
		db '/o:device           Select output device. Available options for "device" are:', 13, 10
		db '                    - speaker: Internal PC speaker', 13, 10
		db '                    - lpt: One or two parallel port D/A converters *', 13, 10
		db '                    - lptst: Stereo parallel port D/A converter (stereo-on-1) *', 13, 10
		db '                    - sb: Sound Blaster (detect type from BLASTER env. var.) *', 13, 10
		db '                    - sb1: Sound Blaster (pre-2.0) *', 13, 10
		db '                    - sb2: Sound Blaster 2.0 *', 13, 10
		db '                    - sbpro: Sound Blaster Pro *', 13, 10
		db '                    - sb16: Sound Blaster 16 *', 13, 10
		db '                    * These devices use the software wavetable renderer.', 13, 10, 13, 10
		db '/p:port[,port2]     Output device base port in hexadecimal or as "lptX", where', 13, 10
		db '                    X is the LPT port number. To enable playback on two', 13, 10
		db '                    parallel port DACs, specify both printer ports in this', 13, 10
		db '                    option (for example: /p:lpt1,lpt2).', 13, 10, 13, 10
		db '/i:irq              IRQ number for the output device.', 13, 10, 13, 10
		db '/d:dma[,dma16]      DMA channel for the output device. Provide 16-bit DMA', 13, 10
		db '                    channel for SB 16. Alternatively, provide both 8-bit and', 13, 10
		db '                    16-bit DMA channels separated with a comma.', 13, 10, 13, 10
		db '/sr:samplerate      Sampling frequency in Hz.', 13, 10, 13, 10
		db '/s:mode[,panpct]    Output stereo mode for stereo devices. Accepted values for', 13, 10
		db '                    "mode":', 13, 10
		db '                    - mono: Force mono output (fast)', 13, 10
		db '                    - hard: Hard left/right panning as on Amiga (still fast)', 13, 10
		db '                    - cross: Set 25% left/right from center (slower)', 13, 10
		db '                    - real: Real panning via 8xx and E8x commands (slowest)', 13, 10, 13, 10
		db '                    "panpct" specifies the initial left/right panning from', 13, 10
		db '                    center for "real" mode (default: 0%).', 13, 10, 13, 10
		db '/amp:amplification  Output amplification between 0 - 4. Value is decimal.', 13, 10, 13, 10
		db '/ipol:mode          Set sample interpolation mode for software wavetable', 13, 10
		db '                    renderer. Accepted values for "mode":', 13, 10
		db '                    - nearest: Nearest neighbor (similar to Amiga)', 13, 10
		db '                    - linear: Linear interpolation (similar to GUS, slow)', 13, 10, 13, 10
		db '/?                  Display this command line usage information.', 13, 10
		db 0

		; Command line named arguments

arg_out		db '/o', 0
arg_out_sb	db 'sb', 0
arg_out_sb16	db 'sb16', 0
arg_out_sbpro	db 'sbpro', 0
arg_out_sb2	db 'sb2', 0
arg_out_sb1	db 'sb1', 0
arg_out_lpt	db 'lpt', 0
arg_out_lptst	db 'lptst', 0
arg_out_speaker	db 'speaker', 0
arg_port	db '/p', 0
arg_port_lpt1	db 'lpt1', 0
arg_port_lpt2	db 'lpt2', 0
arg_port_lpt3	db 'lpt3', 0
arg_port_lpt4	db 'lpt4', 0
arg_irq		db '/i', 0
arg_dma		db '/d', 0
arg_stereo	db '/s', 0
arg_stereo_mono	db 'mono', 0
arg_stereo_hard	db 'hard', 0
arg_stereo_x	db 'cross', 0
arg_stereo_real	db 'real', 0
arg_stereo_rpan	db 'real,', 0
arg_samplerate	db '/sr', 0
arg_amp		db '/amp', 0
arg_ipol	db '/ipol', 0
arg_ipol_nn	db 'nearest', 0
arg_ipol_linear	db 'linear', 0
arg_help	db '/?', 0

arg_flags	db 0

		; Error messages

err_cpu		db 'This program requires a 80386 or newer processor.', 13, 10, 0
		ERR_CPU_SIZE EQU $ - err_cpu - 1
err_out_sb	db 'Cannot find Sound Blaster.', 13, 10
		db 'Make sure the BLASTER environment variable is set correctly.', 13, 10, 0
err_arg_out	db 'Invalid device "{s}" for option {s}.', 13, 10, 0
err_arg_port	db 'Invalid I/O port "{s}" for option {s}.', 13, 10
		db 'Make sure to not insert any spaces before and after comma when specifying', 13, 10
		db 'multiple ports for two parallel port D/A converters.', 13, 10, 0
err_arg_irq	db 'Invalid IRQ number "{s}" for option {s}.', 13, 10, 0
err_arg_dma	db 'Invalid DMA channel "{s}" for option {s}.', 13, 10
		db 'Make sure to not insert any spaces before and after comma when specifying', 13, 10
		db 'separate 8-bit and 16-bit DMA channels.', 13, 10, 0
err_arg_stereo	db 'Invalid stereo mode "{s}" for option {s}.', 13, 10, 0
err_arg_realpan	db 'Invalid initial pan % "{s}" for stereo option {s}:{s}.', 13, 10
		db 'Use a value between 0 (hard side pan) and 100 (mono).', 13, 10, 0
err_arg_smprate	db 'Invalid samplerate "{s}" for option {s}.', 13, 10
		db 'Make sure the value is not less, than 8000.', 13, 10, 0
err_arg_amp	db 'Invalid amplification "{s}" for option {s}.', 13, 10
		db 'Use a value between 0.0 (silence) and 4.0.', 13, 10, 0
err_arg_ipol	db 'Invalid interpolation mode "{s}" for option {s}.', 13, 10, 0
err_arg_fname	db 'Please specify the name of the file to play.', 13, 10, 0
err_args	db 13, 10, 'Type tmodplay /? for help.', 13, 10, 0

err_mod_invalid	db 'Invalid MOD file format.', 13, 10, 0
err_mod_nb_chan	db 'Too many channels in the MOD file.', 13, 10, 0
err_mod_device	db 'Cannot initialize output device.', 13, 10, 0
err_sys_v86	db 'Cannot initialize system, CPU already in V86 mode. Please remove any offending', 13, 10
		db 'memory managers (HIMEM.SYS can stay).', 13, 10, 0
err_sys_lh	db 'This program cannot run in high memory with the current memory manager.', 13, 10, 0
err_gui_novga	db 'This program requires a VGA display adapter.', 13, 10, 0
err_dos_02	db 'File not found.', 13, 10, 0
err_dos_03	db 'Path not found.', 13, 10, 0
err_dos_04	db 'Too many open files.', 13, 10, 0
err_dos_05	db 'Access denied.', 13, 10, 0
err_dos_06	db 'Invalid handle.', 13, 10, 0
err_dos_07	db 'Memory control blocks destroyed, possible memory corruption.', 13, 10, 0
err_dos_08	db 'Insufficient memory.', 13, 10, 0
err_dos_09	db 'Invalid memory block address.', 13, 10, 0
err_dos_0a	db 'Invalid environment.', 13, 10, 0
err_dos_0f	db 'Invalid drive.', 13, 10, 0
err_generic	db 'Unable to play the file.', 13, 10, 0

		alignb 4
errtab_mod	dd MOD_ERR_INVALID, err_mod_invalid
		dd MOD_ERR_NB_CHN, err_mod_nb_chan
		dd MOD_ERR_DEVICE, err_mod_device
		dd 0x02, err_dos_02
		dd 0x03, err_dos_03
		dd 0x04, err_dos_04
		dd 0x05, err_dos_05
		dd 0x06, err_dos_06
		dd 0x07, err_dos_07
		dd 0x08, err_dos_08
		dd 0x09, err_dos_09
		dd 0x0a, err_dos_0a
		dd 0x0f, err_dos_0f
		dd 0, err_generic

errtab_sys	dd SYS_ERR_V86, err_sys_v86
		dd SYS_ERR_LH, err_sys_lh
		dd 0x02, err_dos_02
		dd 0x03, err_dos_03
		dd 0x04, err_dos_04
		dd 0x05, err_dos_05
		dd 0x06, err_dos_06
		dd 0x07, err_dos_07
		dd 0x08, err_dos_08
		dd 0x09, err_dos_09
		dd 0x0a, err_dos_0a
		dd 0x0f, err_dos_0f
		dd 0, err_generic

errtab_gui	dd VID_ERR_NOVGA, err_gui_novga
		dd 0x02, err_dos_02
		dd 0x03, err_dos_03
		dd 0x04, err_dos_04
		dd 0x05, err_dos_05
		dd 0x06, err_dos_06
		dd 0x07, err_dos_07
		dd 0x08, err_dos_08
		dd 0x09, err_dos_09
		dd 0x0a, err_dos_0a
		dd 0x0f, err_dos_0f
		dd 0, err_generic

		; Output devices

out_unknown	db 'Initializing playback', 13, 10, 0
out_speaker	db 'Using internal PC speaker', 13, 10, 0
out_lpt		db 'Using parallel port DAC on port {X16}h', 13, 10, 0
out_lptst	db 'Using stereo parallel port DAC on port {X16}h', 13, 10, 0
out_lptdual	db 'Using dual parallel port DAC on ports {X16}h and {X16}h', 13, 10, 0
out_sb1		db 'Using Sound Blaster on port {X16}h{>}, IRQ {u8}{>}, DMA {u8}', 13, 10, 0
out_sb2		db 'Using Sound Blaster 2.0 on port {X16}h{>}, IRQ {u8}{>}, DMA {u8}', 13, 10, 0
out_sbpro	db 'Using Sound Blaster Pro on port {X16}h{>}, IRQ {u8}{>}, DMA {u8}', 13, 10, 0
out_sb16	db 'Using Sound Blaster 16 on port {X16}h{>}, IRQ {u8}{>}, DMA {>}{u8}', 13, 10, 0
msg_samplerate	db 'Playback sampling rate: {u} Hz', 13, 10, 0
msg_loading	db 'Loading file: {s}', 13, 10, 0

		alignb 4
outtab		dd MOD_OUT_DAC * 256 + MOD_DAC_SPEAKER, out_speaker
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPT, out_lpt
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPTST, out_lptst
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPTDUAL, out_lptdual
		dd MOD_OUT_SB * 256 + MOD_SB_1, out_sb1
		dd MOD_OUT_SB * 256 + MOD_SB_2, out_sb2
		dd MOD_OUT_SB * 256 + MOD_SB_PRO, out_sbpro
		dd MOD_OUT_SB * 256 + MOD_SB_16, out_sb16
		dd 0, out_unknown

		; Scope render functions for possible output buffer combinations

		alignb 4
renderfntab	dd MOD_BUF_8BIT | MOD_BUF_1CHN | MOD_BUF_UINT, render_scopes_8_m_u
		dd MOD_BUF_8BIT | MOD_BUF_2CHN | MOD_BUF_UINT, render_scopes_8_s_u
		dd MOD_BUF_16BIT | MOD_BUF_1CHN | MOD_BUF_UINT, render_scopes_16_m_u
		dd MOD_BUF_16BIT | MOD_BUF_2CHN | MOD_BUF_UINT, render_scopes_16_s_u
		dd MOD_BUF_1632BIT | MOD_BUF_2CHNL | MOD_BUF_INT, render_scopes_32_m_s
		dd MOD_BUF_1632BIT | MOD_BUF_2CHN | MOD_BUF_INT, render_scopes_32_s_s
		RENDERFNTAB_SIZE EQU ($ - renderfntab) / 8

		; Output device parameters

		alignb 4
out_params	db mod_out_params.strucsize dup (0)

		; Output device information

		alignb 4
output_info	db mod_output_info.strucsize dup (0)

		; File function pointers (all far)

		alignb 4
file_fns	istruc mod_file_fns
		set_file_fn(open, sys_file_open)
		set_file_fn(read, sys_file_read)
		set_file_fn(close, sys_file_close)
		iend


;==============================================================================
; Stack
;==============================================================================

segment stack stack class=STACK align=16

	resb 4096
