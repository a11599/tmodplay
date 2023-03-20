;==============================================================================
; MOD player - public interface
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "mod/consts/public.inc"
%include "mod/consts/player.inc"
%include "mod/structs/global.inc"
%include "mod/structs/player.inc"
%include "mod/api/loader.inc"
%include "mod/api/routine.inc"
%include "mod/api/out_none.inc"
%include "mod/api/out_dac.inc"
%include "mod/api/out_sb.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	file_fn(fn) mod.file_fns + mod_file_fns. %+ fn
%define	out_fn(fn) mod.out_fns + mod_out_fns. %+ fn
%define	sample(var) mod_sample. %+ var

segment modplayer public use16 class=CODE align=16
segment modplayer


;------------------------------------------------------------------------------
; Setup the MOD player.
;------------------------------------------------------------------------------
; -> AH - Output device (MOD_OUT_*)
;    AL - Output device type
;    CH.CL - Amplification in 8.8-bit fixed point format *, **
;    EDX - Requested sample rate **
;    DS:EBX - Pointer to mod_out_params structure
;    DS:EDI - Pointer to mod_file_fns structure
; <- CF - Set on error
;    EAX - Error code or actual sample rate
;    ES - Player instance segment if no error
;------------------------------------------------------------------------------
; * Amplification is a range between 0.00 - 4.00, where 0.00 is silence and
; 4.00 is maximum (values above 4.00 are treated as 4.00). At 4.00 one channel
; can saturate the entire output, at 1.00 four channels are required to saturate
; the output sample range. Setting amplification to 4 / number of channels
; always avoids clipping, but reduces the useful output dynamic range for most
; MODs since they usually don't saturate the output range, especially
; multichannel MODs.
;------------------------------------------------------------------------------
; ** Some output devices ignore this setting.
;------------------------------------------------------------------------------

	align 4

global mod_setup
mod_setup:
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ds
	push es

	cld

	; Validate output device

	cmp ah, mod_out_dev.strucsize / 2
	jb .alloc_data
	mov ax, MOD_ERR_DEV_UNK
	jmp .error

.alloc_data:

	; ---------------------------------------------------------------------
	; Allocate memory for player data

	push ecx			; CX: Amplification
	push eax			; AX: output device
	push ebx			; BX: device parameters (ptr)

	log {'Allocating {u} bytes for MOD player data', 13, 10}, mod.strucsize

	mov ebx, mod.strucsize
	mov al, SYS_MEM_LO
	call far sys_mem_alloc
	jnc .init_data

	add sp, 12			; Discard EBX, EAX and ECX from stack
	jmp .error

.init_data:
	mov ebx, eax
	shr eax, 4
	mov es, ax			; ES: player data segment

	log {'Player data allocated @{X16}:0000', 13, 10}, es

	push edi			; Push mod_file_fns structure pointer

	; Zero out data area

	xor ax, ax
	mov cx, mod.strucsize / 2
	xor di, di
	rep stosw

	; Initialize defaults

	mov byte es:[mod.state], STATE_STOP
	mov es:[mod.instance_addr], ebx

	; Copy dependent function pointers

	pop esi				; Pop mod_file_fns ptr into SI
	mov edi, mod.file_fns
	mov ecx, mod_file_fns.strucsize / 2
	a32 rep movsw

	; Copy output device parameters

	pop esi				; Pop mod_out_params ptr into SI
	mov edi, mod.out_params
	mov ecx, mod_out_params.strucsize / 2
	a32 rep movsw

	mov ax, es
	mov ds, ax			; DS: player instance segment

	; ---------------------------------------------------------------------
	; Setup output device

	pop eax

	; Copy output device function pointers

	movzx si, ah			; movzx si, ah
	shl si, 1
	mov si, cs:[out_devices + si]	; SI: mod_out_fns pointer structure
	mov di, mod.out_fns
	mov cx, mod_out_fns.strucsize / 2

	push eax

.loop_out_fns:
	mov ax, cs:[si]
	stosw
	add si, 2
	dec cx
	jnz .loop_out_fns

	pop eax
	pop ecx

	; Input parameters ready, invoke output device setup method

	call [out_fn(setup)]
	jc .error
	mov [mod.extra_samples], ebx
	mov [mod.sample_padding], ecx
	add sp, 2			; Discard ES from stack
	clc

.exit:
	pop ds
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	retf

.error:
	pop es
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Load a MOD file into memory.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to ASCIIZ filename
;    ES - Player instance segment
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global mod_load
mod_load:
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push ds
	push eax

	; Open MOD file

	log {'Opening file "{s}"', 13, 10}, ds, esi

	mov al, 0			; Open for read
	call far [es:file_fn(open)]
	jc .error

.save_file_handle:
	mov es:[mod.file_handle], eax	; Save file handle

	log {'File opened, handle: {u}', 13, 10}, eax

	mov ax, es
	mov ds, ax			; DS: player instance segment

	; Load MOD header

	call mod_load_header
	jc .error_close

	; Read samples from MOD file

	mov ebp, [mod.extra_samples]
	add ebp, [mod.sample_padding]
	mov esi, mod.sample_hdr
	mov dl, [mod.num_samples]
	test dl, dl
	jz .done

	log {'Wavetable requires {u} bytes of extra memory for each sample', 13, 10, 'Loading samples', 13, 10}, ebp

.loop_sample:
	mov ebx, [si + sample(length)]
	mov ecx, ebx
	test ebx, ebx			; Skip empty samples
	jz .next_sample

	; Allocate memory

	add ebx, ebp
	mov al, SYS_MEM_HI_LO
	call far sys_mem_alloc
	jc .error_close
	mov [si + sample(addr)], eax

	log {'Allocated {u} bytes @{X} for sample "{s}"', 13, 10}, ebx, eax, ds, esi

	; Read from file into allocated memory

	mov ebx, [mod.file_handle]
	mov edi, eax
	add edi, [mod.sample_padding]	; Load after padding
	call far [file_fn(read)]
	jc .error_close

	cmp eax, ecx			; Check end of file
	je .upload_sample
	sub ecx, eax
	cmp ecx, 16
	jbe .upload_sample
	mov eax, MOD_ERR_INVALID	; >16 samples missing, bail out
	jmp .error_close

.upload_sample:
	call [out_fn(upload_sample)]
	jc .error_close
	mov [si + sample(wt_id)], eax

	; Release memory if wavetable has own sample RAM

	test dh, dh
	jz .next_sample
	mov eax, edi

	log {'Disposing sample @{X}', 13, 10}, eax

	call far sys_mem_free
	mov dword [si + sample(addr)], 0

.next_sample:
	add si, mod_sample.strucsize	; Advance sample header pointer
	dec dl				; Next sample
	jnz .loop_sample

	; Close file

	call .close_file

.done:
	pop eax
	clc

.exit:
	pop ds
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	retf

.error_close:
	push eax
	push cs				; Simulate far call
	call mod_unload			; Free allocated memory
	call .close_file		; Close file
	pop eax

.error:
	add sp, 4			; Discard EAX from stack
	stc
	jmp .exit

; Close the MOD file.
; -> DS - Player instance segment

.close_file:

	log {'Closing file', 13, 10}

	push eax			; May be destroyed by file_close
	call far [file_fn(close)]
	pop eax
	mov dword [mod.file_handle], 0
	retn


;------------------------------------------------------------------------------
; Unload MOD file from memory.
;------------------------------------------------------------------------------
; -> ES - Player instance segment
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global mod_unload
mod_unload:
	push eax
	push cx
	push dx
	push si
	push di
	push ds

	mov ax, es
	mov ds, ax

	cld

	; Stop playback if still playing

	push cs
	call mod_stop

	; Release pattern data memory

	mov eax, [mod.pattern_addr]
	test eax, eax
	jz .free_samples
	add eax, [mod.instance_addr]	; EAX: pattern data linear address

	log {'Disposing patterns @{X}', 13, 10}, eax

	call far sys_mem_free
	mov dword [mod.pattern_addr], 0

.free_samples:

	; Release sample memory

	mov si, mod.sample_hdr
	mov dl, [mod.num_samples]
	test dl, dl
	jz .reset_data

.loop_sample:
	mov eax, [si + sample(addr)]
	test eax, eax
	jz .free_sample

	log {'Disposing sample @{X}', 13, 10}, eax

	call far sys_mem_free
	mov dword [si + sample(addr)], 0

.free_sample:
	mov eax, [si + sample(wt_id)]
	call [out_fn(free_sample)]
	mov dword [si + sample(wt_id)], 0

.next_sample:
	add si, mod_sample.strucsize	; Advance sample header pointer
	dec dl				; Next sample
	jnz .loop_sample

.reset_data:

	; Fill file-specific data with zeroes

	xor ax, ax
	mov cx, (mod.file_data_end - mod.file_data) / 2
	mov di, mod.file_data
	rep stosw

	clc
	pop ds
	pop di
	pop si
	pop dx
	pop cx
	pop eax
	retf


;------------------------------------------------------------------------------
; Shutdown the MOD playback system.
;------------------------------------------------------------------------------
; -> ES - Player instance segment
; <- CF - Set on error
;    EAX - Error code if CF set
;    Player instance segment is no longer valid!
;------------------------------------------------------------------------------

	align 4

global mod_shutdown
mod_shutdown:
	push eax
	push ds

	; Unload module

	push cs
	call mod_unload

	xor eax, eax
	mov ax, es
	mov ds, ax

	; Shutdown the output device

	log {'Shutting down output device', 13, 10}

	call [out_fn(shutdown)]

	; Free player instance segment

	log {'Disposing MOD player data @{X16}:0000', 13, 10}, ax

	shl eax, 4
	call far sys_mem_free

	clc
	pop ds
	pop eax
	retf


;------------------------------------------------------------------------------
; Return MOD flags after the file has been loaded.
;------------------------------------------------------------------------------
; -> ES - Player instance segment
; <- EAX - MOD flags (MOD_FLG_*)
;------------------------------------------------------------------------------

	align 4

global mod_get_flags
mod_get_flags:
	mov eax, es:[mod.flags]
	retf


;------------------------------------------------------------------------------
; Return the number of channels in the loaded MOD file.
;------------------------------------------------------------------------------
; -> ES - Player instance segment
; <- AL - Number of channels
;------------------------------------------------------------------------------

	align 4

global mod_get_channel_count
mod_get_channel_count:
	mov al, es:[mod.num_channels]
	retf


;------------------------------------------------------------------------------
; Set the amplification level.
;------------------------------------------------------------------------------
; -> AH.AL - Amplification in 8.8 fixed point value *
;    ES - Player instance segment
;------------------------------------------------------------------------------
; * See notes in mod_setup.
;------------------------------------------------------------------------------

	align 4

global mod_set_amplify
mod_set_amplify:
	push ax
	push ds

	push es
	pop ds
	call [out_fn(set_amplify)]

	pop ds
	pop ax
	retf


;------------------------------------------------------------------------------
; Set sample interpolation.
;------------------------------------------------------------------------------
; -> AL - Sample interpolation method (MOD_IPOL_*)
;    ES - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_set_interpolation
mod_set_interpolation:
	push ax
	push ds

	push es
	pop ds
	call [out_fn(set_interpol)]

	pop ds
	pop ax
	retf


;------------------------------------------------------------------------------
; Set stereo rendering mode.
;------------------------------------------------------------------------------
; -> AL - Stereo rendering mode (MOD_PAN_*)
;    ES - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_set_stereo_mode
mod_set_stereo_mode:
	push ax
	push ds

	push es
	pop ds
	call [out_fn(set_stereomode)]

	pop ds
	pop ax
	retf


;------------------------------------------------------------------------------
; Start playback.
;------------------------------------------------------------------------------
; -> ES - Player instance segment
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global mod_play
mod_play:
	push ds
	push bx

	mov bx, es
	mov ds, bx				; DS: player instance segment

	; Don't start again if already playing

	mov bl, STATE_PLAY
	xchg bl, byte [mod.state]
	cmp bl, STATE_STOP
	je .start
	mov eax, MOD_ERR_PLAYING
	stc
	jmp .exit

.start:

	; Start output device

	call mod_playroutine_reset
	call [out_fn(play)]
	jnc .exit

	; Couldn't start playback

	mov byte [mod.state], STATE_STOP

.exit:
	pop bx
	pop ds
	retf


;------------------------------------------------------------------------------
; Stop playback.
;------------------------------------------------------------------------------
; -> ES - Player instance segment
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global mod_stop
mod_stop:
	push ds
	push bx

	mov bx, es
	mov ds, bx			; DS: player instance segment

	; Don't stop again if not playing

	mov bl, STATE_STOP
	xchg bl, byte [mod.state]
	cmp bl, STATE_STOP
	je .exit

	; Stop output device

	call [out_fn(stop)]
	jnc .exit

	; Couldn't stop playback (this should not happen)

	mov byte [mod.state], STATE_PLAY

.exit:
	pop bx
	pop ds
	retf


;------------------------------------------------------------------------------
; Render audio into the output buffer.
;------------------------------------------------------------------------------
; Call this function at an interval when it is convenient for your main program
; to spend extra time with audio rendering (for example during vertical
; retrace). Make sure the output buffer is large enough to not drain sooner
; than you can call this function. If the buffer drains, the player will render
; the next chunk of audio during the timer interrupt (the main program code
; will have to wait for it to finish).
;------------------------------------------------------------------------------
; -> ES - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_render
mod_render:
	push eax
	push ds

	mov ax, es
	mov ds, ax			; DS: player instance segment

	; Don't render if not playing

	cmp byte [mod.state], STATE_STOP
	je .exit

	; Render audio into output buffer

	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	call [out_fn(render)]

	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx

.exit:
	pop ds
	pop eax
	retf


;------------------------------------------------------------------------------
; Copies data from mod structure into mod_info structure.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;    ES:EDI - Pointer to mod_info structure
;    %1 - Structure member to copy
;    %2 - Temporary register used to copy data
; <- %2 - Contains structure member value
;------------------------------------------------------------------------------

%macro	copy_mod_info 2

	mov %2, [mod.%1]
	mov es:[edi + mod_info.%1], %2

%endmacro


;------------------------------------------------------------------------------
; Copies data from mod_sample structure into mod_sample_info structure.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to mod_sample structure
;    ES:EDI - Pointer to mod_sample_info structure
;    %1 - Structure member to copy
;    %2 - Temporary register used to copy data
; <- %2 - Contains structure member value
;------------------------------------------------------------------------------

%macro	copy_sample_info 2

	mov %2, [si + mod_sample.%1]
	mov es:[edi + mod_sample_info.%1], %2

%endmacro


;------------------------------------------------------------------------------
; Returns a memory buffer filled with MOD information.
;------------------------------------------------------------------------------
; -> AL - Memory allocation mode (SYS_MEM_*)
;    ES - Player instance segment
; <- CF - Set if error
;    EAX - Linear address of mod_info structure or error code if CF set
;------------------------------------------------------------------------------

	align 4

global mod_get_info
mod_get_info:
	push ebx
	push cx
	push edx
	push esi
	push edi
	push ds
	push es

	; Setup segment registers and allocate memory for mod_info structure

	mov bx, es
	mov ds, bx			; DS: player instance segment
	xor bx, bx
	mov es, bx			; ES: zeropage

	movzx ebx, byte [mod.num_samples]
	imul bx, mod_sample_info.strucsize
	add bx, mod_info.strucsize	; EBX: memory needed for mod_info
	call far sys_mem_alloc
	jc .exit
	mov edx, eax			; EDX: mod_info linear address

	log {'Allocated {u} bytes for MOD info structure @{X32}', 13, 10}, ebx, edx

	; Copy header info

	mov esi, mod.title		; Title
	mov cx, 21
	lea edi, [edx + mod_info.title]
	a32 rep movsb

	mov edi, eax
	copy_mod_info num_channels, al	; Number of samples
	copy_mod_info num_samples, al	; Number of channels
	copy_mod_info length, al	; Song length
	copy_mod_info num_patterns, al	; Number of patterns
	copy_mod_info restart_pos, al	; Restart position
	copy_mod_info flags, eax	; MOD flags

	mov bx, es			; Linear address of pattern data
	movzx eax, bx
	shl eax, 4
	add eax, [mod.pattern_addr]
	mov es:[edi + mod_info.pattern_addr], eax

	; Copy sample info

	mov bl, [mod.num_samples]
	mov esi, mod.sample_hdr
	lea edi, [edx + mod_info.samples]

.sample_info_loop:
	mov cx, 23			; Sample name
	a32 rep movsb
	sub si, 23
	sub edi, 23

	copy_sample_info addr, eax	; Linear address
	copy_sample_info length, eax	; Length
	copy_sample_info rpt_start, eax	; Repeat start
	copy_sample_info rpt_len, eax	; Repeat length

	add esi, mod_sample.strucsize
	add edi, mod_sample_info.strucsize
	dec bl
	jnz .sample_info_loop

	mov eax, edx
	clc

.exit:
	pop es
	pop ds
	pop edi
	pop esi
	pop edx
	pop cx
	pop ebx
	retf


;------------------------------------------------------------------------------
; Return information about channels.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to buffer receiving mod_channel_info structures
;    ES - Player instance segment
; <- DS:ESI - Filled with data
;------------------------------------------------------------------------------

	align 4

global mod_get_channel_info
mod_get_channel_info:
	push edi
	push ds
	push es

	push ds
	push es
	pop ds				; DS: player instance segment
	pop es
	mov edi, esi			; ES:EDI: mod_channel_info buffer

	call mod_playroutine_get_channel_info
	call [out_fn(get_mixer_info)]

	pop es
	pop ds
	pop edi
	retf


;------------------------------------------------------------------------------
; Return information about the output device.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to buffer receiving mod_output_info structure
;    ES - Player instance segment
; <- DS:ESI - Filled with data
;------------------------------------------------------------------------------

	align 4

global mod_get_output_info
mod_get_output_info:
	push edi
	push ds
	push es

	push ds
	push es
	pop ds				; DS: player instance segment
	pop es
	mov edi, esi			; ES:EDI: mod_output_info buffer

	call [out_fn(get_info)]

	pop es
	pop ds
	pop edi
	retf


;==============================================================================
; Data area
;==============================================================================

		; Output device function tables

		alignb 2
out_devices	istruc mod_out_dev
		at mod_out_dev.none, dw mod_out_none_fns
		at mod_out_dev.dac, dw mod_out_dac_fns
		at mod_out_dev.sb, dw mod_out_sb_fns
		iend
