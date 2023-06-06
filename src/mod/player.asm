;==============================================================================
; MOD player - Public interface
;==============================================================================

cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/env_arg.inc"
%include "rtl/api/string.inc"
%include "rtl/api/log.inc"

%include "mod/config.inc"
%include "mod/api/convert.inc"
%include "mod/api/routine.inc"
%include "mod/structs/public.inc"
%include "mod/consts/public.inc"
%include "mod/structs/mod_file.inc"
%include "mod/structs/dev.inc"

; Shortcut macros for easier access to nested structures

%define mod(var) mod_file + mod. %+ var
%define	dev(fn) mod_dev + mod_dev_api. %+ fn
%define	sample(var) mod_sample. %+ var

; Player status

STATE_UNINIT	EQU 0
STATE_INIT	EQU 1
STATE_STOP	EQU 2
STATE_PLAY	EQU 3


;------------------------------------------------------------------------------
; Setup the MOD player.
;------------------------------------------------------------------------------
; -> AH - Output device (MOD_OUT_*)
;    AL - Output device type
;    EBX - Pointer to mod_dev_params structure
;    CH.CL - Amplification in 8.8-bit fixed point format *, **
;    EDX - Requested sample rate **
; <- CF - Set on error
;    EAX - Error code or actual sample rate
;------------------------------------------------------------------------------
; * Amplification is a range between 0.00 - 4.00, where 0.00 is silence and
; 4.00 is maximum (values above 4.00 are treated as 4.00). At 4.00 one channel
; can saturate the entire output, at 1.00 four channels are required to saturate
; the output sample range. Setting amplification to 4 / number of channels
; always avoids clipping, but reduces the useful output dynamic range for most
; MODs since they usually don't saturate the output range, especially
; multichannel MODs.
;------------------------------------------------------------------------------
; ** Some output devices may ignore this setting.
;------------------------------------------------------------------------------

global mod_setup
mod_setup:
	push ebx
	push edx
	push esi
	push edi
	push ecx

	cmp byte [state], STATE_UNINIT
	jne .error_state

	cld

	; Validate device and save its API jump table in mod_dev

	mov esi, mod_devs
	mov ecx, MOD_DEVS_COUNT

.find_dev_loop:
	cmp [esi], ah
	je .use_dev
	add esi, 5
	dec ecx
	jnz .find_dev_loop
	mov eax, MOD_ERR_DEV_UNK	; Unknown output device
	jmp .error

.use_dev:
	mov esi, [esi + 1]
	mov edi, mod_dev
	mov ecx, (mod_dev_api.strucsize + 3) / 4
	rep movsd

	; Setup output device

	mov ecx, [esp]			; Restore input ECX
	call [dev(setup)]
	jc .error
	mov [extra_samples], ebx
	mov [sample_padding], ecx

	; Initialize defaults

	mov byte [state], STATE_INIT

	clc

.exit:
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

.error_state:
	mov eax, MOD_ERR_STATE

.error:
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Load a MOD file into memory.
;------------------------------------------------------------------------------
; -> EBX - Handle of MOD file opened for read
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global mod_load
mod_load:
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push eax

	cmp byte [state], STATE_INIT
	jne .error_state

	mov [file_handle], ebx		; Save file handle
	call clear_mod_file

	; Load MOD header

	call load_mod_header
	jc .error

	; Read samples from MOD file

	mov ebp, [extra_samples]
	add ebp, [sample_padding]
	mov esi, mod(sample_hdr)
	mov dl, [mod(num_samples)]
	test dl, dl
	jz .done

	log LOG_DEBUG, {'Wavetable requires {u} bytes of extra memory for each sample', 13, 10, 'Loading samples', 13, 10}, ebp

.loop_sample:
	mov ecx, [esi + sample(length)]
	test ecx, ecx			; Skip empty samples
	jz .next_sample

	; Allocate memory

	add ecx, ebp
	mov al, PMI_MEM_HI_LO
	call pmi(mem_alloc)
	jc .error
	mov [esi + sample(addr)], eax

	log LOG_DEBUG, {'Allocated {u} bytes at 0x{X} for sample "{s}"', 13, 10}, ecx, eax, esi

	; Read from file into allocated memory

	sub ecx, ebp
	mov ebx, [file_handle]
	mov edi, eax
	add edi, [sample_padding]	; Load after padding
	call pmi(file_read)
	jc .error

	cmp eax, ecx			; Check end of file
	je .upload_sample
	sub ecx, eax
	cmp ecx, 16
	jbe .pad_sample_end
	mov eax, MOD_ERR_INVALID	; >16 samples missing, bail out
	jmp .error

.pad_sample_end:
	push edi			; Pad missing data with zeroes
	add edi, eax
	xor al, al
	rep stosb
	pop edi

.upload_sample:
	call [dev(upload_sample)]
	jc .error
	mov [esi + sample(wt_id)], eax

	; Release memory if wavetable has own sample RAM

	test dh, dh
	jz .next_sample
	mov eax, edi

	log LOG_DEBUG, {'Disposing sample at 0x{X}', 13, 10}, eax

	call pmi(mem_free)
	mov dword [esi + sample(addr)], 0

.next_sample:
	add esi, mod_sample.strucsize	; Advance sample header pointer
	dec dl				; Next sample
	jnz .loop_sample

.done:
	mov byte [state], STATE_STOP

	pop eax
	clc

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

.error_state:
	mov eax, MOD_ERR_STATE

.error:
	push eax
	call mod_unload			; Free allocated memory
	pop eax
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Erase all stored MOD file related data.
;------------------------------------------------------------------------------
; <- Destroys EAX, ECX and EDI
;------------------------------------------------------------------------------

clear_mod_file:
	cld

	xor eax, eax
	mov edi, mod_file
	mov ecx, (mod.strucsize + 3) / 4
	rep stosd
	ret


;------------------------------------------------------------------------------
; Load header and patterns from the MOD file. The file pointer will point to
; the start of samples. file_handle must contain the handle of the MOD file.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if cannot be loaded
;------------------------------------------------------------------------------

load_mod_header:
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push eax

	cld

	log LOG_INFO, {'Loading MOD file', 13, 10}

	; Read MOD header into I/O buffer temporarily
	; TODO: Add format detection and support for other formats!

	log LOG_DEBUG, {'Loading MOD header', 13, 10}

	call pmi(file_get_buf)
	mov edi, eax			; EDI: linear address of temp buffer
	mov ecx, 1084
	call pmi(file_read)
	jc .error
	cmp eax, ecx			; Check end of file
	je .convert_header
	mov eax, MOD_ERR_INVALID
	jmp .error

	;----------------------------------------------------------------------
	; Convert MOD header to internal data structure

.convert_header:

	; Copy title as ASCIIZ

	mov esi, edi			; ESI: raw MOD header
	mov edi, mod(title)		; EDI: MOD title
	mov ecx, 5
	rep movsd
	mov byte [edi], 0		; Extra zero for ASCIIZ
	lea edi, [esi - 5 * 4]		; EDI: raw MOD header

	log LOG_DEBUG, {'Song title: {s}', 13, 10}, mod(title)

	; Number of channels
	; TODO: Add support for even more formats

	mov ebx, [edi + 1080]		; EBX: MOD format ID

	; 4-channel variants

	mov al, 4
	cmp ebx, 'M.K.'
	je .set_channels
	cmp ebx, 'M!K!'
	je .set_channels
	cmp ebx, 'FLT4'
	je .set_channels

	; 8-channel variants

	mov al, 8
	cmp ebx, 'OCTA'
	je .set_channels
	cmp ebx, 'CD81'
	je .set_channels

	; Any-channel (TDZn / nCHN / nnCH)

	mov al, 4			; Fallback to 4-channel on failure

	mov ecx, ebx			; TDZn
	and ecx, 0xffffff
	cmp ecx, 'TDZ'
	jne .check_anychannel_fmt
	shr ebx, 24
	sub bl, '0'
	mov al, bl
	jmp .set_channels

.check_anychannel_fmt:
	mov ecx, ebx			; nCHN
	and ecx, 0xffffff00
	cmp ecx, ('CHN' << 8)
	jne .check_10pluschannel_fmt
	mov al, bl
	sub al, '0'
	jmp .set_channels

.check_10pluschannel_fmt:
	mov ecx, ebx			; nnCH
	shr ecx, 16
	cmp cx, 'CH'
	jne .set_channels
	sub bh, '0'			; BH: number of channels lower digit
	sub bl, '0'			; BL: number of channels upper digit
	add bl, bl
	mov al, bl
	shl bl, 2
	add al, bl			; AL: upper digit * 10
	add al, bh			; AL: number of channels

.set_channels:
	mov [mod(num_channels)], al

	log LOG_DEBUG, {'Number of channels: {u8}', 13, 10}, [mod(num_channels)]

	cmp al, MOD_MAX_CHANS
	jbe .check_min_channels

.channel_num_error:
	mov eax, MOD_ERR_NB_CHN		; Too many or no channels
	jmp .error

.check_min_channels:
	cmp byte [mod(num_channels)], 0
	je .channel_num_error

	; Number of samples
	; TODO: Add support for Soundtracker format

	mov al, 31
	mov [mod(num_samples)], al

	log LOG_DEBUG, {'Max. number of samples: {u8}', 13, 10}, [mod(num_samples)]

	; Song length (number of entries in mod.sequence)

	mov al, [edi + 950]
	mov [mod(length)], al

	log LOG_DEBUG, {'Song length: {u8} patterns', 13, 10}, [mod(length)]

	; BPM

	mov al, 125			; Protracker default BPM
	mov [mod(bpm)], al

	; Song restart
	; TODO: Implement (Noisetracker only)

	mov byte [mod(restart_pos)], 0

	log LOG_DEBUG, {'Restart position: {u8}', 13, 10}, [mod(restart_pos)]

	; Copy pattern sequence (positions)

	mov esi, mod(sequence)
	xor ebx, ebx
	mov ecx, 128
	xor ah, ah

	log LOG_DEBUG, {'Copying pattern sequence to 0x{X}', 13, 10}, esi

.loop_seq_pos:
	mov al, [edi + ebx + 952]
	cmp al, ah
	jbe .copy_seq_pos
	mov ah, al

.copy_seq_pos:
	mov [esi + ebx], al
	inc ebx
	dec ecx
	jnz .loop_seq_pos

	; Got number of patterns in AH; it's the largest pattern number in the
	; sequence + 1

	inc ah
	mov [mod(num_patterns)], ah

	log LOG_DEBUG, {'Number of patterns: {u8}', 13, 10}, [mod(num_patterns)]

	; Convert sample headers
	; TODO: Support Soundtracker (15 samples only)

	add edi, 20
	mov esi, mod(sample_hdr)
	mov ebx, mod(sample_hdr_ofs)
	mov cl, [mod(num_samples)]
	test cl, cl
	jz .read_patterns

	log LOG_DEBUG, {'Copying {u8} sample headers to 0x{X}, ptr[] at 0x{X}', 13, 10}, cl, esi, ebx

	; EDI - Sample binary structure in module
	; ESI - mod.sample_hdr[]
	; EBX - mod.sample_hdr_ofs[]
	; CL - Number of sample headers to convert

.loop_sample_hdr:
	mov [ebx], esi			; Save sample data offset

	; Sample name

	mov edx, 10

.loop_sample_name:
	mov ax, [edi + edx * 2]
	mov [esi + sample(name) + edx * 2], ax
	dec edx
	jns .loop_sample_name
	mov byte [esi + sample(name) + 22], 0

	; Sample length

	xor eax, eax
	mov ax, [edi + 22]
	xchg al, ah			; Swap byte order
	add eax, eax			; Convert to bytes
	cmp eax, 2
	ja .save_sample_length
	xor eax, eax

.save_sample_length:
	mov [esi + sample(length)], eax

	; Finetune

	mov al, [edi + 24]
	shl al, 4
	sar al, 4			; Extend to signed small int
	mov [esi + sample(finetune)], al

	; Volume

	mov al, [edi + 25]
	cmp al, 64			; Clamp to 64
	jbe .save_volume
	mov al, 64

.save_volume:
	mov [esi + sample(volume)], al

	; Repeat start, disable repeat if > sample length

	xor eax, eax
	mov ax, [edi + 26]
	xchg al, ah			; Swap byte order
	add eax, eax			; Convert to bytes
	cmp eax, [esi + sample(length)]
	ja .disable_repeat
	mov [esi + sample(rpt_start)], eax
	jmp .rpt_len

.disable_repeat:
	mov dword [esi + sample(rpt_start)], 0
	mov dword [esi + sample(rpt_len)], 0
	jmp .next_sample_hdr

.rpt_len:

	; Repeat length

	xor eax, eax
	mov ax, [edi + 28]
	xchg al, ah			; Swap byte order
	cmp eax, 1			; Repeat length <= 1: no repeat
	ja .chk_rpt_len
	xor eax, eax

.chk_rpt_len:
	add eax, eax			; Convert to bytes
	mov edx, [esi + sample(rpt_start)]
	add edx, eax
	cmp edx, [esi + sample(length)]
	jbe .save_rpt_len

.clamp_rpt_len:
	mov eax, [esi + sample(length)]	; Limit repeat length to sample length
	sub eax, [esi + sample(rpt_start)]

.save_rpt_len:
	mov [esi + sample(rpt_len)], eax

.next_sample_hdr:
	log LOG_DEBUG, {'Sample: "{s}", size: {u}, ft: {i8}, vol: {u8}, rpt: {u} + {u}', 13, 10}, esi, [esi + sample(length)], [esi + sample(finetune)], [esi + sample(volume)], [esi + sample(rpt_start)], [esi + sample(rpt_len)]

	add esi, mod_sample.strucsize	; Next sample header
	add edi, 30			; Raw MOD sample header size
	add ebx, 4			; Next sample header offset table entry
	dec cl
	jnz .loop_sample_hdr

.read_patterns:

	;----------------------------------------------------------------------
	; Read patterns from MOD file

	; Allocate memory for patterns, each pattern uses 256 bytes per channel

	xor eax, eax
	xor ecx, ecx
	mov al, [mod(num_channels)]
	mov cl, [mod(num_patterns)]
	imul ecx, eax
	shl ecx, 8			; Patterns * channels * 256

	log LOG_DEBUG, {'Allocating {u} bytes of pattern memory', 13, 10}, ecx

	mov al, PMI_MEM_HI_LO
	call pmi(mem_alloc)
	jc .error

	log LOG_DEBUG, {'Allocated pattern memory at 0x{X}, loading patterns', 13, 10}, eax

	; Load patterns into allocated data area

	mov edi, eax
	mov ebx, [file_handle]
	call pmi(file_read)
	jc .error
	cmp eax, ecx
	je .set_pattern_pointers
	mov eax, MOD_ERR_INVALID
	jmp .error

.set_pattern_pointers:

	; Setup pattern data pointer table

	mov esi, mod(pattern_addr)
	xor edx, edx
	mov dh, [mod(num_channels)]	; EDX: pattern size = channels * 256
	mov cl, [mod(num_patterns)]	; CL: number of patterns

	log LOG_DEBUG, {'Setting relative pattern pointers at 0x{X}', 13, 10}, esi

.loop_pattern_addr:
	mov [esi], edi
	add edi, edx
	add esi, 4
	dec cl
	jnz .loop_pattern_addr

	log LOG_DEBUG, {'Converting ProTracker note structure', 13, 10}

	; Replace ProTracker 4-byte note structure for faster replay with the
	; following:
	; [0]: instrument number (0 - 31)
	; [1]: note (0 - 83, +12 / octave,  24 = C-1; lowest ProTracker note)
	; [2]: effect command (0x00 - 0x0f, 0xe0 - 0xef)
	; [3]: effect parameter

	movzx esi, byte [mod(num_channels)]
	xor edx, edx
	mov dl, [mod(num_patterns)]
	imul esi, edx
	shl esi, 6			; ESI: Number of note structures in MOD
	mov edi, [mod(pattern_addr)]	; EDI: Patterns
	xor ebp, ebp			; EBP: MOD flags

	align 16

.loop_note_convert:
	mov ecx, [edi]
	shld edx, ecx, 16		; DH: Effect parameter
	xchg cl, ch
	mov bl, dl
	shr bl, 4
	and dl, 0x0f			; DL: Effect command
	mov bh, ch
	and bh, 0xf0
	or bl, bh			; BL: Instrument number
	and ch, 0x0f			; CX: Note period
	cmp dl, 0x0e			; Adjust DH and DL for extra commands
	jne .convert_period
	mov ah, dh
	and ah, 0xf0
	or dl, ah
	rol dl, 4
	and dh, 0x0f

.convert_period:
	mov al, bl			; AL: instrument number
	xor ah, ah
	test cx, cx
	jz .no_period
	call mod_convert_period_to_note	; BX: note
	mov ah, bl			; AH: note

.no_period:
	cmp dl, 0x08			; Analyze note structure for MOD flags
	je .has_pan
	cmp dl, 0xe8
	je .has_pan
	test cx, cx
	jz .save_note
	cmp cx, 856
	ja .ext_octave
	cmp cx, 113
	jb .ext_octave

.save_note:
	shl edx, 16
	mov dx, ax
	mov [edi], edx
	add edi, 4
	dec esi
	jnz .loop_note_convert
	mov [mod(flags)], ebp
	jmp .done

.has_pan:
	or ebp, MOD_FLG_PAN
	jmp .save_note

.ext_octave:
	or ebp, MOD_FLG_EXT_OCT
	jmp .save_note

.done:
	pop eax				; Restore caller AX
	clc

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret

.error:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Unload MOD file from memory.
;------------------------------------------------------------------------------
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global mod_unload
mod_unload:
	push ecx
	push edx
	push esi
	push edi
	push eax

	; Stop playback if still playing

	cmp byte [state], STATE_PLAY
	jne .unload
	call mod_stop

.unload:
	cmp byte [state], STATE_UNINIT
	je .error_state

	log LOG_INFO, {'Unloading MOD file', 13, 10}

	; Release mod_info buffer

	mov eax, [mod_info_addr]
	test eax, eax
	jz .free_patterns
	log LOG_DEBUG, {'Disposing MOD info structure at 0x{X}', 13, 10}, eax
	call pmi(mem_free)
	mov dword [mod_info_addr], 0

.free_patterns:

	; Release pattern data memory

	mov eax, [mod(pattern_addr)]
	test eax, eax
	jz .free_samples
	log LOG_DEBUG, {'Disposing patterns at 0x{X}', 13, 10}, eax
	call pmi(mem_free)
	mov dword [mod(pattern_addr)], 0

.free_samples:

	; Release sample memory

	mov esi, mod(sample_hdr)
	mov dl, [mod(num_samples)]
	test dl, dl
	jz .reset_data

.loop_sample:
	mov eax, [esi + sample(wt_id)]	; Free in wavetable
	call [dev(free_sample)]
	mov dword [esi + sample(wt_id)], 0

	mov eax, [esi + sample(addr)]	; Free in memory
	test eax, eax
	jz .next_sample
	log LOG_DEBUG, {'Disposing sample at 0x{X}', 13, 10}, eax
	call pmi(mem_free)
	mov dword [esi + sample(addr)], 0

.next_sample:
	add esi, mod_sample.strucsize	; Advance sample header pointer
	dec dl				; Next sample
	jnz .loop_sample

.reset_data:
	call clear_mod_file

	mov byte [state], STATE_INIT

	clc
	pop eax

.exit:
	pop edi
	pop esi
	pop edx
	pop ecx
	ret

.error_state:
	mov eax, MOD_ERR_STATE

.error:
	add esp, 4			; Discard EAX from stack
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown the MOD playback system.
;------------------------------------------------------------------------------
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global mod_shutdown
mod_shutdown:
	cmp byte [state], STATE_UNINIT
	je .done

	; Unload MOD file

	call mod_unload

	; Shutdown output device

	call [dev(shutdown)]

.done:
	clc
	ret


;------------------------------------------------------------------------------
; Set the amplification level.
;------------------------------------------------------------------------------
; -> AH.AL - Amplification in 8.8 fixed point value *
; <- AH.AL - Actual audio amplification level
;------------------------------------------------------------------------------
; * See notes in mod_setup.
;------------------------------------------------------------------------------

global mod_set_amplify
mod_set_amplify:
	cmp byte [state], STATE_UNINIT
	je .noop

	jmp [dev(set_amplify)]

.noop:
	ret


;------------------------------------------------------------------------------
; Set sample interpolation.
;------------------------------------------------------------------------------
; -> AL - Sample interpolation method (MOD_IPOL_*)
;------------------------------------------------------------------------------

global mod_set_interpolation
mod_set_interpolation:
	cmp byte [state], STATE_UNINIT
	je .noop

	jmp [dev(set_interpol)]

.noop:
	ret


;------------------------------------------------------------------------------
; Set stereo rendering mode.
;------------------------------------------------------------------------------
; -> AL - Stereo rendering mode (MOD_PAN_*)
;------------------------------------------------------------------------------

global mod_set_stereo_mode
mod_set_stereo_mode:
	cmp byte [state], STATE_UNINIT
	je .noop

	jmp [dev(set_stereomode)]

.noop:
	ret


;------------------------------------------------------------------------------
; Start playback.
;------------------------------------------------------------------------------
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global mod_play
mod_play:
	push ebx

	cmp byte [state], STATE_STOP
	je .play
	mov eax, MOD_ERR_STATE
	stc
	jmp .exit

.play:
	log LOG_INFO, {'Starting playback', 13, 10}

	; Start output device

	%ifdef MOD_USE_PROFILER
	mov dword [mod_perf_ticks], 0	; Reset performance tick counter
	%endif

	call mod_playroutine_init
	call [dev(play)]
	jc .exit

	mov byte [state], STATE_PLAY
	clc

.exit:
	pop ebx
	ret


;------------------------------------------------------------------------------
; Get the current position within the song.
;------------------------------------------------------------------------------
; <- AH - Sequence entry (starting with 0)
;    AL - Row within the sequence entry pattern (0 - 63)
;    DL - Current tick within the row
;------------------------------------------------------------------------------

global mod_get_position
mod_get_position:
	jmp mod_playroutine_get_position


;------------------------------------------------------------------------------
; Jump to a specific sequence entry within the song.
;------------------------------------------------------------------------------
; -> AH - New sequence entry (starting with 0)
;    AL - New row within the sequence entry pattern
;    DL - Set to 1 to stop currently playing samples
; <- AH - New sequence entry (starting with 0)
;    AL - New row within the sequence entry pattern (0 - 63)
;------------------------------------------------------------------------------

global mod_set_position
mod_set_position:
	jmp mod_playroutine_set_position


;------------------------------------------------------------------------------
; Stop playback.
;------------------------------------------------------------------------------
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

global mod_stop
mod_stop:
	push ebx

	cmp byte [state], STATE_PLAY
	je .stop
	mov eax, MOD_ERR_STATE
	stc
	jmp .exit

.stop:
	log LOG_INFO, {'Stopping playback', 13, 10}

	; Stop output device

	call [dev(stop)]
	jc .exit

	mov byte [state], STATE_STOP
	clc

.exit:
	pop ebx
	ret


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

	align 4

global mod_render
mod_render:

	; Don't render if not playing

	cmp byte [state], STATE_PLAY
	jne .noop

	; Render audio into output buffer

	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	call [dev(render)]

	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax

.noop:
	ret


;------------------------------------------------------------------------------
; Copies data from mod structure into mod_info structure.
;------------------------------------------------------------------------------
; -> EDI - Pointer to mod_info structure
;    %1 - Structure member to copy
;    %2 - Temporary register used to copy data
; <- %2 - Contains structure member value
;------------------------------------------------------------------------------

%macro	copy_mod_info 2

	mov %2, [mod(%1)]
	mov [edi + mod_info.%1], %2

%endmacro


;------------------------------------------------------------------------------
; Copies data from mod_sample structure into mod_sample_info structure.
;------------------------------------------------------------------------------
; -> ESI - Pointer to mod_sample structure
;    EDI - Pointer to mod_sample_info structure
;    %1 - Structure member to copy
;    %2 - Temporary register used to copy data
; <- %2 - Contains structure member value
;------------------------------------------------------------------------------

%macro	copy_sample_info 2

	mov %2, [esi + sample(%1)]
	mov [edi + mod_sample_info.%1], %2

%endmacro


;------------------------------------------------------------------------------
; Returns a memory buffer filled with information of the loaded MOD.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Linear address of mod_info structure or error code if CF set
;------------------------------------------------------------------------------

global mod_get_info
mod_get_info:
	cmp byte [state], STATE_STOP	; No info until MOD loaded
	jae .get_info
	mov eax, MOD_ERR_STATE
	stc
	ret

.get_info:
	mov eax, [mod_info_addr]	; Return already retrieved info
	test eax, eax
	jz .init_info
	ret

.init_info:
	push ebx
	push ecx
	push edx
	push esi
	push edi

	cld

	; Allocate memory for mod_info structure

	mov al, PMI_MEM_HI_LO
	xor ecx, ecx
	mov cl, [mod(num_samples)]
	imul ecx, mod_sample_info.strucsize
	add ecx, mod_info.strucsize	; ECX: memory needed for mod_info
	call pmi(mem_alloc)
	jc .exit
	mov [mod_info_addr], eax
	mov edx, eax			; EDX: mod_info linear address

	log LOG_DEBUG, {'Allocated {u} bytes for MOD info structure at 0x{X}', 13, 10}, ecx, edx

	; Copy header info

	mov esi, mod(title)		; Title
	mov ecx, 21
	lea edi, [edx + mod_info.title]
	rep movsb

	mov edi, eax
	copy_mod_info num_channels, al	; Number of samples
	copy_mod_info num_samples, al	; Number of channels
	copy_mod_info length, al	; Song length
	copy_mod_info num_patterns, al	; Number of patterns
	copy_mod_info restart_pos, al	; Restart position
	copy_mod_info flags, eax	; MOD flags
	copy_mod_info pattern_addr, eax	; Linear address of pattern data

	; Copy sample info

	mov bl, [mod(num_samples)]
	mov esi, mod(sample_hdr)
	lea edi, [edx + mod_info.samples]

.sample_info_loop:
	mov ecx, 23			; Sample name
	rep movsb
	sub esi, 23
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
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	ret


;------------------------------------------------------------------------------
; Return information about channels.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_channel_info structures
; <- CF - Set if error
;    EAX - Error code if CF set
;    ESI - Filled with data
;------------------------------------------------------------------------------

	align 4

global mod_get_channel_info
mod_get_channel_info:
	cmp byte [state], STATE_PLAY	; No info when not playing
	je .get_info
	mov eax, MOD_ERR_STATE
	stc
	ret

.get_info:
	push ebx

	mov bl, [mod(num_channels)]
	call mod_playroutine_get_channel_info
	call [dev(get_mixer_info)]

	pop ebx
	ret


;------------------------------------------------------------------------------
; Return information about the output device.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_output_info structure
; <- CF - Set if error
;    EAX - Error code if CF set
;    ESI - Filled with data
;------------------------------------------------------------------------------

	align 4

global mod_get_output_info
mod_get_output_info:
	cmp byte [state], STATE_PLAY	; No info when not playing
	je .get_info
	mov eax, MOD_ERR_STATE
	stc
	ret

.get_info:
	call [dev(get_info)]

	ret


;------------------------------------------------------------------------------
; Return current playback position of the MOD file.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_position_info structure
; <- CF - Set if error
;    EAX - Error code if CF set
;    ESI - Filled with data
;------------------------------------------------------------------------------

global mod_get_position_info
mod_get_position_info:
	cmp byte [state], STATE_PLAY	; No info when not playing
	je .get_info
	mov eax, MOD_ERR_STATE
	stc
	ret

.get_info:
	call [dev(get_position)]

	ret


;==============================================================================
; Data area
;==============================================================================

section .data

		; Output device API jump tables

		extern mod_dev_none_api
		extern mod_dev_dac_api
		extern mod_dev_sb_api

mod_devs	db MOD_OUT_NONE
		dd mod_dev_none_api
		db MOD_OUT_DAC
		dd mod_dev_dac_api
		db MOD_OUT_SB
		dd mod_dev_sb_api
		MOD_DEVS_COUNT EQU ($ - mod_devs) / 5

mod_info_addr	dd 0			; Address of mod_info structure buffer

state		db STATE_UNINIT		; Player status (STATE_*)

section .bss

global mod_dev				; Output device API jump table
mod_dev		resd (mod_dev_api.strucsize + 3) / 4

global mod_file				; MOD file data
mod_file	resd (mod.strucsize + 3) / 4

file_handle	resd 1			; File handle of MOD being loaded
extra_samples	resd 1			; Number of extra samples at end
sample_padding	resd 1			; Number of extra samples at start

%ifdef MOD_USE_PROFILER
global mod_perf_ticks
mod_perf_ticks	resd 1			; Performance counter ticks used
%endif
