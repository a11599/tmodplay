;==============================================================================
; MOD player - module file loader
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "mod/consts/public.inc"
%include "mod/consts/global.inc"
%include "mod/structs/global.inc"
%include "mod/api/convert.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	file_fn(fn) mod.file_fns + mod_file_fns. %+ fn
%define	sample(var) mod_sample. %+ var

segment modplayer public use16 class=CODE align=16
segment modplayer


;------------------------------------------------------------------------------
; Load header and patterns from the MOD file. The file pointer will point to
; the start of samples and the file will remain open when the function returns
; successfully. Requires mod.file_handle to point to a file opened for read.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- CF - Set if error
;    EAX - Error code if cannot be loaded
;------------------------------------------------------------------------------

global mod_load_header
mod_load_header:
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push ds
	push es
	push eax

	cld

	; ---------------------------------------------------------------------
	; Read MOD header into temporary buffer

	; Allocate temporary buffer for MOD header

	mov al, SYS_MEM_LO
	mov ebx, 1084
	call far sys_mem_alloc
	jc .malloc_error
	mov edi, eax			; EDI: linear address for file_read
	shr eax, 4
	mov es, ax			; ES: temporary buffer with MOD header

	log {'Temporary MOD header buffer allocated @{X16}:0000, loading MOD header', 13, 10}, es

	; Read MOD header from file
	; TODO: Add format detection and support for other formats!

	mov ebx, [mod.file_handle]
	mov ecx, 1084
	call far [file_fn(read)]
	jc .error

	cmp eax, ecx			; Check end of file
	je .convert_header
	mov eax, MOD_ERR_INVALID
	jmp .error

	; ---------------------------------------------------------------------
	; Convert MOD header to internal data structure

.convert_header:

	; Copy title as ASCIIZ

	xor di, di
	mov si, mod.title
	xor bx, bx
	mov cx, 10

.loop_title:
	mov ax, es:[di + bx]
	mov [si + bx], ax
	add bx, 2
	loop .loop_title, cx
	mov byte [mod.title + 20], 0	; Extra zero for ASCIIZ

	log {'Song title: {s}', 13, 10}, ds, mod.title

	; Number of channels
	; TODO: Add support for even more formats

	mov ebx, es:[1080]		; EBX: MOD format ID

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

	mov al, 4			; Fallback to 4-channel if below fails

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
	mov [mod.num_channels], al

	log {'Number of channels: {u8}', 13, 10}, [mod.num_channels]

	cmp al, MOD_MAX_CHANS
	jbe .check_min_channels

.channel_num_error:
	mov eax, MOD_ERR_NB_CHN		; Too many or no channels
	jmp .error

.check_min_channels:
	cmp byte [mod.num_channels], 0
	je .channel_num_error

	; Number of samples
	; TODO: Add support for Soundtracker format

	mov al, 31
	mov [mod.num_samples], al

	log {'Max. number of samples: {u8}', 13, 10}, [mod.num_samples]

	; Song length (number of entries in mod.sequence)

	mov al, es:[950]
	mov [mod.length], al

	log {'Song length: {u8} patterns', 13, 10}, [mod.length]

	; BPM

	mov al, 125			; Protracker default BPM
	mov [mod.bpm], al

	; Song restart
	; TODO: Implement (Noisetracker only)

	mov byte [mod.restart_pos], 0

	log {'Restart position: {u8}', 13, 10}, [mod.restart_pos]

	; Copy pattern sequence (positions)

	mov di, 952
	mov si, mod.sequence
	xor bx, bx
	mov cx, 128
	xor ah, ah

	log {'Copying pattern sequence @{X16}:{X16} to {X16}:{X16}', 13, 10}, es, di, ds, si

.loop_seq_pos:
	mov al, es:[di + bx]
	cmp al, ah
	jle .copy_seq_pos
	mov ah, al

.copy_seq_pos:
	mov [si + bx], al
	inc bx
	loop .loop_seq_pos, cx

	; Got number of patterns in AH; it's the largest pattern number in the
	; sequence + 1

	inc ah
	mov [mod.num_patterns], ah

	log {'Number of patterns: {u8}', 13, 10}, [mod.num_patterns]

	; Convert sample headers
	; TODO: Support Soundtracker (15 samples only)

	mov di, 20
	mov esi, mod.sample_hdr
	mov bx, mod.sample_hdr_ofs
	movzx cx, byte [mod.num_samples]
	test cx, cx
	jz .dealloc_header

	log {'Copying {u16} sample headers @{X16}:{X16} to {X16}:{X16}, ptr[] @{X16}:{X16}', 13, 10}, cx, es, di, ds, si, ds, bx

	; ES:DI - Sample binary structure in module
	; DS:SI - mod.sample_hdr[]
	; DS:BX - mod.sample_hdr_ofs[]
	; CX - Number of sample headers to convert

.loop_sample_hdr:
	mov [bx], si			; Save sample data offset

	; Sample name

	push ebx
	push ecx

	mov cx, 11
	xor bx, bx

.loop_sample_name:
	mov ax, es:[di + bx]
	mov [si + sample(name) + bx], ax
	add bx, 2
	loop .loop_sample_name, cx
	mov byte [si + sample(name) + 22], 0

	pop ecx
	pop ebx

	; Sample length

	movzx eax, word es:[di + 22]
	xchg al, ah
	add eax, eax			; Convert to bytes
	cmp eax, 2
	ja .save_sample_length
	xor eax, eax

.save_sample_length:
	mov [si + sample(length)], eax

	; Finetune

	mov al, es:[di + 24]
	and al, 0x0f			; Mask upper bits, just in case...
	test al, 0x08			; Extend to signed small int
	jz .save_finetune
	or al, 0xf0

.save_finetune:
	mov [si + sample(finetune)], al

	; Volume

	mov al, es:[di + 25]
	cmp al, 64			; Clamp to 64
	jbe .save_volume
	mov al, 64

.save_volume:
	mov [si + sample(volume)], al

	; Repeat start, disable repeat if > sample length

	movzx eax, word es:[di + 26]
	xchg al, ah
	add eax, eax			; Convert to bytes
	cmp eax, [si + sample(length)]
	ja .disable_repeat
	mov [si + sample(rpt_start)], eax
	jmp .rpt_len

.disable_repeat:
	mov dword [si + sample(rpt_start)], 0
	mov dword [si + sample(rpt_len)], 0
	jmp .next_sample_hdr

.rpt_len:

	; Repeat length

	movzx eax, word es:[di + 28]
	xchg al, ah
	cmp ax, 1			; Repeat length <= 1: no repeat
	ja .chk_rpt_len
	xor ax, ax

.chk_rpt_len:
	add eax, eax			; Convert to bytes
	mov edx, [si + sample(rpt_start)]
	add edx, eax
	cmp edx, [si + sample(length)]
	jbe .save_rpt_len

.clamp_rpt_len:
	mov eax, [si + sample(length)]	; Limit repeat length to sample length
	sub eax, [si + sample(rpt_start)]

.save_rpt_len:
	mov [si + sample(rpt_len)], eax

.next_sample_hdr:
	log {'Sample: "{s}", size: {u}, ft: {i8}, vol: {X8}, rpt: {u}+{u}', 13, 10}, ds, esi, [si + sample(length)], [si + sample(finetune)], [si + sample(volume)], [si + sample(rpt_start)], [si + sample(rpt_len)]

	add si, mod_sample.strucsize	; Next sample header
	add di, 30			; MOD sample header size
	add bx, 2			; Next sample header offset table entry
	dec cx
	jnz .loop_sample_hdr

.dealloc_header:

	; Dispose temporary MOD header buffer

	xor eax, eax
	mov ax, es
	shl eax, 4
	call far sys_mem_free
	jc .error

	log {'Temporary MOD header disposed', 13, 10}

	; ---------------------------------------------------------------------
	; Read patterns from MOD file

	; Allocate memory for patterns, each pattern uses 256 bytes per channel

	movzx eax, byte [mod.num_channels]
	movzx ebx, byte [mod.num_patterns]
	imul ebx, eax
	shl ebx, 8			; Patterns * channels * 256

	log {'Allocating {u} bytes of pattern memory', 13, 10}, ebx

	mov al, SYS_MEM_HI_LO
	call far sys_mem_alloc
	jc .malloc_error

	log {'Allocated pattern memory @{X32}, loading patterns', 13, 10}, eax

	; Load patterns into allocated data area

	mov edi, eax
	mov ecx, ebx
	mov ebx, [mod.file_handle]
	call far [file_fn(read)]
	jc .error
	cmp eax, ecx
	je .set_pattern_pointers
	mov eax, MOD_ERR_INVALID
	jmp .error

.set_pattern_pointers:

	; Setup pattern data pointer table

	mov si, mod.pattern_addr
	mov eax, edi
	sub eax, [mod.instance_addr]	; EAX: pattern data relative address
	movzx edx, byte [mod.num_channels]
	shl edx, 8			; EDX: pattern size = channels * 256
	movzx cx, [mod.num_patterns]	; CX: number of patterns

	log {'Setting relative pattern pointers @{X16}:{X16}', 13, 10}, ds, si

.loop_pattern_addr:
	mov [si], eax

	add eax, edx
	add si, 4
	loop .loop_pattern_addr, cx

	log {'Converting ProTracker note structure', 13, 10}

	; Replace ProTracker 4-byte note structure for faster replay with the
	; following:
	; [0]: instrument number (0 - 31)
	; [1]: note (0 - 83, +12 / octave,  24 = C-1; lowest ProTracker note)
	; [2]: effect command (0x00 - 0x0f, 0xe0 - 0xef)
	; [3]: effect parameter

	movzx esi, byte [mod.num_channels]
	movzx edx, byte [mod.num_patterns]
	imul esi, edx
	shl esi, 6
	mov edi, [mod.pattern_addr]
	xor ebp, ebp			; EBP: MOD flags

.loop_note_convert:
	mov cx, [edi]
	xchg cl, ch
	mov dx, [edi + 2]		; DH: Effect parameter
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
	cmp dl, 0x08
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
	mov [edi], ax
	mov [edi + 2], dx

	add edi, 4
	dec esi
	jnz .loop_note_convert
	mov [mod.flags], ebp
	jmp .done

.has_pan:
	or ebp, MOD_FLG_PAN
	jmp .save_note

.ext_octave:
	or ebp, MOD_FLG_EXT_OCT
	jmp .save_note

.done:

	; Done, return to caller

	pop eax				; Restore caller AX
	clc
	jmp .exit

.malloc_error:
	mov eax, 0x08

.error:
	add sp, 4			; Discard EAX from stack
	stc

.exit:
	pop es
	pop ds
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	retn
