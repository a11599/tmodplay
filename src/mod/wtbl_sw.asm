;==============================================================================
; MOD player - software wavetable
;------------------------------------------------------------------------------
; Implements software mixing up to 32 channels. The aim is to replicate
; Gravis Ultrasound-like capabilities in software to the extent required by
; MOD playback.
;------------------------------------------------------------------------------
; 86Box 386DX clocks used with no output clipping at 44100 Hz samplerate:
;
; Stereo mode   Interpolation   MHz per channel for rendering    MHz for mixing
; -----------   -------------   -----------------------------    --------------
; Mono          Nearest                                  1.92              1.89
; Mono          Linear                                   3.56              1.89
; Mono          Watte                                   10.50              1.89
; Hard          Nearest                                  1.96              2.55
; Hard          Linear                                   3.65              2.55
; Hard          Watte                                   11.05              2.55
; Cross         Nearest                                  1.96              4.67
; Cross         Linear                                   3.68              4.67
; Cross         Watte                                   11.16              4.67
; Real          Nearest                                  3.17              2.55
; Real          Linear                                   4.68              2.55
; Real          Watte                                   13.76              2.55
;
; Performance scales linearly with sample rate.
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "mod/structs/global.inc"
%include "mod/consts/out.inc"
%include "mod/consts/wtbl_sw.inc"
%include "mod/structs/wtbl_sw.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	state(var) mod.wt + mod_wtbl_sw. %+ var
%define	sample(var) mod_sample. %+ var

segment modplayer public use16 class=CODE align=16
segment modplayer


;------------------------------------------------------------------------------
; Setup the wavetable mixer.
;------------------------------------------------------------------------------
; -> AL - Interpolation mode (MOD_IPOL_*)
;    AH - Stereo rendering mode (MOD_PAN_*)
;    BH.BL - Amplification as 8.8 bit fixed point value
;    CX - Size of the software wavetable render buffer
;    DL - Output device bitstream format (FMT_* flags)
;    DH - Initial pan for real stereo mixing
;    DS - Player instance segment
; <- CF - Set on error
;    EAX - Error code if CF set or number of extra samples that will be
;          generated at the end of each sample (must reserve enough space)
;    ECX - Number of extra samples that will be generated at the beginning of
;          each sample (must reserve enough space)
;------------------------------------------------------------------------------

	align 4

global mod_swt_setup
mod_swt_setup:
	push ebx

	; ---------------------------------------------------------------------
	; Initialize wavetable instance

	call mod_swt_set_interpolation

	mov [state(output_format)], dl
	mov [state(amplify)], bx

	mov al, ah
	call mod_swt_set_stereo_mode

	push ecx

	; Channel structure offset pointers

	mov ax, state(channels)		; AX: start of channel[]
	mov bx, state(channel_ofs)	; DS:BX: channel index table entry
	mov cx, MOD_MAX_CHANS

	log {'Creating channel offset ptr[] @{X16}:{X16}', 13, 10}, ds, bx

.loop_channel_ofs:
	mov [bx], ax			; AX: channel data structure offset
	add bx, 2
	add ax, channel.strucsize	; Advance offset
	dec cx
	jnz .loop_channel_ofs

	pop ecx

	; Reset channels

	call mod_swt_reset_channels

	; Allocate memory for volume lookup table - keep in low memory so we
	; can use direct offset reference to a segment address

	log {'Allocating 33280 bytes for software wavetable volume table', 13, 10}

	mov ebx, 65 * 256 * 2		; 1 word for 65 vols, 256 samples each
	mov al, SYS_MEM_LO
	call far sys_mem_alloc
	jc .error
	shr eax, 4
	mov [state(voltab_seg)], ax

	log {'Volume table allocated @{X16}:0000', 13, 10}, ax

	; Allocate memory for linear interpolation lookup table - keep in low
	; memory so we can use direct offset reference to a segment address

	mov ebx, 1024 << LIN_IPOL_EXP	; 512 words for each step

	log {'Allocating {u} bytes for software wavetable linear interpolation lookup table', 13, 10}, ebx

	mov al, SYS_MEM_LO
	call far sys_mem_alloc
	jc .error
	shr eax, 4
	mov [state(ipoltab_seg)], ax

	log {'Software wavetable linear interpolation lookup table allocated @{X16}:0000', 13, 10}, ax

	; Allocate memory for render buffer

	movzx ebx, cx
	shl ebx, 3			; 8 bytes / buffer entry (32-bit stereo)
	jz .init_voltab

	log {'Allocating {u} bytes for software wavetable render buffer', 13, 10}, ebx

	mov al, SYS_MEM_HI_LO
	call far sys_mem_alloc
	jc .error
	mov [state(buffer_addr)], eax

	log {'Render buffer allocated @{X}', 13, 10}, eax

.init_voltab:

	; Initialize volume lookup table

	call mod_swt_init_voltab

	; Initialize linear interpolation lookup table

	call mod_swt_init_ipoltab

	; Ready, return number of bytes required after each sample

	log {'Software wavetable initialized', 13, 10}

	mov eax, UNROLL_SAMPLES
	mov ecx, SAMPLE_PADDING
	clc
	jmp .exit

.error:
	stc

.exit:
	pop ebx
	retn


;------------------------------------------------------------------------------
; Shuts down the software wavetable.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_swt_shutdown
mod_swt_shutdown:
	push eax

	movzx eax, word [state(voltab_seg)]
	test eax, eax
	jz .free_ipoltab

	log {'Disposing software wavetable volume table @{X16}:0000', 13, 10}, ax

	shl eax, 4
	call far sys_mem_free
	mov word [state(voltab_seg)], 0

.free_ipoltab:
	movzx eax, word [state(ipoltab_seg)]
	test eax, eax
	jz .free_buffer

	log {'Disposing software wavetable linear interpolation lookup table @{X16}:0000', 13, 10}, ax

	shl eax, 4
	call far sys_mem_free
	mov word [state(ipoltab_seg)], 0

.free_buffer:
	mov eax, [state(buffer_addr)]
	test eax, eax
	jz .done

	log {'Disposing software wavetable render buffer @{X}', 13, 10}, eax

	call far sys_mem_free
	mov dword [state(buffer_addr)], 0

.done:
	pop eax
	retn


;------------------------------------------------------------------------------
; Set the amplification level.
;------------------------------------------------------------------------------
; -> AH.AL - Amplification in 8.8 fixed point value
;    DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_swt_set_amplify
mod_swt_set_amplify:
	cmp ax, [state(amplify)]
	je .exit

	mov [state(amplify)], ax
	call mod_swt_init_voltab

.exit:
	retn


;------------------------------------------------------------------------------
; Set sample interpolation.
;------------------------------------------------------------------------------
; -> AL - Sample interpolation method (MOD_IPOL_*)
;    DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_swt_set_interpolation
mod_swt_set_interpolation:
	push bx
	push cx

	mov cx, render_store_fn
	mov bx, render_mix_fn
	cmp al, 1
	jb .set_render_fns
	mov cx, renlin_store_fn
	mov bx, renlin_mix_fn
	je .set_render_fns
	mov cx, renwtl_store_fn
	mov bx, renwtl_mix_fn

.set_render_fns:
	mov cs:[render_store_fn_addr], cx
	mov cs:[render_direct_store_fn_addr], cx
	mov cs:[render_mix_fn_addr], bx
	mov cs:[render_direct_mix_fn_addr], bx
	add bx, RENDER_MIX_FN_LAST
	mov cs:[render_mix_fn_last_addr], bx
	mov cs:[render_direct_mix_fn_last_addr], bx

	pop cx
	pop bx
	retn


;------------------------------------------------------------------------------
; Set stereo rendering mode.
;------------------------------------------------------------------------------
; -> AL - Stereo rendering mode
;    DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_swt_set_stereo_mode
mod_swt_set_stereo_mode:
	push eax

	test byte [state(output_format)], FMT_STEREO
	jnz .set_pantab_base
	xor al, al

.set_pantab_base:
	movzx eax, al
	movzx eax, word cs:[modpantab + eax * 2]
	mov cs:[render_pantab_base], eax
	mov cs:[render_direct_pantab_base], eax

	pop eax
	retn


;------------------------------------------------------------------------------
; Initialize the renderer's volume lookup table.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

mod_swt_init_voltab:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push di
	push es

	cld

	mov es, [state(voltab_seg)]	; ES: volume table segment

	; Limit amplification to prevent overflow and excessive clipping.

	movzx ebx, word [state(amplify)]
	cmp bx, 0x0400
	jbe .init_voltab
	mov bx, 0x0400

.init_voltab:
	xor di, di			; ES:DI: volume table

	log {'Initializing volume table @{X16}:0000, amplification: {q16:256.2}x', 13, 10}, es, bx

	; Initialize mixer volume table. The table consists of 256 word values
	; for each of the 65 possible volumes (0 - 64).

	; For volume 0, all sample value is also 0.

	mov ecx, 256 / 2
	xor eax, eax
	rep stosd

	; Calculate values for volumes between 1 - 64

	; EAX: scaled sample value
	; EBX: amplification
	; ECX: volume
	; EDX: sample value scaling multiplier
	; ES:DI: volume table entry address
	; (E)SI: sample value

	inc ecx				; ECX: volume
	xor eax, eax

	align 4

.loop_volume:
	xor esi, esi			; ESI: sample value, -128 - 127

	; Calculate positive and negative multipliers for proper sample scaling.

	mov edx, ebx			; EDX: sample multiplier
	imul edx, ecx			; volume * eff. amp.
	shl edx, 8			; volume * eff. amp. * 32768 / 128

	align 4

.loop_scale_sample:
	mov eax, edx
	imul eax, esi
	add eax, 32767			; Rounding
	shr eax, 16			; EAX: scaled sample, high word clear
	stosw				; Store volume table entry

	; To ensure correct order in the volume table, go from 0 - 127, then
	; -128 - -1.

	inc si				; Next sample value
	jz .next_vol
	cmp si, 127
	jle .loop_scale_sample
	mov si, -128
	jmp .loop_scale_sample

	align 4

.next_vol:
	inc cl				; Next volume
	cmp cl, 64
	jbe .loop_volume

.exit:
	pop es
	pop di
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	clc
	retn


;------------------------------------------------------------------------------
; Initialize the renderer's linear interpolation lookup table.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

mod_swt_init_ipoltab:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push di
	push es

	cld

	mov es, [state(ipoltab_seg)]	; ES: linear interpolation table segment
	xor di, di			; ES:DI: linear interpolation table

	; Calculate fractional values for sample differences. The table consists
	; of 2^LIN_IPOL_EXP rows, where each row contains 256 words for
	; positive (0 -> 255) and 256 words for negative (-255 -> 0) sample
	; difference values multiplied by the index of the row, divided by the
	; amount of interpolation (2^LIN_IPOL_EXP). Effectively the fractional
	; part of sample position and the difference between next and current
	; sample form an index into the table. The difference goes into bits 0-8
	; (the difference is between -255 and 255, thus fits into 9 bits) and
	; the fractional part's most significant bits go into bits
	; 9-(8 + LIN_IPOL_EXP).

	mov cx, 1 << LIN_IPOL_EXP	; Calculate interpolation steps
	xor si, si			; Current interpolation step

	log {'Initializing {u16}x linear interpolation lookup table @{X16}:0000', 13, 10}, cx, es

.loop_samples:
	xor bx, bx			; Current sample difference

	; Calculate fractional values for positive range (0 -> 255).

	align 4

.loop_samples_pos:
	mov ax, bx
	imul si
	sar ax, LIN_IPOL_EXP
	adc ax, 0
	stosw
	inc bx
	cmp bx, 255
	jle .loop_samples_pos
	mov bx, -255

	; Calculate fractional values for negative range (-255 -> 0).

	align 4

.loop_samples_neg:
	mov ax, bx
	imul si
	sar ax, LIN_IPOL_EXP
	adc ax, 0
	stosw
	inc bx
	cmp bx, 0
	jle .loop_samples_neg

	inc si
	dec cx
	jnz .loop_samples

.exit:
	pop es
	pop di
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	clc
	retn


;------------------------------------------------------------------------------
; Initialize an 8-bit PCM sample for use with the wavetable mixer.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;    DS:SI - Pointer to mod_sample structure containing the sample header
; <- CF - Set on error
;    EAX - Error code if CF set or wavetable sample ID
;    DH - Set to 1 if the sample can be removed from RAM (wavetable has
;         own internal sample memory)
;------------------------------------------------------------------------------

	align 4

global mod_swt_upload_sample
mod_swt_upload_sample:
	push ebx
	push ecx
	push edi
	push es
	push edx
	push esi

	cld

	xor ax, ax
	mov es, ax			; ES: zeropage
	mov edi, [si + sample(addr)]
	test edi, edi
	jz .done			; No sample, nothing to do

	; Fill padding area with zeroes to prevent clicks with Watte
	; interpolation.

	mov al, es:[edi + SAMPLE_PADDING]
	mov ecx, SAMPLE_PADDING
	a32 rep stosb

	; Unroll the sample: add extra samples to the end, so there is no need
	; to worry about reaching the (loop) end of the sample in the unrolled
	; render loop.

	mov eax, [si + sample(addr)]
	mov ebx, [si + sample(rpt_start)]
	mov edx, [si + sample(rpt_len)]

	test edx, edx
	jnz .unroll_looped_sample

	; No sample repeat -> just fill zeroes after end of sample

	mov edi, [si + sample(length)]
	add edi, eax
	xor eax, eax
	%if (UNROLL_SAMPLES / 4 > 0)
	mov ecx, UNROLL_SAMPLES / 4
	a32 rep stosd
	%endif
	%if (UNROLL_SAMPLES % 4 > 0)
	mov ecx, UNROLL_SAMPLES % 4
	a32 rep stosb
	%endif
	jmp .done

	align 4

.unroll_looped_sample:

	; Sample repeat -> unroll loop

	add edx, ebx			; EDX: repeat end position
	add edx, eax
	mov edi, edx			; ES:EDI: sample repeat end position
	mov esi, ebx			; ES:ESI: sample repeat start position
	add esi, eax
	add ebx, eax			; EBX: repeat start position
	mov cx, UNROLL_SAMPLES

	align 4

.loop_unroll:
	mov al, es:[esi]		; Unroll repeat loop
	a32 stosb
	inc esi
	cmp esi, edx			; Wrap back to repeat start when needed
	jb .next_sample
	mov esi, ebx

.next_sample:
	dec cx
	jnz .loop_unroll

.done:
	pop esi
	pop edx

	log {'Sample initialized for software wavetable', 13, 10}

	; Calculate address of sample relative to player instance segment and
	; return it as wavetable ID

	mov eax, [si + sample(addr)]
	test eax, eax
	jz .no_sample
	sub eax, [mod.instance_addr]
	add eax, SAMPLE_PADDING

.no_sample:
	xor dh, dh
	clc

	pop es
	pop edi
	pop ecx
	pop ebx
	retn


;------------------------------------------------------------------------------
; Remove an 8-bit PCM sample from the wavetable mixer.
;------------------------------------------------------------------------------
; -> EAX - Sample wavetable ID
;    DS - Player instance segment
;    DS:SI - Pointer to mod_sample structure containing the sample header
; <- CF - Set on error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

	align 4

global mod_swt_free_sample
mod_swt_free_sample:
	clc
	retn


;------------------------------------------------------------------------------
; Reset all channels of the software wavetable.
;------------------------------------------------------------------------------
; -> DH - Initial pan for real stereo mixing
;    DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_swt_reset_channels
mod_swt_reset_channels:
	push eax
	push cx
	push di
	push ds
	push es

	mov ax, ds
	mov es, ax

	cld

	; Zero out channel[]

	mov di, state(channels)
	mov cx, channel.strucsize * MOD_MAX_CHANS / 4
	xor eax, eax
	rep stosd

	; Initialize non-zero values

	mov di, state(channels)
	mov cx, MOD_MAX_CHANS
	xor al, al

.loop_channels:
	mov word [di + channel.sample_hdr_ofs], -1
	mov ah, dh
	cmp al, 1
	jb .set_default_pan
	cmp al, 2
	ja .set_default_pan
	not ah

.set_default_pan:
	mov byte [di + channel.pan], ah

	inc al
	and al, 0x03
	add di, channel.strucsize
	dec cx
	jnz .loop_channels

.exit:
	pop es
	pop ds
	pop di
	pop cx
	pop eax
	retn


;------------------------------------------------------------------------------
; Set the sample to be played in a channel.
;------------------------------------------------------------------------------
; -> AL - Channel
;    AH - Mask bits
;         bit 0: Set sample from DS:SI
;         bit 1: Set playback position from CX.DX
;    ECX.DX - Playback position as 16.16 bit fixed point number
;    DS - Player instance segment
;    DS:SI - Pointer to mod_sample structure containing the sample header. Set
;            SI to -1 to stop playing in this channel.
;------------------------------------------------------------------------------

	align 4

global mod_swt_set_sample
mod_swt_set_sample:
	push bx
	push edi

	movzx edi, al			; DS:DI: wavetable channel structure
	mov di, [state(channel_ofs) + edi * 2]

	; Save sample header pointer and requested playback position

	test ah, 0x01			; Sample header pointer
	jz .position
	mov [di + channel.sample_hdr_ofs], si

.position:
	test ah, 0x02			; Playback position
	jz .exit
	mov [di + channel.sample_pos_int], ecx
	mov [di + channel.sample_pos_fr], dx

.exit:
	pop edi
	pop bx
	retn


;------------------------------------------------------------------------------
; Set the volume, panning and playback speed of a channel.
;------------------------------------------------------------------------------
; -> AL - Channel
;    AH - Mask bits
;         bit 0: Set volume to BL
;         bit 1: Set panning (balance) to BH
;         bit 2: Set speed to CX:DX
;    BL - Volume (0 - 64)
;    BH - Panning (0 - 255; 0 = far left, 255 = far right)
;    CX.DX - Playback speed as 16.16 fixed-point number
;    DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_swt_set_mixer
mod_swt_set_mixer:
	push edi

	movzx edi, al			; DS:EDI: wavetable channel structure
	mov di, [state(channel_ofs) + edi * 2]

	; Set mixer values

	test ah, 0x01			; Volume
	jz .pan
	mov [di + channel.volume], bl

.pan:
	test ah, 0x02			; Pan
	jz .panvol
	mov [di + channel.pan], bh

.panvol:
	test byte [state(output_format)], FMT_STEREO
	jz .speed

	; Calculate pan volume and bitshift for real panning

	push ax
	push bx
	push cx
	push fs
	push si

	mov cx, modplayer_data
	mov fs, cx			; FS: modplayer_data segment

	mov al, [di + channel.volume]	; AL: volume (0 - 64)
	mov bh, [di + channel.pan]	; BH: pan (0 - 255)
	call .calc_pan_volume
	mov [di + channel.right_volume], ch
	mov [di + channel.right_bitshift], cl
	mov [di + channel.right_pan_mul], si
	not bh				; BH: invert pan for left channel
	call .calc_pan_volume
	mov [di + channel.left_volume], ch
	mov [di + channel.left_bitshift], cl
	mov [di + channel.left_pan_mul], si

	pop si
	pop fs
	pop cx
	pop bx
	pop ax

.speed:
	test ah, 0x04			; Playback speed
	jz .exit
	mov [di + channel.speed_int], cx
	mov [di + channel.speed_fr], dx

.exit:
	pop edi
	retn

;------------------------------------------------------------------------------
; Calculate volume and bitshift for real panning
;------------------------------------------------------------------------------
; -> AL - Overall channel volume
;    BH - Panning
;    FS - Segment of modplayer_data
; <- CL - Pan bitshift
;    CH - Left/right channel volume
;    SI - Pan multiplier (0 - 256)
;------------------------------------------------------------------------------

	align 4

.calc_pan_volume:
	push ax
	push ebx
	push dx

	xor cl, cl			; CL: pan bitshift
	mov ch, al			; CH: channel volume
	mov si, 0x100
	cmp bh, 255
	je .use_pan_volume

	; Calculate panned volume using panning table

	movzx ebx, bh
	mov si, fs:[pantab + ebx * 2]
	mov bl, al			; BX: sample (BH is already zero)
	mov ax, si			; AX: pan ratio (16-bit)
	shr si, 8			; SI: pan multiplier
	mul bx
	mov ch, dl
	cmp dl, 32			; Panned volume >= 32: pan bitshift = 0
	jae .round_pan_volume

	; Calculate bitshift to improve accuracy of lower panned volume levels

	movzx ebx, dl
	mov cl, fs:[panshifttab + ebx]
	shld dx, ax, cl
	shl ax, cl

.round_pan_volume:

	; Round panned volume, but make sure to not exceed 64

	add ax, ax
	adc dx, 0
	mov ch, dl
	cmp ch, 64
	jbe .use_pan_volume
	mov ch, 64

.use_pan_volume:
	pop dx
	pop ebx
	pop ax
	retn


;------------------------------------------------------------------------------
; Add mixer information to mod_channel_info structure.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; -> ES:EDI - Pointer to buffer receiving mod_channel_info structures
; <- ES:EDI - Filled with data
;------------------------------------------------------------------------------

	align 4

global mod_swt_get_mixer_info
mod_swt_get_mixer_info:
	push eax
	push bx
	push si
	push edi

	mov bl, [mod.num_channels]
	mov si, state(channels)

.loop_channel:
	mov eax, [si + channel.sample_pos_int]
	mov es:[edi + mod_channel_info.sample_pos_int], eax
	mov ax, [si + channel.sample_pos_fr]
	mov es:[edi + mod_channel_info.sample_pos_fr], ax

	add si, channel.strucsize
	add edi, mod_channel_info.strucsize
	dec bl
	jnz .loop_channel

	pop edi
	pop si
	pop bx
	pop eax
	retn


;------------------------------------------------------------------------------
; Macro to render samples for a channel.
;------------------------------------------------------------------------------
; -> CX - Number of samples to render
;    DS - Player instance segment
;    DS:EBX - Render buffer pointer
;    DS:DI - Wavetable channel structure
;    ES - Player instance segment
;    FS - Volume table segment
;    GS - Linear interpolation lookup table segment
;    %1 - Operation on render buffer (RENDER_STORE / RENDER_ADD)
;    %2 - Panning balance (RENDER_PAN_L / RENDER_PAN_R)
;    %3 - Panning operation (RENDER_PAN_X / RENDER_PAN_HARD / RENDER_PAN_REAL)
;    %4 - Interpolation (RENDER_IPOL_NN / RENDER_IPOL_LIN)
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

%macro	render_channel 4

	push es

	; ---------------------------------------------------------------------
	; Initialize renderer

	; Setup sample index into render buffer for stereo rendering

	%assign	sample_idx 0
	%assign pan_idx 4

	%if (%2 = RENDER_PAN_R)
	%assign sample_idx 4
	%assign pan_idx 0
	%endif

	; Use high word of EBP for to-be rendered sample counter

	movzx ebp, cx
	dec ebp
	shl ebp, 16

%%setup_render:
	push ebx

	; DS:SI: sample header

	mov si, [di + channel.sample_hdr_ofs]
	cmp si, -1
	je %%no_sample

	; Setup self-modifying code

	mov eax, [di + channel.speed_int]
	%assign repcnt 1
	%rep UNROLL_COUNT		; Playback speed - integer part
	mov cs:[%%speed_int_ %+ repcnt], eax
	%assign repcnt repcnt + 1
	%endrep
	mov ax, [di + channel.speed_fr]
	%assign repcnt 1
	%rep UNROLL_COUNT		; Playback speed - fractional part
	mov cs:[%%speed_fr_ %+ repcnt], ax
	%assign repcnt repcnt + 1
	%endrep
	mov ebx, [si + sample(rpt_len)]
	test ebx, ebx			; Check if sample has repeat
	jnz %%init_looped_sample

	; Sample without loop

	mov byte cs:[%%reset_sample_jmp_dest], %%stop_sample - %%reset_sample_jmp
	mov eax, [si + sample(length)]
	cmp [di + channel.sample_pos_int], eax
	jae %%no_sample			; Sample end reached
	jmp %%init_sample_length

	align 4

%%init_looped_sample:
	mov cs:[%%sample_rpt_len], ebx
	mov byte cs:[%%reset_sample_jmp_dest], %%loop_sample - %%reset_sample_jmp
	mov eax, ebx
	add eax, [si + sample(rpt_start)]
	%if (%4 = RENDER_IPOL_WTL)
	inc eax				; Click-prevention when wrapping back
	%endif

%%init_sample_length:
	add eax, [si + sample(wt_id)]	; Relative to player instance
	mov cs:[%%sample_length], eax	; Sample length or repeat end position

	pop ecx				; Restore render buffer address
	push esi
	push edi

	; Register usage during mixing

	; EAX: sample value after amplification / temp
	; FS:EBX: volume table lookup index
	; BL: sample value before amplification
	; BH: left channel volume
	; GS:ECX: linear interpolation table lookup index (CX: temp)
	; FS:EDX: volume table lookup index for real panning / temp
	; DL: sample value before amplification
	; DH: right channel volume
	; EBP high word: number of samples to render (counter) - 1
	; BP: fractional sample position
	; DS:ESI: sample position pointer
	; DS:EDI: output render buffer pointer
	; DS: player instance segment
	; FS: volume table segment

	%if (%4 = RENDER_IPOL_WTL)

	; Volume and panning constants for Watte interpolation

	mov al, [di + channel.volume]
	%assign repcnt 1
	%rep UNROLL_COUNT		; Volume for Watte interpolation
	mov cs:[%%wtl_volume %+ repcnt], al
	%assign repcnt repcnt + 1
	%endrep

	%if (%3 = RENDER_PAN_REAL)
	mov eax, [di + channel.left_pan_mul]
	mov ebx, [di + channel.right_pan_mul]
	%assign repcnt 1
	%rep UNROLL_COUNT		; Panning ratio for Watte interpolation
	mov cs:[%%wtl_left_pan_mul %+ repcnt], eax
	mov cs:[%%wtl_right_pan_mul %+ repcnt], ebx
	%assign repcnt repcnt + 1
	%endrep
	%endif

	%else

	; Volume and panning for nearest and linear interpolation

	%if (%3 = RENDER_PAN_REAL)

	mov al, [di + channel.left_bitshift]
	%assign repcnt 1
	%rep UNROLL_COUNT		; Left sample bitshift for real panning
	mov cs:[%%left_bitshift_ %+ repcnt], al
	%assign repcnt repcnt + 1
	%endrep

	mov al, [di + channel.right_bitshift]
	%assign repcnt 1
	%rep UNROLL_COUNT		; Right sample bitshift for real panning
	mov cs:[%%right_bitshift_ %+ repcnt], al
	%assign repcnt repcnt + 1
	%endrep

	movzx ebx, byte [di + channel.left_volume]
	shl ebx, 8
	movzx edx, byte [di + channel.right_volume]
	shl edx, 8

	%else				; RENDER_PAN_REAL

	movzx ebx, byte [di + channel.volume]
	shl ebx, 8

	%endif

	%endif

	mov bp, [di + channel.sample_pos_fr]
	mov eax, [di + channel.sample_pos_int]
	add eax, [si + sample(wt_id)]
	mov edi, ecx
	mov esi, eax
	xor ecx, ecx
	cld

	; ---------------------------------------------------------------------
	; Sample render loop begins

	cmp ebp, UNROLL_COUNT * 65536
	jae %%loop_render_unrolled	; Can render a full unrolled chunk

%%loop_render_partial:

	; If a full unrolled loop is not necessary, jump into it at the
	; appropriate position to render only the number of samples needed.

	mov eax, ebp
	shr eax, 16
	jmp [cs:%%render_cnt_tab + eax * 2]

	; ---------------------------------------------------------------------
	; Handle end of sample scenarios (kept close to %%loop_render_unrolled
	; to enable using short jumps)

	align 4

%%loop_sample:

	; Looped sample - wrapback if repeat end exceeded

	sub esi, 0x12345678		; Wrap back by length of loop
	%%sample_rpt_len EQU $ - 4
	jmp %%loop_render_unrolled	; Render next sample

	align 4

%%no_sample:

	; End of sample or no sample selected for the channel, fill rest of
	; buffer with zeroes if RENDER_STORE, otherwise just return to caller.

	pop ecx				; Restore render buffer address
	push esi
	push edi

	mov bp, [di + channel.sample_pos_fr]
	mov eax, [si + sample(wt_id)]
	add eax, [di + channel.sample_pos_int]
	mov esi, eax
	mov edi, ecx

%%stop_sample:
	%if (%1 = RENDER_STORE)

	; Erase rest of buffer

	mov ecx, ebp
	shr ecx, 16
	inc cx
	add cx, cx
	jz %%done

	mov ax, ds
	mov es, ax
	xor eax, eax
	a32 rep stosd

	%endif

	%if (%3 = RENDER_PAN_X)

	; Apply cross-fade on rest of buffer for RENDER_PAN_X if no sample is
	; being played in the channel (or the sample ends before the end of the
	; render buffer).

	jmp %%stop_pan_x

	%endif

	jmp %%done

	; ---------------------------------------------------------------------
	; Unrolled sample render loop

	align 4

%%loop_render_next:

	; The render loop jumps here AFTER a full unrolled loop finished. This
	; is kept here so that we can do partial rendering by jumping into the
	; unrolled loop at a specific position.

	sub ebp, UNROLL_COUNT * 65536

	; Check if we can render a full loop (this is skipped for the very first
	; round by the code above %%loop_render_partial).

	cmp ebp, UNROLL_COUNT * 65536
	jb %%loop_render_partial

%%loop_render_unrolled:

	; Check if end of sample has reached. When yes, jump to either
	; %%loop_sample or %%stop_sample, depending on whether the sample is
	; looped or not.

	cmp esi, 0x12345678		; End of loop or sample?
	%%sample_length EQU $ - 4
	jae %%loop_sample		; Yes, check whether it's looped
	%%reset_sample_jmp EQU $
	%%reset_sample_jmp_dest EQU $ - 1

	; The following piece of code is the actual unrolled sample renderer.

	%assign repcnt 1
	%assign rendercnt UNROLL_COUNT - 1
	%rep UNROLL_COUNT

%%render_cnt_ %+ rendercnt:

	; There are two separate code paths to handle nearest neighbour, linear
	; and the high quality Watte interpolation. The former two operate in
	; the 8-bit sample domain and then apply volume/amplification and
	; panning via lookup tables (no multiplications). This is better suited
	; for 386s and older, L2 cacheless 486s. Operating in the 8-bit domain
	; doesn't matter for nearest neighbour. Linear interpolation's quality
	; is somewhat degraded by this, however it still sounds pretty good and
	; allows avoiding slow multiplications.
	;
	; Watte on the other hand operates in 16-bit domain. Not only because
	; it's the high quality resampler, but because it can produce overshoot
	; (the interpolated sample may exceed the 8-bit range). Extending the
	; volume lookup table (which is already over 32 KB) was no-go and
	; clipping the oversampled value to 8-bits was slow and causes nasty
	; artefacts, something i really don't want from a HQ resampler. Watte
	; needs two multiplications for the calculation of the interpolated
	; sample. Volume and amplification is applied using the volume table.
	; For real panning, two additional multiplications are necessary to
	; calculate sample values for left and right channels.

	;----------------------------------------------------------------------
	; Jon Watte tri-linear interpolation.

	%if (%4 = RENDER_IPOL_WTL)

	; Code adapted from Olli Niemitalo's "Polynomial Interpolators for
	; High-Quality Resampling of Oversampled Audio" paper (deip.pdf).
	;
	; // 4-point, 2nd-order Watte tri-linear (x-form)
	; float ym1py2 = y[-1]+y[2];
	; float c0 = y[0];
	; float c1 = 3/2.0*y[1] - 1/2.0*(y[0]+ym1py2);
	; float c2 = 1/2.0*(ym1py2-y[0]-y[1]);
	; return (c2*x+c1)*x+c0;

	; This interpolation method has pretty good quality, much cleaner, than
	; linear, but that is partly due to operating in the 16-bit domain. Also
	; it can mostly be done using integer operations only except for the
	; multiplications in the last C line. May sound dull on MODs with low
	; quality samples though.

	push edi

	mov edx, [esi - 1]		; [-1][0][1][2]
	mov ebx, 0x1200			; FS:EBX: index into volume table
	%%wtl_volume %+ repcnt EQU $ - 3
	rol edx, 8			; [2][-1][0][1]
	mov bl, dl			; [2]
	movsx eax, word fs:[ebx * 2]	; EAX: y[2]
	mov bl, dh			; [-1]
	movsx ecx, word fs:[ebx * 2]	; ECX: y[-1]
	shr edx, 16			; [0][1], EDX high word 0
	mov bl, dl			; [0]
	add ecx, eax			; ECX: ym1py2 = y[-1] + y[2]
	movsx eax, word fs:[ebx * 2]	; EAX: c0 = y[0]
	mov bl, dh			; [1]
	sub ecx, eax			; ECX: ym1py2 - y[0]
	lea edi, [ecx + eax * 2]	; EDI: y[0] + ym1py2
	mov dx, bp
	movsx ebx, word fs:[ebx * 2]	; EBX: y[1]
	shr dx, (16 - WATTE_IPOL_EXP)	; EDX: Fractional part for interpolation
	sub ecx, ebx			; ECX: c2 * 2 = ym1py2 - y[0] - y[1]
	lea ebx, [ebx * 2 + ebx]	; EBX: 3 * y[1]
	imul ecx, edx			; ECX: c2 * 2 * x
	sub ebx, edi			; EBX: c1 * 2 = 3 * y[1] - y[0] - ym1py2
	shl ebx, WATTE_IPOL_EXP		; EBX: c1 * 2 (fixed point normalized)
	add ecx, ebx			; ECX: c2 * 2 * x + c1 * 2
	imul ecx, edx			; ECX: (c2 * 2 * x + c1 * 2) * x
	sar ecx, WATTE_IPOL_EXP * 2 + 1	; ECX: (c2 * x + c1) * x
	add eax, ecx			; EAX: (c2 * x + c1) * x + c0

	pop edi

	%if (%3 = RENDER_PAN_HARD)

	; Hard panning: render sample for primary channel, clear/skip other
	; channel

	store_sample %1, pan_idx, 0
	store_sample %1, sample_idx, eax

	%elif (%3 = RENDER_PAN_X)

	; Fixed panning with crossfade: store/mix 75% of rendered sample to
	; panned channel. This is only used for the last channel, the rest is
	; mixed with hard pan. It is much faster that way, the cross-mixing is
	; done here, in the last channel. However, care must be taken to process
	; the rest of the buffer if nothing is playing in the last channel or
	; the sample ends before the end of buffer.

	mov edx, [edi + pan_idx]	; EDX: ch2 hard pan sample
	add eax, [edi + sample_idx]	; EAX: ch1 hard pan sample
	lea ecx, [edx * 2 + edx]
	sar ecx, 2			; ECX: .75 * ch2 hard pan sample
	add ecx, eax			; ECX: ch1 + .75 * ch2 cross-mixed
	mov [edi + sample_idx], ecx	; ch1: ch1 + .75 * ch2 cross-mixed
	lea ecx, [eax * 2 + eax]
	sar ecx, 2			; ECX: .75 * ch1 hard pan sample
	add ecx, edx			; ECX: ch2 + .75 * ch1 cross-mixed
	mov [edi + pan_idx], ecx	; ch2: ch2 + .75 * ch1 cross-mixed

	%elif (%3 = RENDER_PAN_REAL)

	; Real panning: multiply sample value by panning ratio for each channel.

	imul edx, eax, 0x12345678
	%%wtl_left_pan_mul %+ repcnt EQU $ - 4
	imul eax, 0x12345678
	%%wtl_right_pan_mul %+ repcnt EQU $ - 4
	sar edx, 8
	sar eax, 8
	store_sample %1, 0, edx
	store_sample %1, 4, eax

	%endif

	%else

	;----------------------------------------------------------------------
	; Nearest neighbour (zero-order hold) and linear interpolation.

	%if (%4 = RENDER_IPOL_NN)

	; Nearest neighbour interpolation: use nearest sample

	mov bl, [esi]			; Get current 8-bit sample

	%elif (%4 = RENDER_IPOL_LIN)

	; Linear upsampling interpolation: interpolate between sample points.
	; When downsampling (ie. playback speed > 1), this routine should
	; probably take skipped samples into consideration, but there are no
	; more registers left to do that. It's not a big  problem though and
	; still sounds a lot better than using nearest neighbor for
	; downsampling.

	mov ax, bp			; Calculate interpolation index
	shr ax, (7 - LIN_IPOL_EXP)
	mov bl, ah
	mov ax, [esi]			; Get current and next 8-bit samples
	and bl, 0xfe			; BL: interpolation index
	movsx cx, ah			; CX: Next 8-bit sample (sign-extend)
	movsx ax, al			; AX: Current 8-bit sample (sign-extend)
	sub cx, ax			; CX: Difference between samples (9-bit)
	%if (%3 = RENDER_PAN_X)
	and ecx, 0x01ff			; Crossfade mixer destroys ECX high word
	%else
	and cx, 0x01ff
	%endif
	or ch, bl			; ECX: merge interpolation index
	add ax, gs:[ecx * 2]		; Add interpolated difference
	mov bl, al			; Use interpolated sample value

	%endif

	%if (%3 = RENDER_PAN_HARD)

	; Hard panning: render sample for primary channel, clear/skip other
	; channel

	movsx eax, word fs:[ebx * 2]
	store_sample %1, pan_idx, 0
	store_sample %1, sample_idx, eax

	%elif (%3 = RENDER_PAN_X)

	; Fixed panning with crossfade: store/mix 75% of rendered sample to
	; panned channel. This is only used for the last channel, the rest is
	; mixed with hard pan. It is much faster that way, the cross-mixing is
	; done here, in the last channel. However, care must be taken to process
	; the rest of the buffer if nothing is playing in the last channel or
	; the sample ends before the end of buffer.

	movsx eax, word fs:[ebx * 2]
	mov edx, [edi + pan_idx]	; EDX: ch2 hard pan sample
	add eax, [edi + sample_idx]	; EAX: ch1 hard pan sample
	lea ecx, [edx * 2 + edx]
	sar ecx, 2			; ECX: .75 * ch2 hard pan sample
	add ecx, eax			; ECX: ch1 + .75 * ch2 cross-mixed
	mov [edi + sample_idx], ecx	; ch1: ch1 + .75 * ch2 cross-mixed
	lea ecx, [eax * 2 + eax]
	sar ecx, 2			; ECX: .75 * ch1 hard pan sample
	add ecx, edx			; ECX: ch2 + .75 * ch1 cross-mixed
	mov [edi + pan_idx], ecx	; ch2: ch2 + .75 * ch1 cross-mixed

	%elif (%3 = RENDER_PAN_REAL)

	; Real panning: calculate panned channel sample value using volume table
	; and a bitshift operation.

	movsx eax, word fs:[ebx * 2]
	sar ax, 0x12
	%%left_bitshift_ %+ repcnt EQU $ - 1
	mov dl, bl			; DL: sample value for right channel
	store_sample %1, 0, eax
	movsx eax, word fs:[edx * 2]
	sar ax, 0x12
	%%right_bitshift_ %+ repcnt EQU $ - 1
	store_sample %1, 4, eax

	%endif				; RENDER_PAN

	%endif				; RENDER_IPOL

	; Next sample

	add edi, 8			; Advance buffer position (2 x dword)
	add bp, 0x1234			; Increase fractional position
	%%speed_fr_ %+ repcnt EQU $ - 2
	adc esi, 0x12345678		; Increase sample position
	%%speed_int_ %+ repcnt EQU $ - 4

	%assign repcnt repcnt + 1
	%assign rendercnt rendercnt - 1
	%endrep

	; If we have rendered a full unrolled loop, render some more samples.
	; Otherwise, the job is finished.

	cmp ebp, UNROLL_COUNT * 65536
	jae %%loop_render_next

%%done:
	mov eax, esi
	pop edi				; DS:DI: wavetable channel structure
	pop esi

	; Update sample positions in wavetable channel structure.

	mov [di + channel.sample_pos_fr], bp
	sub eax, [si + sample(wt_id)]
	mov [di + channel.sample_pos_int], eax

	pop es
	retf

	; ---------------------------------------------------------------------
	; Jump table for partial rendering

	align 4

%%render_cnt_tab:
	%assign rendercnt 0
	%rep UNROLL_COUNT
	dw %%render_cnt_ %+ rendercnt
	%assign rendercnt rendercnt + 1
	%endrep

	%if (%3 = RENDER_PAN_X)

%%stop_pan_x:

	; Crossfade rest of buffer when no sample is playing in the last channel
	; or the sample ends before the end of the render buffer.

	shr ebp, 16
	inc bp				; BP: number of samples left in buffer
	mov bx, bp
	jz %%done
	shr bp, 4			; BP: number of samples / 16
	jz %%pan_x_rest

%%pan_x_loop:

	; Unrolled cross-fade mixer loop, process 16 samples at once for better
	; performance.

	%assign idx 0
	%rep 16

	mov eax, [edi + sample_idx + idx]
	mov edx, [edi + pan_idx + idx]
	lea ecx, [edx * 2 + edx]
	sar ecx, 2
	add ecx, eax
	mov [edi + sample_idx + idx], ecx
	lea ecx, [eax * 2 + eax]
	sar ecx, 2
	add ecx, edx
	mov [edi + pan_idx + idx], ecx

	%assign idx idx + 8
	%endrep

	add edi, 8 * 16
	dec bp
	jnz %%pan_x_loop

%%pan_x_rest:
	and bl, 0x0f			; BL: rest of samples in render buffer
	jz %%done

	; Crossfade the rest of samples (up to 15) left after the unrolled loop

%%pan_x_loop_rest:
	mov eax, [edi + sample_idx]
	mov edx, [edi + pan_idx]
	lea ecx, [edx * 2 + edx]
	sar ecx, 2
	add ecx, eax
	mov [edi + sample_idx], ecx
	lea ecx, [eax * 2 + eax]
	sar ecx, 2
	add ecx, edx
	mov [edi + pan_idx], ecx

	add edi, 8
	dec bl
	jnz %%pan_x_loop_rest
	jmp %%done

	%endif

%endmacro


;------------------------------------------------------------------------------
; Helper macro to save the rendered sample into the wavetable render buffer.
;------------------------------------------------------------------------------
; -> EAX - Rendered sample
;    DS:EDI - Pointer to next entry in wavetable render buffer
;    %1 - Operation on render buffer (RENDER_STORE / RENDER_ADD)
;    %2 - Channel offset within render buffer where the sample shall be stored
;    %3 - Value to store/add
;------------------------------------------------------------------------------

%macro	store_sample 3

	%if (%1 = RENDER_STORE)
	mov dword [edi + %2], %3
	%elif (%1 = RENDER_ADD && %3 != 0)
	add dword [edi + %2], %3
	%endif

%endmacro


;------------------------------------------------------------------------------
; Output audio buffer converter macro.
;------------------------------------------------------------------------------
; TODO: optimized converter for 1x amp scenario
; 8 bit -> no clip for sure,
; 16 bit -> only positive clipping possible
;------------------------------------------------------------------------------
; -> CX - Number of samples to convert
;    EDI - Linear address of output device buffer
;    DS - Player instance segment
;    DS:ESI - Wavetable render buffer pointer
;    ES - Player instance segment
;    %1 - Output device bitdepth (FMT_8BIT / FMT_16BIT)
;    %2 - Output device number of channels (FMT_MONO / FMT_STEREO)
;    %3 - Output device data format (FMT_SIGNED / FMT_UNSIGNED)
; <- EDI - Linear address of output device buffer after last sample
;    Destroys everything except segment registers.
;------------------------------------------------------------------------------

%macro	convert_buffer 3

	; Setup registers for conversion

	sub edi, [mod.instance_addr]	; DS:EDI: output device buffer
	%if (%2 = FMT_STEREO)
	add cx, cx			; Twice as many samples when stereo
	%endif

	; ---------------------------------------------------------------------
	; Conversion loop

	align 4

.loop_buffer:

	; Get 32-bit sample from wavetable render buffer

	a32 lodsd
	%if (%2 = FMT_MONO)		; Mono: skip right channel
	add esi, 4
	%endif

	; Convert to unsigned + 8-bit rounding correction

	%if (%1 = FMT_8BIT && %3 = FMT_SIGNED)
	add eax, 0x80
	%elif (%1 = FMT_8BIT && %3 = FMT_UNSIGNED)
	add eax, 0x8080
	%elif (%3 = FMT_UNSIGNED)
	add eax, 0x8000
	%endif

	; Clip detection

	%if (%3 = FMT_SIGNED)
	cmp eax, 32767
	jg %%clip_pos
	cmp eax, -32768
	jl %%clip_neg
	%elif (%3 = FMT_UNSIGNED)
	test eax, 0xffff0000
	jnz %%clip_unsigned
	%endif

	; No clipping

	%if (%1 = FMT_8BIT)
	mov [edi], ah
	inc edi
	%elif (%1 = FMT_16BIT)
	a32 stosw
	%endif

	convert_buffer_next_sample

	%if (%3 = FMT_SIGNED)

	align 4

%%clip_pos:

	; Positive clipping

	%if (%1 = FMT_8BIT)
	inc edi				; Increase before mov to save jXX cycles
	mov byte [edi - 1], 127
	%elif (%1 = FMT_16BIT)
	add edi, 2			; Increase before mov to save jXX cycles
	mov word [edi - 2], 32767
	%endif

	convert_buffer_next_sample

	align 4

%%clip_neg:

	; Negative clipping

	%if (%1 = FMT_8BIT)
	inc edi				; Increase before mov to save jXX cycles
	mov byte [edi - 1], -128
	%elif (%1 = FMT_16BIT)
	add edi, 2			; Increase before mov to save jXX cycles
	mov word [edi - 2], -32768
	%endif

	convert_buffer_next_sample

	%elif (%3 = FMT_UNSIGNED)

	align 4

%%clip_unsigned:
	cmp eax, 0
	setg al

	%if (%1 = FMT_8BIT)
	neg al
	a32 stosb
	%elif (%1 = FMT_16BIT)
	xor ah, ah
	neg ax
	a32 stosw
	%endif

	convert_buffer_next_sample

	%endif

%endmacro

;------------------------------------------------------------------------------
; Helper macro to loop to next sample when converting to the output buffer.
;------------------------------------------------------------------------------
; -> CX - Number of samples left to convert
;------------------------------------------------------------------------------

%macro	convert_buffer_next_sample 0

	dec cx
	jnz .loop_buffer
	add edi, [mod.instance_addr]
	retn

%endmacro


;------------------------------------------------------------------------------
; Output audio buffer converters. One function for each combinations of
; bitdepth and channels.
;------------------------------------------------------------------------------
; -> CX - Number of samples to convert
;    DS - Player instance segment
;    EDI - Linear address of output device buffer
; <- EDI - Linear address of output device buffer after last sample
;    Destroys everything except segment registers.
;------------------------------------------------------------------------------

	align 4
out_8bit_mono_signed:
	convert_buffer FMT_8BIT, FMT_MONO, FMT_SIGNED

	align 4
out_16bit_mono_signed:
	convert_buffer FMT_16BIT, FMT_MONO, FMT_SIGNED

	align 4
out_8bit_stereo_signed:
	convert_buffer FMT_8BIT, FMT_STEREO, FMT_SIGNED

	align 4
out_16bit_stereo_signed:
	convert_buffer FMT_16BIT, FMT_STEREO, FMT_SIGNED

	align 4
out_8bit_mono_unsigned:
	convert_buffer FMT_8BIT, FMT_MONO, FMT_UNSIGNED

	align 4
out_16bit_mono_unsigned:
	convert_buffer FMT_16BIT, FMT_MONO, FMT_UNSIGNED

	align 4
out_8bit_stereo_unsigned:
	convert_buffer FMT_8BIT, FMT_STEREO, FMT_UNSIGNED

	align 4
out_16bit_stereo_unsigned:
	convert_buffer FMT_16BIT, FMT_STEREO, FMT_UNSIGNED


;------------------------------------------------------------------------------
; Wavetable channel renderers. One function for each combination of store/add
; (first/additional channels) and pan settings.
;------------------------------------------------------------------------------
; -> CX - Number of samples to render
;    DS - Player instance segment
;    DS:EBX - Render buffer pointer
;    DS:DI - Wavetable channel structure
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

segment modplayer_nearest public use16 class=CODE align=16
segment modplayer_nearest

	align 4
render_left_store:
	render_channel RENDER_STORE, RENDER_PAN_L, RENDER_PAN_HARD, RENDER_IPOL_NN

	align 4
render_left_mix:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_HARD, RENDER_IPOL_NN

	align 4
render_right_store:
	render_channel RENDER_STORE, RENDER_PAN_R, RENDER_PAN_HARD, RENDER_IPOL_NN

	align 4
render_right_mix:
	render_channel RENDER_ADD, RENDER_PAN_R, RENDER_PAN_HARD, RENDER_IPOL_NN

	align 4
render_leftx_mix:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_X, RENDER_IPOL_NN

	align 4
render_rightx_mix:
	render_channel RENDER_ADD, RENDER_PAN_R, RENDER_PAN_X, RENDER_IPOL_NN

	align 4
render_stereo_store:
	render_channel RENDER_STORE, RENDER_PAN_L, RENDER_PAN_REAL, RENDER_IPOL_NN

	align 4
render_stereo_mix:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_REAL, RENDER_IPOL_NN

segment modplayer_linear public use16 class=CODE align=16
segment modplayer_linear

; Same for linear interpolation

	align 4
render_left_store_lin:
	render_channel RENDER_STORE, RENDER_PAN_L, RENDER_PAN_HARD, RENDER_IPOL_LIN

	align 4
render_left_mix_lin:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_HARD, RENDER_IPOL_LIN

	align 4
render_right_store_lin:
	render_channel RENDER_STORE, RENDER_PAN_R, RENDER_PAN_HARD, RENDER_IPOL_LIN

	align 4
render_right_mix_lin:
	render_channel RENDER_ADD, RENDER_PAN_R, RENDER_PAN_HARD, RENDER_IPOL_LIN

	align 4
render_leftx_mix_lin:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_X, RENDER_IPOL_LIN

	align 4
render_rightx_mix_lin:
	render_channel RENDER_ADD, RENDER_PAN_R, RENDER_PAN_X, RENDER_IPOL_LIN

	align 4
render_stereo_store_lin:
	render_channel RENDER_STORE, RENDER_PAN_L, RENDER_PAN_REAL, RENDER_IPOL_LIN

	align 4
render_stereo_mix_lin:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_REAL, RENDER_IPOL_LIN

segment modplayer_watte public use16 class=CODE align=16
segment modplayer_watte

; Same for Watte tri-linear interpolation

	align 4
render_left_store_wtl:
	render_channel RENDER_STORE, RENDER_PAN_L, RENDER_PAN_HARD, RENDER_IPOL_WTL

	align 4
render_left_mix_wtl:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_HARD, RENDER_IPOL_WTL

	align 4
render_right_store_wtl:
	render_channel RENDER_STORE, RENDER_PAN_R, RENDER_PAN_HARD, RENDER_IPOL_WTL

	align 4
render_right_mix_wtl:
	render_channel RENDER_ADD, RENDER_PAN_R, RENDER_PAN_HARD, RENDER_IPOL_WTL

	align 4
render_leftx_mix_wtl:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_X, RENDER_IPOL_WTL

	align 4
render_rightx_mix_wtl:
	render_channel RENDER_ADD, RENDER_PAN_R, RENDER_PAN_X, RENDER_IPOL_WTL

	align 4
render_stereo_store_wtl:
	render_channel RENDER_STORE, RENDER_PAN_L, RENDER_PAN_REAL, RENDER_IPOL_WTL

	align 4
render_stereo_mix_wtl:
	render_channel RENDER_ADD, RENDER_PAN_L, RENDER_PAN_REAL, RENDER_IPOL_WTL

segment modplayer


;------------------------------------------------------------------------------
; Render audio into the output device buffer.
;------------------------------------------------------------------------------
; -> CX - Number of samples to render
;    DS - Player instance segment
;    EDI - Linear address of output device buffer
; <- EDI - Linear address of output device buffer after last rendered sample
;    Destroys everything except segment registers.
;------------------------------------------------------------------------------

	align 4

global mod_swt_render
mod_swt_render:
	cld

	test cx, cx
	jz .exit

	push es
	push fs
	push gs
	mov ax, ds
	mov es, ax
	mov fs, [state(voltab_seg)]
	mov gs, [state(ipoltab_seg)]

	; ---------------------------------------------------------------------
	; Render samples to wavetable render buffer

	push cx
	push edi

	movzx eax, byte [mod.num_channels]
	xor esi, esi
	mov di, state(channels)
	mov ebx, [state(buffer_addr)]
	sub ebx, [mod.instance_addr]
	mov edx, 0x12345678
	render_pantab_base EQU $ - 4

	; Render first channel (store rendered samples)

	push ax
	push ebx
	push cx
	push edx
	push esi
	push di

	movzx si, byte cs:[edx + esi]
	call far [cs:render_store_fn + si]
	render_store_fn_addr EQU $ - 2

	pop di
	pop esi
	pop edx
	pop cx
	pop ebx
	pop ax

	add di, channel.strucsize
	dec al
	jz .exit_render			; Single-channel, stop rendering
	dec al
	jz .last_channel		; Two channels, skip middle channels

	; Render additional channels (add rendered samples)

.loop_channels:
	inc si

	push ax
	push ebx
	push cx
	push edx
	push esi
	push di

	movzx si, byte cs:[edx + esi]
	call far [cs:render_mix_fn + si]
	render_mix_fn_addr EQU $ - 2

	pop di
	pop esi
	pop edx
	pop cx
	pop ebx
	pop ax

	add di, channel.strucsize
	dec al
	jnz .loop_channels

	; Render last channel (add rendered samples and cross-fade if needed)

.last_channel:
	inc si

	push ebx

	movzx si, byte cs:[edx + esi]
	call far [cs:render_mix_fn + si]
	render_mix_fn_last_addr EQU $ - 2

	pop ebx

.exit_render:
	pop edi
	pop cx

	; ---------------------------------------------------------------------
	; Convert samples from wavetable render buffer to output buffer

	mov esi, ebx
	movzx ebx, byte [state(output_format)]
	call [cs:out_convert_fn + ebx * 2]

	pop gs
	pop fs
	pop es

.exit:
	retn


;------------------------------------------------------------------------------
; Render audio into the output device buffer in the softwave wavetable's
; internal rendering format.
;------------------------------------------------------------------------------
; -> CX - Number of samples to render
;    DS - Player instance segment
;    EDI - Linear address of output device buffer
; <- Destroys everything except segment registers and EDI.
;------------------------------------------------------------------------------

	align 4

global mod_swt_render_direct
mod_swt_render_direct:
	cld

	test cx, cx
	jz .exit

	push es
	push fs
	push gs
	push edi

	mov ax, ds
	mov es, ax
	mov fs, [state(voltab_seg)]
	mov gs, [state(ipoltab_seg)]

	; ---------------------------------------------------------------------
	; Render samples to output device buffer

	mov ebx, edi
	sub ebx, [mod.instance_addr]
	movzx eax, byte [mod.num_channels]
	xor esi, esi
	mov di, state(channels)
	mov edx, 0x12345678
	render_direct_pantab_base EQU $ - 4

	; Render first channel (store rendered samples)

	push ax
	push ebx
	push cx
	push edx
	push esi
	push di

	movzx si, byte cs:[edx + esi]
	call far [cs:render_store_fn + si]
	render_direct_store_fn_addr EQU $ - 2

	pop di
	pop esi
	pop edx
	pop cx
	pop ebx
	pop ax

	add di, channel.strucsize
	dec al
	jz .exit_render			; Single-channel, stop rendering
	dec al
	jz .last_channel		; Two channels, skip middle channels

	; Render additional channels (add rendered samples)

.loop_channels:
	inc si

	push ax
	push ebx
	push cx
	push edx
	push esi
	push di

	movzx si, byte cs:[edx + esi]
	call far [cs:render_mix_fn + si]
	render_direct_mix_fn_addr EQU $ - 2

	pop di
	pop esi
	pop edx
	pop cx
	pop ebx
	pop ax

	add di, channel.strucsize
	dec al
	jnz .loop_channels

	; Render last channel (add rendered samples and cross-fade)

.last_channel:
	inc si

	movzx si, byte cs:[edx + esi]
	call far [cs:render_mix_fn + si]
	render_direct_mix_fn_last_addr EQU $ - 2

.exit_render:
	pop edi
	pop gs
	pop fs
	pop es

.exit:
	retn


;==============================================================================
; Data area
;==============================================================================

		; Output buffer conversion function lookup table. Must be
		; aligned with FMT_* constants!

		alignb 2
out_convert_fn	dw out_8bit_mono_signed
		dw out_16bit_mono_signed
		dw out_8bit_stereo_signed
		dw out_16bit_stereo_signed
		dw out_8bit_mono_unsigned
		dw out_16bit_mono_unsigned
		dw out_8bit_stereo_unsigned
		dw out_16bit_stereo_unsigned

		; Render function lookup tables: render_store_fn for first
		; channel (overwrites the render buffer) and render_mix_fn for
		; further channels (adds to the render buffer).

		alignb 4
render_store_fn	dw render_left_store, modplayer_nearest
		dw render_right_store, modplayer_nearest
		dw render_left_store, modplayer_nearest
		dw render_right_store, modplayer_nearest
		dw render_stereo_store, modplayer_nearest

render_mix_fn	dw render_left_mix, modplayer_nearest
		dw render_right_mix, modplayer_nearest
		dw render_left_mix, modplayer_nearest
		dw render_right_mix, modplayer_nearest
		dw render_stereo_mix, modplayer_nearest
		RENDER_MIX_FN_LAST EQU $ - render_mix_fn

		dw render_left_mix, modplayer_nearest
		dw render_right_mix, modplayer_nearest
		dw render_leftx_mix, modplayer_nearest
		dw render_rightx_mix, modplayer_nearest
		dw render_stereo_mix, modplayer_nearest

		; Same for linear interpolation

renlin_store_fn	dw render_left_store_lin, modplayer_linear
		dw render_right_store_lin, modplayer_linear
		dw render_left_store_lin, modplayer_linear
		dw render_right_store_lin, modplayer_linear
		dw render_stereo_store_lin, modplayer_linear

renlin_mix_fn	dw render_left_mix_lin, modplayer_linear
		dw render_right_mix_lin, modplayer_linear
		dw render_left_mix_lin, modplayer_linear
		dw render_right_mix_lin, modplayer_linear
		dw render_stereo_mix_lin, modplayer_linear

		dw render_left_mix_lin, modplayer_linear
		dw render_right_mix_lin, modplayer_linear
		dw render_leftx_mix_lin, modplayer_linear
		dw render_rightx_mix_lin, modplayer_linear
		dw render_stereo_mix_lin, modplayer_linear

		; Same for Watte tri-linear interpolation

renwtl_store_fn	dw render_left_store_wtl, modplayer_watte
		dw render_right_store_wtl, modplayer_watte
		dw render_left_store_wtl, modplayer_watte
		dw render_right_store_wtl, modplayer_watte
		dw render_stereo_store_wtl, modplayer_watte

renwtl_mix_fn	dw render_left_mix_wtl, modplayer_watte
		dw render_right_mix_wtl, modplayer_watte
		dw render_left_mix_wtl, modplayer_watte
		dw render_right_mix_wtl, modplayer_watte
		dw render_stereo_mix_wtl, modplayer_watte

		dw render_left_mix_wtl, modplayer_watte
		dw render_right_mix_wtl, modplayer_watte
		dw render_leftx_mix_wtl, modplayer_watte
		dw render_rightx_mix_wtl, modplayer_watte
		dw render_stereo_mix_wtl, modplayer_watte

		; Lookup table for render function indexes for various stereo
		; output modes. Must be aligned with MOD_PAN_* constants!

modpantab	dw monotab, hardpantab, xpantab, realpantab

		; Indexes into render_store_fn and render_mix_fn tables for
		; various stereo output modes.

monotab:	; MOD_PAN_MONO: Mix everything to left channel
		%rep MOD_MAX_CHANS
		db 0
		%endrep

hardpantab:	; MOD_PAN_HARD: Left - Right - Right - Left
		%rep ((MOD_MAX_CHANS + 3) / 4)
		db 0, 4, 4, 0
		%endrep

xpantab:	; MOD_PAN_CROSS: Left - Right - Right - Left
		%rep ((MOD_MAX_CHANS + 3) / 4)
		db 8, 12, 12, 8
		%endrep

realpantab:	; MOD_PAN_REAL: Use real stereo mixer based on panning effect
		%rep MOD_MAX_CHANS
		db 16
		%endrep


segment modplayer_data public use16 class=DATA align=16
segment modplayer_data

		; Multiplier table for panning volume calculation (0 - 255),
		; uses logarithmic range. The last value should be 65536, but
		; that's not possible. This edge case is handled in the code
		; and the value from the table is ignored.

		alignb 2
pantab		dw 0, 4104, 5804, 7108, 8208, 9177, 10053, 10858
		dw 11608, 12312, 12978, 13612, 14217, 14797, 15356, 15895
		dw 16416, 16921, 17412, 17889, 18354, 18807, 19250, 19682
		dw 20106, 20520, 20926, 21325, 21716, 22101, 22479, 22850
		dw 23216, 23576, 23930, 24280, 24624, 24964, 25299, 25630
		dw 25956, 26279, 26597, 26912, 27223, 27531, 27835, 28136
		dw 28434, 28728, 29020, 29309, 29595, 29878, 30158, 30436
		dw 30712, 30985, 31255, 31524, 31790, 32053, 32315, 32575
		dw 32832, 33088, 33341, 33593, 33843, 34091, 34337, 34581
		dw 34824, 35065, 35304, 35542, 35778, 36013, 36246, 36477
		dw 36708, 36936, 37164, 37389, 37614, 37837, 38059, 38280
		dw 38499, 38717, 38934, 39150, 39364, 39578, 39790, 40001
		dw 40211, 40420, 40628, 40835, 41040, 41245, 41449, 41651
		dw 41853, 42054, 42254, 42452, 42650, 42847, 43043, 43239
		dw 43433, 43626, 43819, 44011, 44202, 44392, 44581, 44770
		dw 44957, 45144, 45330, 45516, 45700, 45884, 46068, 46250
		dw 46432, 46613, 46793, 46973, 47152, 47330, 47508, 47684
		dw 47861, 48036, 48211, 48386, 48559, 48733, 48905, 49077
		dw 49248, 49419, 49589, 49759, 49928, 50096, 50264, 50431
		dw 50598, 50764, 50930, 51095, 51259, 51423, 51587, 51750
		dw 51912, 52074, 52236, 52397, 52557, 52717, 52877, 53036
		dw 53194, 53352, 53510, 53667, 53824, 53980, 54136, 54291
		dw 54446, 54600, 54755, 54908, 55061, 55214, 55366, 55518
		dw 55670, 55821, 55971, 56122, 56272, 56421, 56570, 56719
		dw 56867, 57015, 57162, 57310, 57456, 57603, 57749, 57894
		dw 58040, 58185, 58329, 58473, 58617, 58761, 58904, 59047
		dw 59189, 59331, 59473, 59614, 59755, 59896, 60037, 60177
		dw 60317, 60456, 60595, 60734, 60873, 61011, 61149, 61286
		dw 61423, 61560, 61697, 61833, 61969, 62105, 62241, 62376
		dw 62511, 62645, 62779, 62913, 63047, 63181, 63314, 63447
		dw 63579, 63712, 63844, 63975, 64107, 64238, 64369, 64500
		dw 64630, 64760, 64890, 65020, 65149, 65278, 65407, 65535

		; Sample value bitshift after applying panned volume for pan
		; levels 0 - 31 (>= 32 is handled by code).

panshifttab	db 0, 5, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2
		db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
