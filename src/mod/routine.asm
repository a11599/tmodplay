;==============================================================================
; MOD player - Play routine
;------------------------------------------------------------------------------
; This is by no means perfect, ProTracker's playroutine is such a tangled mess
; that any attempt to introduce architecture and maintainability breaks it one
; way or another. This might be rewritten (or not) to align more with the
; original Amiga code to have a better coverage on ProTracker quirks. For now
; this is alright and should play most MODs just fine.
;------------------------------------------------------------------------------
; Unsupported effects / quirks:
; - Sample swap: Amiga hardware-specific
; - E0x - Set filter: Amiga hardware-specific
; - EFx - Funk it / Invert loop: Rare
;------------------------------------------------------------------------------
; Supported non-ProTracker effects:
; - 8xx - Set fine panning (xx = 00 - ff)
; - E8x - Set panning (x = 0 - f)
;==============================================================================

cpu 386

section .text

extern mod_file				; MOD file data
extern mod_dev				; Output device API jump table

%include "pmi/api/pmi.inc"
%include "rtl/api/string.inc"
%include "rtl/api/log.inc"

%include "mod/config.inc"
%include "mod/api/convert.inc"
%include "mod/structs/public.inc"
%include "mod/structs/mod_file.inc"
%include "mod/consts/dev.inc"
%include "mod/structs/dev.inc"

; Shortcut macros for easier access to nested structures

%define mod(var) mod_file + mod. %+ var
%define	channel(var) channel. %+ var
%define	sample(var) mod_sample. %+ var
%define	dev(fn) mod_dev + mod_dev_api. %+ fn
%define wflags(sample, mix) sample * 256 + mix

; Playroutine state flags

STATE_BREAK	EQU 0x01
STATE_SET_POS	EQU 0x02
STATE_SKIP	EQU 0x04

; Internal state of the play routine for each channel

struc		channel

.flags:
.mixer_flags	resb 1			; Mixer flags
.sample_flags	resb 1			; Sample flags
		resw 1			; Dummy filler for dword operations
.current_note	resb 1			; Note in current row
.note		resb 1			; Channel note
.volume		resb 1			; Channel volume (0 - 64)
.play_volume	resb 1			; Channel volume for playback
.pan		resb 1			; Channel pan (0 - 255)
.sample		resb 1			; Sample number (0 - 32)
.finetune	resb 1			; Channel finetune
.toneporta_dir	resb 1			; Tone portamento direction
.toneporta_spd	resb 1			; Tone portamento speed
.wave_control	resb 1			; Waveform control flags
.vibrato_pos	resb 1			; Vibrato waveform position
.vibrato_param	resb 1			; Vibrato command parameter
.tremolo_pos	resb 1			; Tremolo waveform position
.tremolo_param	resb 1			; Tremolo command parameter
.glissando_flag	resb 1			; Glissando enable / disable
		alignb 4
.sample_ofs	resd 1			; Sample offset position (memory)
.sample_pos	resd 1			; Sample playback start position
.period		resd 1			; Note period
.play_period	resd 1			; Note period for playback
.effect		resd 1			; Effect command (lo) / parameter (hi)
.toneporta_per	resd 1			; Wanted portamento note period
.sample_hdr_ofs	resd 1			; Offset to sample header
		alignb 4
.strucsize:

endstruc


;------------------------------------------------------------------------------
; Reset the playroutine.
;------------------------------------------------------------------------------

global mod_playroutine_init
mod_playroutine_init:
	push eax
	push ecx
	push edi

	cld

	; Zero out state memory

	mov edi, state
	mov ecx, STATE_SIZE / 4
	xor eax, eax
	rep stosd

	; Initialize global state

	mov al, [mod(bpm)]
	mov [bpm], al
	mov byte [speed], 6

	mov ah, [mod(bpm)]
	movzx ebx, ah
	shl ebx, 3
	lea ebx, [ebx * 2 + ebx]	; EBX: ticks per minute = BPM * 24
	call [dev(set_tick_rate)]

	xor eax, eax
	mov al, [mod(sequence)]
	mov [pattern], al
	mov eax, [mod(pattern_addr) + eax * 4]
	mov [pattern_addr], eax

	; Initialize channel state

	mov al, [mod(num_channels)]
	call [dev(set_channels)]

	pop edi
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Call effects as defined in an effect routine pointer table.
; -> %1 - Effect routine pointer table
;    DL - Effect command
;    DH - Effect parameter
; <- Destroys EBX
;------------------------------------------------------------------------------

%macro	call_fx 1

	test dx, dx
	jz %%no_fx
	xor ebx, ebx
	mov bl, dl
	cmp dl, 0xe0
	jb %%get_fx_fn
	sub bl, 0xd0

%%get_fx_fn:
	mov ebx, [%1 + ebx * 4]
	test ebx, ebx
	jz %%no_fx
	call ebx

%%no_fx:

%endmacro


;------------------------------------------------------------------------------
; Process a MOD "tick" event.
;------------------------------------------------------------------------------
; Ticks occur 6 times per pattern row normally, but this can be changed with
; the set speed (Fxx) command.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	align 4

global mod_playroutine_tick
mod_playroutine_tick:
	cmp byte [speed], 0		; Speed = 0 -> stop song
	je .exit

	; AL: channel number
	; ESI: channel[] pointer
	; EDI: pointer to current pattern row

	xor al, al
	mov esi, channels
	mov edi, [pattern_addr]

.loop_channels:

	; Reset flags

	mov dword [esi + channel(flags)], 0

	cmp byte [tick], 0
	jne .intra_tick

	;----------------------------------------------------------------------
	; First tick
	;----------------------------------------------------------------------

	; Pattern delay, process related commands only

	cmp byte [patt_del_count], 0
	ja .pattern_delay_tick

	; Extract pattern row content for current channel

	xor edx, edx
	mov ebx, [edi]			; BH: note, BL: instrument
	shld edx, ebx, 16		; DL: effect command, DH: effect param
	mov [esi + channel(effect)], edx
	mov [esi + channel(current_note)], bh

	test bl, bl
	jz .check_period

	;----------------------------------------------------------------------
	; Instrument number present

	movzx ebp, bl			; EBP: mod_sample[]
	dec ebp

	or dword [esi + channel(flags)], wflags(SMP_CHANGE, MIX_SET_VOL)

	; Copy sample settings into channel state

	mov ebp, ds:[mod(sample_hdr_ofs) + ebp * 4]
	mov ah, ds:[ebp + sample(volume)]
	mov [esi + channel(volume)], ah
	mov [esi + channel(play_volume)], ah
	mov ah, ds:[ebp + sample(finetune)]
	mov [esi + channel(finetune)], ah
	mov ah, [esi + channel(sample)]
	mov [esi + channel(sample)], bl

	; Instant change if:
	; - No instrument was played in the channel yet
	; - Note is also present without tone portamento effect (3xx)
	; - Instrument is same as the one currently playing

	test ah, ah
	jz .change_sample
	test bh, bh
	jnz .change_sample_tone_portamento
	cmp ah, bl
	je .change_sample

	; If instrument is not the same as the one currently playing, ProTracker
	; hot-swaps the sample when the current sample (loop) ends, but our
	; wavetables don't support this feature.

	jmp .change_sample

.change_sample_tone_portamento:
	cmp dl, 0x03
	je .check_period

.change_sample:

	; Instant instrument change

	mov [esi + channel(sample_hdr_ofs)], ebp
	cmp dl, 0x09
	je .check_period
	mov dword [esi + channel(sample_ofs)], 0

.check_period:
	test bh, bh
	jz .note_fx

	;----------------------------------------------------------------------
	; Handle effects that require a period in the note column

	cmp dl, 0xe5			; Set finetune
	je .fx_finetune_period
	cmp dl, 0x03			; Tone portamento
	je .fx_tone_portamento_set
	cmp dl, 0x05			; Tone portamento + volume slide
	je .fx_tone_portamento_set
	jmp .set_period

.fx_finetune_period:			; E5y: set finetune
	call fx_set_finetune
	jmp .set_period

.fx_tone_portamento_set:		; 3xy, 5xy: tone portamento (+ volslide)
	mov ah, [esi + channel(finetune)]
	call mod_note_finetune
	mov [esi + channel(toneporta_per)], ecx
	mov byte [esi + channel(toneporta_dir)], 0
	cmp ecx, [esi + channel(period)]
	je .fx_tone_portamento_clear
	ja .note_fx
	mov byte [esi + channel(toneporta_dir)], 1
	jmp .note_fx

.fx_tone_portamento_clear:
	mov dword [esi + channel(toneporta_per)], 0
	jmp .note_fx

.set_period:

	;----------------------------------------------------------------------
	; Apply period

	mov ah, [esi + channel(finetune)]
	call mod_note_finetune
	mov [esi + channel(note)], bh
	mov [esi + channel(period)], ecx
	mov [esi + channel(play_period)], ecx

	; Note delay, skip to effects

	cmp dl, 0xed
	je .note_fx

	;----------------------------------------------------------------------
	; Reset vibrato and tremolo waveform positions

	mov ah, [esi + channel(wave_control)]
	test ah, 0x04
	jnz .reset_tremolo_pos
	mov byte [esi + channel(vibrato_pos)], 0

.reset_tremolo_pos:
	mov ah, [esi + channel(wave_control)]
	test ah, 0x40
	jnz .set_wt_flags
	mov byte [esi + channel(tremolo_pos)], 0

.set_wt_flags:
	or dword [esi + channel(flags)], wflags((SMP_SET_POS | SMP_CHANGE), MIX_SET_SPD)
	xor ebp, ebp
	test bl, bl
	jnz .set_wt_sample_pos
	mov ebp, [esi + channel(sample_ofs)]

.set_wt_sample_pos:
	mov [esi + channel(sample_pos)], ebp
	jmp .note_fx

.pattern_delay_tick:

	;----------------------------------------------------------------------
	; Process effects in delayed pattern first tick

	mov edx, [esi + channel(effect)]
	call_fx fxtab_pattdel
	jmp .set_wt

.intra_tick:

	;----------------------------------------------------------------------
	; Process effects in subsequent ticks

	mov edx, [esi + channel(effect)]
	call_fx fxtab_intra
	jmp .set_wt

.note_fx:

	; Reset volume and period on each first tick (yes, this ruins vibrato
	; and tremolo, but that's how it works in ProTracker). To prevent
	; unnecessary mixer calls and the conversion from period to playback
	; speed, this will only be done if the playback volume and period do
	; not match the values from the pattern note.

	mov ah, [esi + channel(volume)]	; Reset volume
	cmp [esi + channel(play_volume)], ah
	je .check_period_reset
	mov [esi + channel(play_volume)], ah
	or byte [esi + channel(mixer_flags)], MIX_SET_VOL

.check_period_reset:
	mov ecx, [esi + channel(period)]; Reset period
	cmp [esi + channel(play_period)], ecx
	je .apply_note_fx
	mov [esi + channel(play_period)], ecx
	or byte [esi + channel(mixer_flags)], MIX_SET_SPD

.apply_note_fx:

	;----------------------------------------------------------------------
	; Process effects in the first tick

	call_fx fxtab_note

.set_wt:

	;----------------------------------------------------------------------
	; Program the wavetable

	mov ah, [esi + channel(sample_flags)]
	test ah, ah
	jz .set_mixer

	; Update playing sample, sample position

	push esi
	test ah, SMP_SET_POS
	jz .no_retrig

.no_retrig:
	mov ecx, [esi + channel(sample_pos)]
	xor edx, edx
	mov esi, [esi + channel(sample_hdr_ofs)]
	call [dev(set_sample)]
	pop esi

.set_mixer:
	mov ah, [esi + channel(mixer_flags)]
	test ah, ah
	jz .next_channel

	; Update volume and playback speed (period, finetune)

	mov bl, [esi + channel(play_volume)]
	mov bh, [esi + channel(pan)]
	mov ecx, [esi + channel(play_period)]
	call [dev(set_mixer)]

.next_channel:
	add esi, channel.strucsize
	add edi, 4
	inc al
	cmp al, [mod(num_channels)]
	jb .loop_channels

	; Next tick

	mov al, [tick]
	inc al
	cmp al, [speed]
	jae .next_row
	mov [tick], al

.exit:
	ret

.next_row:

	;----------------------------------------------------------------------
	; Row playback complete, step playback position forward

	mov byte [tick], 0

	; Check pattern delay

	mov al, [patt_delay]
	test al, al
	jz .check_pattern_delay
	add al, 1
	mov [patt_del_count], al
	mov byte [patt_delay], 0

.check_pattern_delay:
	cmp byte [patt_del_count], 0
	je .no_pattern_delay
	dec byte [patt_del_count]
	jnz .exit

.no_pattern_delay:
	test byte [flags], STATE_BREAK | STATE_SET_POS
	jnz .pattern_jump

.check_next_row:
	mov [pattern_addr], edi
	mov al, [row]
	inc al
	cmp al, 64
	jae .next_pattern
	mov [row], al
	jmp .exit

.next_pattern:
	mov byte [row], 0
	movzx ebx, byte [position]
	inc bl
	cmp bl, [mod(length)]
	jb .get_sequence
	mov bl, [mod(restart_pos)]

.get_sequence:
	mov [position], bl
	mov bl, byte [mod(sequence) + ebx]
	mov [pattern], bl
	mov eax, [mod(pattern_addr) + ebx * 4]
	mov [pattern_addr], eax
	jmp .exit

.pattern_jump:

	;----------------------------------------------------------------------
	; Pattern break or position jump effect encountered during playback,
	; calculate new position

	movzx ebx, byte [position]
	test byte [flags], STATE_SET_POS
	jnz .pattern_jump_pos
	inc bl				; No new song position -> next pattern

.check_jump_pos:
	cmp bl, [mod(length)]		; Restart song on position overflow
	jb .get_jump_sequence
	mov bl, [mod(restart_pos)]

.get_jump_sequence:
	mov [position], bl
	mov bl, [mod(sequence) + ebx]
	mov [pattern], bl		; Get new pattern number
	mov ebx, [mod(pattern_addr) + ebx * 4]
	movzx eax, byte [break_row]
	mov [row], al			; Set new row
	mul byte [mod(num_channels)]
	lea eax, [ebx + eax * 4]
	mov [pattern_addr], eax		; Set pattern offset
	and byte [flags], ~(STATE_BREAK | STATE_SET_POS)
	test byte [flags], STATE_SKIP
	jz .exit
	and byte [flags], ~(STATE_SKIP)
	mov edi, [pattern_addr]
	movzx eax, byte [mod(num_channels)]
	lea edi, [edi + eax * 4]
	jmp .check_next_row

.pattern_jump_pos:
	xor bl, bl			; Get new song position
	xchg bl, [break_position]
	jmp .check_jump_pos


;------------------------------------------------------------------------------
; Get the current position within the song.
;------------------------------------------------------------------------------
; <- AH - Sequence entry (starting with 0)
;    AL - Row within the sequence entry pattern (0 - 63)
;    DL - Current tick within the row
;------------------------------------------------------------------------------

global mod_playroutine_get_position
mod_playroutine_get_position:
	mov ah, [position]
	mov al, [row]
	mov dl, [tick]
	ret


;------------------------------------------------------------------------------
; Jump to a specific sequence entry within the song.
;------------------------------------------------------------------------------
; -> AH - New sequence entry (starting with 0)
;    AL - New row within the sequence entry pattern
;    DL - Set to 1 to stop currently playing samples
; <- AH - New sequence entry (starting with 0)
;    AL - New row within the sequence entry pattern (0 - 63)
;------------------------------------------------------------------------------

global mod_playroutine_set_position
mod_playroutine_set_position:
	push ebx
	push ecx
	push esi

	cmp al, 63			; Limit row to 63
	jbe .check_sequence
	mov al, 63

.check_sequence:
	cmp ah, [mod(length)]		; Limit sequence to song length
	jb .get_jump_sequence
	mov ah, [mod(length)]
	dec ah

.get_jump_sequence:
	and byte [flags], ~(STATE_BREAK | STATE_SET_POS | STATE_SKIP)
	xor ebx, ebx			; Set playroutine tick rate when BPM

	push eax

	test ax, ax			; Reset speed and BPM when jumping to
	jnz .stop_channels		; start of song
	call mod_playroutine_init	; Re-initialize playroutine
	call [dev(reset_channels)]	; Reset wavetable channels
	jmp .set_sequence

.stop_channels:
	test dl, dl			; Stop playback in all channels when
	jz .set_sequence		; requested (prevent stuck notes)

	mov al, [mod(num_channels)]	; Stop playback in all channels
	mov ah, 0x01			; Set sample
	xor esi, esi			; Stop sample in channel
	mov ebx, channels

.stop_channel_loop:
	dec al				; AL: channel number
	js .set_sequence
	mov byte [ebx + channel(sample)], 0
	mov dword [ebx + channel(sample_hdr_ofs)], 0
	call [dev(set_sample)]
	add ebx, channel.strucsize
	jmp .stop_channel_loop

.set_sequence:
	pop eax				; Restore target sequence and row number

	xor ebx, ebx
	mov bl, ah
	mov [position], ah
	mov bl, [mod(sequence) + ebx]
	mov [pattern], bl		; Get new pattern number
	mov ebx, [mod(pattern_addr) + ebx * 4]
	mov [row], al			; Set new row
	mov byte [tick], 0		; Set tick to 0
	xor ecx, ecx
	mov cl, al
	mul byte [mod(num_channels)]
	lea ecx, [ebx + ecx * 4]
	mov [pattern_addr], ecx		; Set pattern offset

.done:
	pop esi
	pop ecx
	pop ebx
	ret


;------------------------------------------------------------------------------
; Return information about channels.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_channel_info structures
; <- ESI - Filled with data
;------------------------------------------------------------------------------

	align 4

global mod_playroutine_get_channel_info
mod_playroutine_get_channel_info:
	push eax
	push ebx
	push esi
	push edi

	mov bl, [mod(num_channels)]
	mov edi, channels

.loop_channel:
	mov al, [edi + channel(sample)]
	mov [esi + mod_channel_info.sample], al
	mov al, [edi + channel(play_volume)]
	mov [esi + mod_channel_info.volume], al
	mov al, [edi + channel(pan)]
	mov [esi + mod_channel_info.pan], al
	mov eax, [edi + channel(play_period)]
	mov [esi + mod_channel_info.period], eax

	add edi, channel.strucsize
	add esi, mod_channel_info.strucsize
	dec bl
	jnz .loop_channel

	pop edi
	pop esi
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Return current playback position of the MOD file.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_position_info structure
; <- ESI - Filled with data
;------------------------------------------------------------------------------

	align 4

global mod_playroutine_get_position_info
mod_playroutine_get_position_info:
	push eax

	mov al, [position]
	mov [esi + mod_position_info.position], al
	mov al, [pattern]
	mov [esi + mod_position_info.pattern], al
	mov al, [row]
	mov [esi + mod_position_info.row], al
	mov al, [tick]
	mov [esi + mod_position_info.tick], al
	mov al, [speed]
	mov [esi + mod_position_info.speed], al
	mov al, [bpm]
	mov [esi + mod_position_info.bpm], al

	pop eax
	ret


;------------------------------------------------------------------------------
; Effects
;------------------------------------------------------------------------------
; -> DL - Effect command
;    DH - Effect parameter
;    ESI - channel[] pointer for current channel
; <- Must keep AL, ESI, EDI and segment registers, may trash everything else
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; 0xy - Arpeggio
; Change pitch to wanted note, then x, then y halfnotes higher on each tick.
;------------------------------------------------------------------------------

	align 4

fx_arpeggio:
	xor ebx, ebx
	mov bl, [tick]
	mov bl, [arptab + ebx]
	mov bh, [esi + channel(note)]
	cmp bl, 1
	jb .use_note
	ja .use_y
	mov bl, dh
	shr bl, 4
	jmp .use_note

.use_y:
	mov bl, dh
	and bl, 0x0f

.use_note:
	add bh, bl
	mov ah, [esi + channel(finetune)]
	call mod_note_finetune
	mov [esi + channel(play_period)], ecx
	or byte [esi + channel(mixer_flags)], MIX_SET_SPD
	ret


;------------------------------------------------------------------------------
; 1xx - Portamento up
; Increase current note pitch/decrease period by xx units.
;------------------------------------------------------------------------------

	align 4

fx_portamento_up:
	movzx edx, dh
	mov ecx, [esi + channel(period)]
	mod_convert_period edx		; EDX: period delta to subtract
	sub ecx, edx
	cmp ecx, mod_period(113)
	jge .set_period
	mov ecx, mod_period(113)

.set_period:
	mov [esi + channel(period)], ecx
	mov [esi + channel(play_period)], ecx
	or byte [esi + channel(mixer_flags)], MIX_SET_SPD
	ret


;------------------------------------------------------------------------------
; 2xx - Portamento down
; Decrease current note pitch/increase period by xx units.
;------------------------------------------------------------------------------

	align 4

fx_portamento_down:
	movzx edx, dh
	mov ecx, [esi + channel(period)]
	mod_convert_period edx		; EDX: period delta to add
	add ecx, edx
	cmp ecx, mod_period(856)
	jbe .set_period
	mov ecx, mod_period(856)

.set_period:
	mov [esi + channel(period)], ecx
	mov [esi + channel(play_period)], ecx
	or byte [esi + channel(mixer_flags)], MIX_SET_SPD
	ret


;------------------------------------------------------------------------------
; 3xx - Tone portamento
; Slide the pitch of the current note towards the specified destination note
; by xx units. If xx is zero, previously used value is applied.
;------------------------------------------------------------------------------

	align 4

fx_tone_portamento:
	test dh, dh			; Save portamento speed
	jz fx_tone_portamento_no_param
	mov [esi + channel(toneporta_spd)], dh

fx_tone_portamento_no_param:
	mov ebx, [esi + channel(toneporta_per)]
	test ebx, ebx			; EBX: target period
	jz .exit			; No target period, exit

	movzx edx, byte [esi + channel(toneporta_spd)]
	mov ecx, [esi + channel(period)]
	mod_convert_period edx		; EDX: period delta to add/subtract
	cmp byte [esi + channel(toneporta_dir)], 0
	jne .tone_portamento_up

	add ecx, edx			; Pitch down -> add delta to period
	cmp ecx, ebx
	jb .use_tone_portamento_period

.use_final_period:
	mov ecx, ebx			; Use target as period when reached
	mov [esi + channel(period)], ecx
	mov [esi + channel(play_period)], ecx
	or byte [esi + channel(mixer_flags)], MIX_SET_SPD
	mov dword [esi + channel(toneporta_per)], 0
	ret

	align 4

.tone_portamento_up:
	sub ecx, edx			; Pitch up -> subtract delta from period
	cmp ecx, ebx
	jle .use_final_period

.use_tone_portamento_period:
	mov [esi + channel(period)], ecx; Apply intermediate period

	cmp byte [esi + channel(glissando_flag)], 0
	je .no_glissando
	mov ah, [esi + channel(finetune)]
	call mod_period_floor_seminote	; Glissando on: round down to semitone

.no_glissando:
	mov [esi + channel(play_period)], ecx
	or byte [esi + channel(mixer_flags)], MIX_SET_SPD

.exit:
	ret


;------------------------------------------------------------------------------
; 4xy - Vibrato
; Apply vibrato with speed x and depth y on current note. Only alters the
; playback speed, does not change the stored pitch (period) for the channel. If
; either x or y is zero, previously used values are applied.
;------------------------------------------------------------------------------

	align 4

fx_vibrato:
	test dh, dh			; Update stored parameter
	jz fx_vibrato_no_param
	mov bl, [esi + channel(vibrato_param)]
	mov dl, dh
	and dl, 0x0f			; Update y
	jz .set_x_param
	and bl, 0xf0
	or bl, dl

.set_x_param:
	and dh, 0xf0			; Update x
	jz .update_memory
	and bl, 0x0f
	or bl, dh

.update_memory:
	mov [esi + channel(vibrato_param)], bl

fx_vibrato_no_param:
	mov bl, [esi + channel(vibrato_pos)]
	mov dl, bl			; DL: vibrato position
	shr bl, 2
	and bl, 0x1f
	mov bh, [esi + channel(wave_control)]
	and bh, 0x03			; BH: vibrato waveform
	jz .sine_wave
	shl bl, 3			; BL: adjusted vibrato position
	cmp bh, 0x01
	je .ramp_down
	mov bl, 255			; BL: vibrato multiplier
	jmp .use_multiplier		; Square wave

	align 4

.ramp_down:
	test dl, 0x80			; Sawtooth
	jz .use_multiplier
	xor bl, 255			; BL: vibrato multiplier (invert)
	jmp .use_multiplier

	align 4

.sine_wave:
	movzx ebx, bl			; Sine wave
	mov bl, [vibtab + ebx]		; BL: vibrato multiplier

.use_multiplier:
	mov dh, [esi + channel(vibrato_param)]
	mov bh, dh
	and bh, 0x0f			; BH: vibrato depth
	push eax			; Calculate period delta for vibrato
	xor eax, eax
	mov al, bl
	mul bh
	mov ebx, eax
	pop eax
	mod_convert_period ebx, -7	; EBX: period delta
	mov ecx, [esi + channel(period)]
	test dl, 0x80
	jnz .negative_wave
	add ecx, ebx			; Positive vibrato -> decrease pitch
	jmp .apply_delta

	align 4

.negative_wave:
	sub ecx, ebx			; Negative vibrato -> increase pitch

.apply_delta:
	mov [esi + channel(play_period)], ecx
	or byte [esi + channel(mixer_flags)], MIX_SET_SPD
	shr dh, 2			; Update vibrato position
	and dh, 0x3c
	add [esi + channel(vibrato_pos)], dh
	ret


;------------------------------------------------------------------------------
; 5xy - Volume slide with tone portamento
; Apply volume slide with given xy parameters and continues portamento with
; previously used tone portamento effect parameters (same as Axy with 300).
;------------------------------------------------------------------------------

	align 4

fx_tone_vol_slide:
	push edx			; Call tone portamento without parameter
	call fx_tone_portamento_no_param
	pop edx
	jmp fx_volume_slide		; Continue with volume slide


;------------------------------------------------------------------------------
; 6xy - Volume slide with vibrato
; Apply volume slide with given xy parameters and continues vibrato with
; previously used vibrato speed and depth parameters (same as Axy with 400).
;------------------------------------------------------------------------------

	align 4

fx_vibrato_vol_slide:
	push edx			; Call tone portamento without parameter
	call fx_vibrato_no_param
	pop edx
	jmp fx_volume_slide		; Continue with volume slide


;------------------------------------------------------------------------------
; 7xy - Tremolo
; Apply tremolo with speed x and depth y on current volume. Only alters the
; playback volume, does not change the stored volume for the channel. If
; either x or y is zero, previously used values are applied.
;------------------------------------------------------------------------------

	align 4

fx_tremolo:
	test dh, dh			; Update stored parameter
	jz .no_param
	mov bl, [esi + channel(tremolo_param)]
	mov dl, dh
	and dl, 0x0f			; Update y
	jz .set_x_param
	and bl, 0xf0
	or bl, dl

.set_x_param:
	and dh, 0xf0			; Update x
	jz .update_memory
	and bl, 0x0f
	or bl, dh

.update_memory:
	mov [esi + channel(tremolo_param)], bl

.no_param:
	mov bl, [esi + channel(tremolo_pos)]
	mov dl, bl			; DL: tremolo position
	shr bl, 2
	and bl, 0x1f
	mov bh, [esi + channel(wave_control)]
	and bh, 0x30			; BH: tremolo waveform
	jz .sine_wave
	shl bl, 3			; BL: adjusted tremolo position
	cmp bh, 0x10
	je .ramp_down
	mov bl, 255			; BL: tremolo multiplier
	jmp .use_multiplier		; Square wave

	align 4

.ramp_down:
	test byte [esi + channel(vibrato_pos)], 0x80 ; Sawtooth
	; ^^^ Yes, this is a copy-paste monster from ProTracker. They forgot to
	; update the variable name. The correct command here would be:
	; test dl, 0x80
	jz .use_multiplier
	xor bl, 255			; BL: tremolo multiplier (invert)
	jmp .use_multiplier

	align 4

.sine_wave:
	movzx ebx, bl			; Sine wave
	mov bl, [vibtab + ebx]		; BL: tremolo multiplier

.use_multiplier:
	mov dh, [esi + channel(tremolo_param)]
	mov bh, dh
	and bh, 0x0f			; BH: tremolo depth
	push eax			; Calculate period delta for tremolo
	movzx eax, bl
	mul bh
	mov ebx, eax
	pop eax
	shr ebx, 6			; BL: volume delta
	mov bh, [esi + channel(volume)]	; BH: volume
	test dl, 0x80
	jnz .negative_wave
	add bh, bl			; Positive tremolo -> decrease volume
	cmp bh, 64			; Limit highest volume to 64
	jbe .apply_delta
	mov bh, 64
	jmp .apply_delta

	align 4

.negative_wave:
	sub bh, bl			; Negative tremolo -> increase volume
	jns .apply_delta		; Limit lowest volume to 0
	xor bh, bh

.apply_delta:
	mov [esi + channel(play_volume)], bh
	or byte [esi + channel(mixer_flags)], MIX_SET_VOL
	shr dh, 2			; Update tremolo position
	and dh, 0x3c
	add [esi + channel(tremolo_pos)], dh
	ret


;------------------------------------------------------------------------------
; 8xx - Set channel fine panning
; Set the panning of the channel to x, where 00 is far left and FF is far
; right.
;------------------------------------------------------------------------------

	align 4

fx_set_fine_panning:
	mov [esi + channel(pan)], dh
	or byte [esi + channel(mixer_flags)], MIX_SET_PAN
	ret


;------------------------------------------------------------------------------
; 9xx - Sample offset
; Play the instrument from the xxth byte sample offset.
;------------------------------------------------------------------------------

	align 4

fx_sample_offset:
	test dh, dh
	jz .no_offset			; No new position, do nothing
	xor ebx, ebx
	mov bh, dh			; New position, set it
	mov [esi + channel(sample_ofs)], ebx
	mov [esi + channel(sample_pos)], ebx
	ret

.no_offset:
	mov ebx, [esi + channel(sample_ofs)]
	mov [esi + channel(sample_pos)], ebx
	ret


;------------------------------------------------------------------------------
; Axy - Volume slide
; Slide the volume up or down. y decreases and x increases the volume. When
; both are present, x (slide volume down) takes precedence.
;------------------------------------------------------------------------------

	align 4

fx_volume_slide:
	test dh, 0x0f
	jnz .slide_down
	shr dh, 4			; Slide up
	add dh, [esi + channel(volume)]
	cmp dh, 64
	jbe .set_volume

.slide_down:
	mov dl, dh			; Slide down
	and dl, 0x0f
	mov dh, [esi + channel(volume)]
	sub dh, dl
	jns .set_volume
	xor dh, dh

.set_volume:
	mov [esi + channel(volume)], dh	; Apply new volume
	mov [esi + channel(play_volume)], dh
	or byte [esi + channel(mixer_flags)], MIX_SET_VOL
	ret


;------------------------------------------------------------------------------
; Bxx - Position jump
; Jump to the first row of the pattern specified in position xx. If the pattern
; break effect is present in a following channel, the row number specified in
; the Dxx command will be used as the target.
;------------------------------------------------------------------------------

	align 4

fx_position_jump:
	mov [break_position], dh	; Save target position
	mov byte [break_row], 0
	or byte [flags], STATE_BREAK | STATE_SET_POS
	ret


;------------------------------------------------------------------------------
; Cxx - Set volume
; Set the volume of the channel to xx.
;------------------------------------------------------------------------------

	align 4

fx_set_volume:
	cmp dh, 64
	jbe .use_volume
	mov dh, 64

.use_volume:
	or byte [esi + channel(mixer_flags)], MIX_SET_VOL
	mov [esi + channel(volume)], dh
	mov [esi + channel(play_volume)], dh
	ret


;------------------------------------------------------------------------------
; Dxx - Pattern break
; Jump to row xx of the next pattern in the sequence. Parameter xx is a weirdo
; BCD-like value, the upper nibble is treated as 10-base but the lower is added
; as hexadecimal. For example xx = 32 jumps to row 32 and xx = 1f jumps to row
; 25 (10 + 15). If a following channel has Bxx effect, the target row is reset
; to 0.
;------------------------------------------------------------------------------

	align 4

fx_pattern_break:
	mov dl, dh			; Convert the weirdo BCD-like parameter
	and dl, 0x0f			; DL: lower nibble decimal value
	shr dh, 3
	and dh, 0x0e			; DH: (upper nibble decimal value) * 2
	add dl, dh			; DL: lower + upper * 2
	shl dh, 2			; DH: (upper nibble decimal value) * 8
	add dl, dh			; DL: lower + upper * 2 + upper * 8
	cmp dl, 63
	jbe .use_row
	xor dl, dl

.use_row:
	mov [break_row], dl
	or byte [flags], STATE_BREAK
	cmp byte [patt_del_count], 0
	je .exit
	or byte [flags], STATE_SKIP

.exit:
	ret


;------------------------------------------------------------------------------
; Fxx - Set speed / tempo
; Set the speed (ticks per row) if xx < 20 (hex) or tempo (BPM) if xx >= 20.
;------------------------------------------------------------------------------

	align 4

fx_set_speed:
	cmp dh, 0x20
	jae .set_bpm
	mov [speed], dh			; Set ticks per row
	ret

	align 4

.set_bpm:
	mov [bpm], dh			; Set tempo (BPM)
	movzx ebx, dh
	shl ebx, 3
	lea ebx, [ebx * 2 + ebx]	; EBX: ticks per minute = BPM * 24
	call [dev(set_tick_rate)]
	ret


;------------------------------------------------------------------------------
; E3x - Glissando control
; Controls whether tone portamento performs slides in semitones or not.
; x = 0: Disable glissando (default slide)
; x = 1: Enable glissando (slide by semitones)
;------------------------------------------------------------------------------

	align 4

fx_glissando_ctrl:
	mov [esi + channel(glissando_flag)], dh
	ret


;------------------------------------------------------------------------------
; E4x - Set vibrato waveform
; Set the waveform of the vibrato command to x, where:
; x = 0: sine (default)
; x = 1: sawtooth
; x = 2: square
; The waveforms restart if a note is specified. Add 4 to ignore the note. Then
; the waveforms will continue through the new note.
;------------------------------------------------------------------------------

	align 4

fx_set_vibrato_ctrl:
	and dh, 0x0f
	mov ah, [esi + channel(wave_control)]
	and ah, 0xf0
	or ah, dh
	mov [esi + channel(wave_control)], ah
	ret


;------------------------------------------------------------------------------
; E5x - Set finetune
; Set finetune. Applies to current playing note only, does not change the
; sample preset finetune value.
; This effect may not make destructive changes to any registers as it is called
; from critical parts of the playroutine.
;------------------------------------------------------------------------------

	align 4

fx_set_finetune:
	shl dh, 4			; Extend to signed small int - safe,
	sar dh, 4			; since it's done anyways for this fx
	mov [esi + channel(finetune)], dh
	ret


;------------------------------------------------------------------------------
; E6x - Pattern loop
; Set loop start position within pattern if x = 0, repeat pattern between
; start position and end of current row x times if x > 0.
;------------------------------------------------------------------------------

	align 4

fx_pattern_loop:
	test dh, dh
	jz .set_loop
	mov ah, [loop_count]
	test ah, ah
	jz .start_loop
	dec ah
	mov [loop_count], ah
	jz .exit

.loop_jump:
	mov dl, [loop_start_row]
	mov [break_row], dl
	mov dl, [position]
	mov [break_position], dl
	or byte [flags], STATE_SET_POS | STATE_BREAK

.exit:
	ret

	align 4

.start_loop:
	mov [loop_count], dh
	jmp .loop_jump

	align 4

.set_loop:
	mov ah, [row]
	mov [loop_start_row], ah
	jmp .exit


;------------------------------------------------------------------------------
; E7x - Set tremolo waveform
; Set the waveform of the tremolo command to x, where:
; x = 0: sine (default)
; x = 1: sawtooth
; x = 2: square
; The waveforms restart if a note is specified. Add 4 to ignore the note. Then
; the waveforms will continue through the new note.
;------------------------------------------------------------------------------

	align 4

fx_set_tremolo_ctrl:
	and dh, 0x0f
	shl dh, 4
	mov ah, [esi + channel(wave_control)]
	and ah, 0x0f
	or ah, dh
	mov [esi + channel(wave_control)], ah
	ret


;------------------------------------------------------------------------------
; E8x - Set channel panning
; Set the panning of the channel to x, where 0 is far left and F is far right
;------------------------------------------------------------------------------

	align 4

fx_set_panning:
	mov ah, dh
	shl ah, 4
	or ah, dh
	mov [esi + channel(pan)], ah
	or byte [esi + channel(mixer_flags)], MIX_SET_PAN
	ret


;------------------------------------------------------------------------------
; E9x - Retrig note
; Reset sample position every x ticks.
;------------------------------------------------------------------------------

	align 4

fx_retrig_note:
	cmp dh, 1
	jb .exit			; x = 0: do nothing
	je .retrig			; x = 1: retrig each tick
	push eax
	xor eax, eax
	mov al, [tick]
	div dh
	mov dh, ah			; DH: tick MOD x
	pop eax
	test dh, dh			; Remainder: not xth tick
	jnz .exit

.retrig:
	mov dword [esi + channel(sample_pos)], 0
	mov ecx, [esi + channel(period)]
	mov [esi + channel(play_period)], ecx
	or dword [esi + channel(flags)], wflags(SMP_SET_POS, MIX_SET_SPD)

.exit:
	ret


;------------------------------------------------------------------------------
; EAx - Fine volume slide up
; Slide volume up by x.
;------------------------------------------------------------------------------

	align 4

fx_fine_vol_up:
	shl dh, 4
	jmp fx_volume_slide


;------------------------------------------------------------------------------
; EBx - Fine volume slide down
; Slide volume down by x.
;------------------------------------------------------------------------------

	align 4

fx_fine_vol_down:
	and dh, 0x0f
	jmp fx_volume_slide


;------------------------------------------------------------------------------
; ECx - Note cut
; Set the volume of the channel to 0 after x ticks.
;------------------------------------------------------------------------------

	align 4

fx_note_cut:
	cmp [tick], dh
	jne .exit
	mov byte [esi + channel(volume)], 0
	mov byte [esi + channel(play_volume)], 0
	or byte [esi + channel(mixer_flags)], MIX_SET_VOL

.exit:
	ret


;------------------------------------------------------------------------------
; EDx - Note delay
; Delay start of the instrument for x ticks.
;------------------------------------------------------------------------------

	align 4

fx_note_delay:
	cmp [tick], dh
	jne .exit
	cmp byte [esi + channel(current_note)], 0
	je .exit
	mov dword [esi + channel(sample_pos)], 0
	mov ecx, [esi + channel(period)]
	mov [esi + channel(play_period)], ecx
	or dword [esi + channel(flags)], wflags(SMP_SET_POS, MIX_SET_SPD)

.exit:
	ret


;------------------------------------------------------------------------------
; EEx - Pattern delay
; Repeat the current row x times without retriggering notes.
;------------------------------------------------------------------------------

	align 4

fx_pattern_delay:
	mov [patt_delay], dh
	ret


;==============================================================================
; Data area
;==============================================================================

section .data

		; Effects which should be processed on the first tick

fxtab_note	dd 0			; 0xy: arpeggio
		dd 0			; 1xx: portamento up
		dd 0			; 2xx: portamento down
		dd 0			; 3xx: tone portamento
		dd 0			; 4xy: vibrato
		dd 0			; 5xy: volume slide + tone portamento
		dd 0			; 6xy: volume slide + vibrato
		dd 0			; 7xy: tremolo
		dd fx_set_fine_panning	; 8xx: set fine panning
		dd fx_sample_offset	; 9xx: sample offset
		dd 0			; Axy: volume slide
		dd fx_position_jump	; Bxx: position jump
		dd fx_set_volume	; Cxx: set volume
		dd fx_pattern_break	; Dxx: pattern break
		dd 0			; Padding
		dd fx_set_speed		; Fxx: set speed / tempo
		dd 0			; E0x: set filter
		dd fx_portamento_up	; E1x: fine portamento up
		dd fx_portamento_down	; E2x: fine portamento down
		dd fx_glissando_ctrl	; E3x: glissando control
		dd fx_set_vibrato_ctrl	; E4x: set vibrato waveform
		dd fx_set_finetune	; E5x: set finetune
		dd fx_pattern_loop	; E6x: pattern loop
		dd fx_set_tremolo_ctrl	; E7x: set tremolo waveform
		dd fx_set_panning	; E8x: set panning
		dd fx_retrig_note	; E9x: retrigger
		dd fx_fine_vol_up	; EAx: fine volume slide up
		dd fx_fine_vol_down	; EBx: fine volume slide down
		dd fx_note_cut		; ECx: note cut
		dd fx_note_delay	; EDx: note delay
		dd fx_pattern_delay	; EEx: pattern delay
		dd 0			; EFx: invert loop

		; Effects which should be processed on subsequent ticks

fxtab_intra	dd fx_arpeggio		; 0xy: arpeggio
		dd fx_portamento_up	; 1xx: portamento up
		dd fx_portamento_down	; 2xx: portamento down
		dd fx_tone_portamento	; 3xx: tone portamento
		dd fx_vibrato		; 4xy: vibrato
		dd fx_tone_vol_slide	; 5xy: volume slide + tone portamento
		dd fx_vibrato_vol_slide	; 6xy: volume slide + vibrato
		dd fx_tremolo		; 7xy: tremolo
		dd 0			; 8xx: set panning
		dd 0			; 9xx: sample offset
		dd fx_volume_slide	; Axy: volume slide
		dd 0			; Bxx: position jump
		dd 0			; Cxx: set volume
		dd 0			; Dxx: pattern break
		dd 0			; Padding
		dd 0			; Fxx: set speed / tempo
		dd 0			; E0x: set filter
		dd 0			; E1x: fine portamento up
		dd 0			; E2x: fine portamento down
		dd 0			; E3x: glissando control
		dd 0			; E4x: set vibrato waveform
		dd 0			; E5x: set finetune
		dd 0			; E6x: pattern loop
		dd 0			; E7x: set tremolo waveform
		dd 0			; E8x: set panning
		dd fx_retrig_note	; E9x: retrigger
		dd 0			; EAx: fine volume slide up
		dd 0			; EBx: fine volume slide down
		dd fx_note_cut		; ECx: note cut
		dd fx_note_delay	; EDx: note delay
		dd 0			; EEx: pattern delay
		dd 0			; EFx: invert loop

		; Effects which should be processed on the first tick during
		; pattern delay

fxtab_pattdel	dd fx_arpeggio		; 0xy: arpeggio
		dd fx_portamento_up	; 1xx: portamento up
		dd fx_portamento_down	; 2xx: portamento down
		dd fx_tone_portamento	; 3xx: tone portamento
		dd fx_vibrato		; 4xy: vibrato
		dd fx_tone_vol_slide	; 5xy: volume slide + tone portamento
		dd fx_vibrato_vol_slide	; 6xy: volume slide + vibrato
		dd fx_tremolo		; 7xy: tremolo
		dd 0			; 8xx: set panning
		dd 0			; 9xx: sample offset
		dd fx_volume_slide	; Axy: volume slide
		dd fx_position_jump	; Bxx: position jump
		dd 0			; Cxx: set volume
		dd fx_pattern_break	; Dxx: pattern break
		dd 0			; Padding
		dd 0			; Fxx: set speed / tempo
		dd 0			; E0x: set filter
		dd fx_portamento_up	; E1x: fine portamento up
		dd fx_portamento_down	; E2x: fine portamento down
		dd 0			; E3x: glissando control
		dd 0			; E4x: set vibrato waveform
		dd 0			; E5x: set finetune
		dd 0			; E6x: pattern loop
		dd 0			; E7x: set tremolo waveform
		dd 0			; E8x: set panning
		dd 0			; E9x: retrigger
		dd fx_fine_vol_up	; EAx: fine volume slide up
		dd fx_fine_vol_down	; EBx: fine volume slide down
		dd 0			; ECx: note cut
		dd 0			; EDx: note delay
		dd 0			; EEx: pattern delay
		dd 0			; EFx: invert loop

		; Vibrato/tremolo sine table

vibtab		db   0,  24,  49,  74,  97, 120, 141, 161
		db 180, 197, 212, 224, 235, 244, 250, 253
		db 255, 253, 250, 244, 235, 224, 212, 197
		db 180, 161, 141, 120,  97,  74,  49,  24

		; Arpeggio effect note "offset" table

arptab		db 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2
		db 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2

section .bss

state:
pattern_addr	resd 1			; Current playing pattern address
channels	resb (channel.strucsize * MOD_MAX_CHANS)

flags		resb 1			; Playback state flags
bpm		resb 1			; Current BPM
speed		resb 1			; Current speed (ticks/row)
tick		resb 1			; Current tick
pattern		resb 1			; Current playing pattern
row		resb 1			; Current playing row
position	resb 1			; Current (pattern) sequence position
break_row	resb 1			; Wanted row after break
break_position	resb 1			; Wanted sequence position after break
patt_delay	resb 1			; Wanted pattern delay
patt_del_count	resb 1			; Pattern delay counter
loop_start_row	resb 1			; Start row of pattern loop
loop_count	resb 1			; Pattern loop count
		alignb 4
		STATE_SIZE EQU $ - state
