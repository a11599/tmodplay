;==============================================================================
; MOD player - play routine
;------------------------------------------------------------------------------
; This is by no means perfect, ProTracker's playroutine is such a tangled mess
; that any attempt to introduce architecture and maintainability breaks it one
; way or another. This will eventually be rewritten to align more with the
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

%include "mod/structs/global.inc"
%include "mod/consts/routine.inc"
%include "mod/consts/out.inc"
%include "mod/api/convert.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	state(var) mod.routine_state + mod_routine_state. %+ var
%define	channel(var) mod_routine_channel. %+ var
%define	sample(var) mod_sample. %+ var
%define	out_fn(fn) mod.out_fns + mod_out_fns. %+ fn
%define wflags(sample, mix) sample * 256 + mix

segment modplayer public use16 class=CODE align=16
segment modplayer


;------------------------------------------------------------------------------
; Reset the playroutine.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

global mod_playroutine_reset
mod_playroutine_reset:
	push eax
	push cx
	push si
	push di
	push es

	cld

	; Zero out state memory

	mov ax, ds
	mov es, ax
	mov di, mod.routine_state
	mov cx, mod_routine_state.strucsize / 2
	xor ax, ax
	rep stosw

	; Initialize global state

	mov al, [mod.bpm]
	mov [state(bpm)], al
	mov byte [state(speed)], 6

	mov ah, [mod.bpm]
	mov bl, ah
	xor bh, bh
	shl bx, 3
	mov cx, bx
	add bx, cx
	add bx, cx			; BX: ticks per minute = BPM * 24
	call [out_fn(set_tick_rate)]

	movzx eax, byte [mod.sequence]
	mov [state(pattern)], al
	mov eax, [mod.pattern_addr + eax * 4]
	mov [state(pattern_addr)], eax

	; Initialize channel state

	mov cl, [mod.num_channels]
	xor ch, ch
	mov si, state(channels)

.loop_reset_channel:
	mov byte [si + channel(volume)], 64
	mov byte [si + channel(play_volume)], 64
	mov word [si + channel(sample_hdr_ofs)], -1
	add si, mod_routine_channel.strucsize
	dec cx
	jnz .loop_reset_channel

	pop es
	pop di
	pop si
	pop cx
	pop eax
	retn


;------------------------------------------------------------------------------
; Call effects as defined in an effect routine pointer table.
; -> FS:%1 - Effect routine pointer table
;    DL - Effect command
; <- Destroys BX
;------------------------------------------------------------------------------

%macro	call_fx 1

	test dx, dx
	jz %%no_fx
	mov bl, dl
	cmp bl, 0xe0
	jb %%get_fx_fn
	sub bl, 0xd0

%%get_fx_fn:
	xor bh, bh
	add bx, bx
	mov bx, fs:[%1 + bx]
	test bx, bx
	jz %%no_fx
	call bx

%%no_fx:

%endmacro


;------------------------------------------------------------------------------
; Process a MOD "tick" event.
;------------------------------------------------------------------------------
; Ticks occur 6 times per pattern row normally, but this can be changed with
; the set speed (Fxx) command.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	align 4

global mod_playroutine_tick
mod_playroutine_tick:
	push fs

	cmp byte [state(speed)], 0	; Speed = 0 -> stop song
	je .exit

	; AL: channel number
	; DS:SI: mod_routine_channel[] pointer
	; DS:EDI: pointer to current pattern row
	; FS: modplayer_data segment

	mov ax, modplayer_data
	mov fs, ax
	xor al, al
	mov si, state(channels)
	mov edi, [state(pattern_addr)]

	align 4

.loop_channels:

	; Reset flags

	mov word [si + channel(flags)], 0

	cmp byte [state(tick)], 0
	jne .intra_tick

	;----------------------------------------------------------------------
	; First tick
	;----------------------------------------------------------------------

	; Pattern delay, process related commands only

	cmp byte [state(patt_del_count)], 0
	ja .pattern_delay_tick

	; Extract pattern row content for current channel

	mov bx, [edi]			; BH: note, BL: instrument
	mov dx, [edi + 2]		; DL: effect command, DH: effect param
	mov [si + channel(effect)], dx	; Save effect
	mov [si + channel(current_note)], bh

	test bl, bl
	jz .check_period

	;----------------------------------------------------------------------
	; Instrument number present

	mov bp, bx			; BP: mod_sample[]
	and bp, 0xff
	dec bp
	add bp, bp

	or word [si + channel(flags)], wflags(SMP_CHANGE, MIX_SET_VOL)

	; Copy sample settings into channel state

	mov bp, ds:[mod.sample_hdr_ofs + bp]
	mov ah, ds:[bp + sample(volume)]
	mov [si + channel(volume)], ah
	mov [si + channel(play_volume)], ah
	mov ah, ds:[bp + sample(finetune)]
	mov [si + channel(finetune)], ah
	mov ah, [si + channel(sample)]
	mov [si + channel(sample)], bl

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

	; Instrument is not the same as the one currently playing, ProTracker
	; hot-swaps the sample when the current sample (loop) ends, but our
	; wavetables don't support this feature.

	jmp .change_sample

	align 4

.change_sample_tone_portamento:
	cmp dl, 0x03
	je .check_period

.change_sample:

	; Instant instrument change

	mov [si + channel(sample_hdr_ofs)], bp
	cmp dl, 0x09
	je .check_period
	mov word [si + channel(sample_ofs)], 0

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

	align 4

.fx_finetune_period:			; E5y: set finetune
	call fx_set_finetune
	jmp .set_period

	align 4

.fx_tone_portamento_set:		; 3xy, 5xy: tone portamento (+ volslide)
	mov ah, [si + channel(finetune)]
	call mod_note_finetune
	mov [si + channel(toneporta_per)], cx
	mov byte [si + channel(toneporta_dir)], 0
	cmp cx, [si + channel(period)]
	je .fx_tone_portamento_clear
	ja .note_fx
	mov byte [si + channel(toneporta_dir)], 1
	jmp .note_fx

	align 4

.fx_tone_portamento_clear:
	mov word [si + channel(toneporta_per)], 0
	jmp .note_fx

	align 4

.set_period:

	;----------------------------------------------------------------------
	; Apply period

	mov ah, [si + channel(finetune)]
	call mod_note_finetune
	mov [si + channel(note)], bh	; Save note
	mov [si + channel(period)], cx	; Save period
	mov [si + channel(play_period)], cx

	; Note delay, skip to effects

	cmp dl, 0xed
	je .note_fx

	;----------------------------------------------------------------------
	; Reset vibrato and tremolo waveform positions

	mov ah, [si + channel(wave_control)]
	test ah, 0x04
	jnz .reset_tremolo_pos
	mov byte [si + channel(vibrato_pos)], 0

.reset_tremolo_pos:
	mov ah, [si + channel(wave_control)]
	test ah, 0x40
	jnz .set_wt_flags
	mov byte [si + channel(tremolo_pos)], 0

.set_wt_flags:
	or word [si + channel(flags)], wflags((SMP_SET_POS | SMP_CHANGE), MIX_SET_SPD)
	xor bp, bp
	test bl, bl
	jnz .set_wt_sample_pos
	mov bp, [si + channel(sample_ofs)]

.set_wt_sample_pos:
	mov word [si + channel(sample_pos)], bp
	jmp .note_fx

	align 4

.pattern_delay_tick:

	;----------------------------------------------------------------------
	; Process effects in delayed pattern first tick

	mov dx, [si + channel(effect)]
	call_fx fxtab_pattdel
	jmp .set_wt

	align 4

.intra_tick:

	;----------------------------------------------------------------------
	; Process effects in subsequent ticks

	mov dx, [si + channel(effect)]
	call_fx fxtab_intra
	jmp .set_wt

	align 4

.note_fx:

	; Reset volume and period on each first tick (yes, this ruins vibrato
	; and tremolo, but that's how it works in ProTracker). To prevent
	; unnecessary mixer calls and the conversion from period to playback
	; speed, this will only be done if the playback volume and period do
	; not match the values from the pattern note.

	mov ah, [si + channel(volume)]	; Reset volume
	cmp [si + channel(play_volume)], ah
	je .check_period_reset
	mov [si + channel(play_volume)], ah
	or byte [si + channel(mixer_flags)], MIX_SET_VOL

.check_period_reset:
	mov cx, [si + channel(period)]	; Reset period
	cmp [si + channel(play_period)], cx
	je .apply_note_fx
	mov [si + channel(play_period)], cx
	or byte [si + channel(mixer_flags)], MIX_SET_SPD

.apply_note_fx:

	;----------------------------------------------------------------------
	; Process effects in the first tick

	call_fx fxtab_note

.set_wt:

	;----------------------------------------------------------------------
	; Program the wavetable

	mov ah, [si + channel(sample_flags)]
	test ah, ah
	jz .set_mixer

	; Update playing sample, sample position

	push si
	test ah, SMP_SET_POS
	jz .no_retrig

.no_retrig:
	movzx ecx, word [si + channel(sample_pos)]
	xor dx, dx
	mov si, [si + channel(sample_hdr_ofs)]
	call [out_fn(set_sample)]
	pop si

.set_mixer:
	mov ah, [si + channel(mixer_flags)]
	test ah, ah
	jz .next_channel

	; Update volume and playback speed (period, finetune)

	mov bl, [si + channel(play_volume)]
	mov bh, [si + channel(pan)]
	mov cx, [si + channel(play_period)]
	call [out_fn(set_mixer)]

.next_channel:
	add si, mod_routine_channel.strucsize
	add edi, 4
	inc al
	cmp al, [mod.num_channels]
	jb .loop_channels

	; Next tick

	mov al, [state(tick)]
	inc al
	cmp al, [state(speed)]
	jae .next_row
	mov [state(tick)], al

.exit:
	pop fs
	retn

.next_row:

	;----------------------------------------------------------------------
	; Row playback complete, step playback position forward

	mov byte [state(tick)], 0

	; Check pattern delay

	mov al, [state(patt_delay)]
	test al, al
	jz .check_pattern_delay
	add al, 1
	mov [state(patt_del_count)], al
	mov byte [state(patt_delay)], 0

.check_pattern_delay:
	cmp byte [state(patt_del_count)], 0
	je .no_pattern_delay
	sub byte [state(patt_del_count)], 1
	jnz .exit

.no_pattern_delay:
	test byte [state(flags)], STATE_BREAK | STATE_SET_POS
	jnz .pattern_jump

.check_next_row:
	mov [state(pattern_addr)], edi
	mov al, [state(row)]
	inc al
	cmp al, 64
	jae .next_pattern
	mov [state(row)], al
	jmp .exit

.next_pattern:
	mov byte [state(row)], 0
	movzx ebx, byte [state(position)]
	inc bl
	cmp bl, [mod.length]
	jb .get_sequence
	mov bl, [mod.restart_pos]

.get_sequence:
	mov [state(position)], bl
	mov bl, byte [mod.sequence + bx]
	mov [state(pattern)], bl
	mov eax, [mod.pattern_addr + ebx * 4]
	mov [state(pattern_addr)], eax
	jmp .exit

.pattern_jump:

	;----------------------------------------------------------------------
	; Pattern break or position jump effect encountered during playback,
	; calculate new position

	movzx ebx, byte [state(position)]
	test byte [state(flags)], STATE_SET_POS
	jnz .pattern_jump_pos
	inc bl				; No new song position -> next pattern

.check_jump_pos:
	cmp bl, [mod.length]		; Restart song on position overflow
	jb .get_jump_sequence
	mov bl, [mod.restart_pos]

.get_jump_sequence:
	mov [state(position)], bl
	mov bl, [mod.sequence + bx]
	mov [state(pattern)], bl	; Get new pattern number
	mov ebx, [mod.pattern_addr + ebx * 4]
	movzx eax, byte [state(break_row)]
	mov [state(row)], al		; Set new row
	mul byte [mod.num_channels]
	lea eax, [ebx + eax * 4]
	mov [state(pattern_addr)], eax	; Set pattern offset
	and byte [state(flags)], ~(STATE_BREAK | STATE_SET_POS)
	test byte [state(flags)], STATE_SKIP
	jz .exit
	and byte [state(flags)], ~(STATE_SKIP)
	mov edi, [state(pattern_addr)]
	movzx eax, byte [mod.num_channels]
	lea edi, [edi + eax * 4]
	jmp .check_next_row

.pattern_jump_pos:
	xor bl, bl			; Get new song position
	xchg bl, [state(break_position)]
	jmp .check_jump_pos


;------------------------------------------------------------------------------
; Effects
;------------------------------------------------------------------------------
; -> DL - Effect command
;    DH - Effect parameter
;    DS - Player instance segment
;    DS:SI - mod_routine_channel[] pointer for current channel
;    FS - modplayer_data segment
; <- Must keep AL, SI, EDI and segment registers, may trash everything else
;------------------------------------------------------------------------------


;------------------------------------------------------------------------------
; 0xy - Arpeggio
; Change pitch to wanted note, then x, then y halfnotes higher on each tick.
;------------------------------------------------------------------------------

	align 4

fx_arpeggio:
	mov bl, [state(tick)]
	xor bh, bh
	mov bl, fs:[arptab + bx]
	mov bh, [si + channel(note)]
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
	mov ah, [si + channel(finetune)]
	call mod_note_finetune
	mov [si + channel(play_period)], cx
	or byte [si + channel(mixer_flags)], MIX_SET_SPD
	retn


;------------------------------------------------------------------------------
; 1xx - Portamento up
; Increase current note pitch/decrease period by xx units.
;------------------------------------------------------------------------------

	align 4

fx_portamento_up:
	mov cx, [si + channel(period)]	; CX: period
	mov dl, dh
	xor dh, dh
	mod_convert_period dx		; DX: period delta to subtract
	sub cx, dx
	cmp cx, mod_period(113) 	; ProTracker highest note - TODO
	jge .set_period
	mov cx, mod_period(113)

.set_period:
	mov [si + channel(period)], cx
	mov [si + channel(play_period)], cx
	or byte [si + channel(mixer_flags)], MIX_SET_SPD
	retn


;------------------------------------------------------------------------------
; 2xx - Portamento down
; Decrease current note pitch/increase period by xx units.
;------------------------------------------------------------------------------

	align 4

fx_portamento_down:
	mov cx, [si + channel(period)]	; CX: period
	mov dl, dh
	xor dh, dh
	mod_convert_period dx		; DX: period delta to add
	add cx, dx
	cmp cx, mod_period(856)		; ProTracker lowest note - TODO
	jbe .set_period
	mov cx, mod_period(856)

.set_period:
	mov [si + channel(period)], cx
	mov [si + channel(play_period)], cx
	or byte [si + channel(mixer_flags)], MIX_SET_SPD
	retn


;------------------------------------------------------------------------------
; 3xx - Tone portamento
; Slide the pitch of the current note towards the specified destination note
; by xx units. If xx is zero, previously used value is applied.
;------------------------------------------------------------------------------

	align 4

fx_tone_portamento:
	test dh, dh			; Save portamento speed
	jz fx_tone_portamento_no_param
	mov [si + channel(toneporta_spd)], dh

fx_tone_portamento_no_param:
	mov bx, [si + channel(toneporta_per)]
	test bx, bx			; BX: target period
	jz .exit			; No target period, exit

	mov dl, [si + channel(toneporta_spd)]
	xor dh, dh
	mod_convert_period dx		; DX: period delta to add/subtract
	mov cx, [si + channel(period)]	; CX: period
	cmp byte [si + channel(toneporta_dir)], 0
	jne .tone_portamento_up

	add cx, dx			; Pitch down -> add delta to period
	cmp cx, bx
	jb .use_tone_portamento_period

.use_final_period:
	mov cx, bx			; Use target as period when reached
	mov [si + channel(period)], cx
	mov [si + channel(play_period)], cx
	or byte [si + channel(mixer_flags)], MIX_SET_SPD
	mov word [si + channel(toneporta_per)], 0
	retn

	align 4

.tone_portamento_up:
	sub cx, dx			; Pitch up -> subtract delta from period
	cmp cx, bx
	jle .use_final_period

.use_tone_portamento_period:
	mov [si + channel(period)], cx	; Apply intermediate period

	cmp byte [si + channel(glissando_flag)], 0
	je .no_glissando
	mov ah, [si + channel(finetune)]
	call mod_period_floor_seminote	; Glissando on: round down to semitone

.no_glissando:
	mov [si + channel(play_period)], cx
	or byte [si + channel(mixer_flags)], MIX_SET_SPD

.exit:
	retn


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
	mov bl, [si + channel(vibrato_param)]
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
	mov [si + channel(vibrato_param)], bl

fx_vibrato_no_param:
	mov bl, [si + channel(vibrato_pos)]
	mov dl, bl			; DL: vibrato position
	shr bl, 2
	and bl, 0x1f
	mov bh, [si + channel(wave_control)]
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
	xor bh, bh			; Sine wave
	mov bl, fs:[vibtab + bx]	; BL: vibrato multiplier

.use_multiplier:
	mov dh, [si + channel(vibrato_param)]
	mov bh, dh
	and bh, 0x0f			; BH: vibrato depth
	push ax				; Calculate period delta for vibrato
	mov al, bl
	mul bh
	mov bx, ax
	pop ax
	mod_convert_period bx, -7	; BX: period delta
	mov cx, [si + channel(period)]	; CX: period
	test dl, 0x80
	jnz .negative_wave
	add cx, bx			; Positive vibrato -> decrease pitch
	jmp .apply_delta

	align 4

.negative_wave:
	sub cx, bx			; Negative vibrato -> increase pitch

.apply_delta:
	mov [si + channel(play_period)], cx
	or byte [si + channel(mixer_flags)], MIX_SET_SPD
	shr dh, 2			; Update vibrato position
	and dh, 0x3c
	add [si + channel(vibrato_pos)], dh
	retn


;------------------------------------------------------------------------------
; 5xy - Volume slide with tone portamento
; Apply volume slide with given xy parameters and continues portamento with
; previously used tone portamento effect parameters (same as Axy with 300).
;------------------------------------------------------------------------------

	align 4

fx_tone_vol_slide:
	push dx				; Call tone portamento without parameter
	call fx_tone_portamento_no_param
	pop dx
	jmp fx_volume_slide		; Continue with volume slide


;------------------------------------------------------------------------------
; 6xy - Volume slide with vibrato
; Apply volume slide with given xy parameters and continues vibrato with
; previously used vibrato speed and depth parameters (same as Axy with 400).
;------------------------------------------------------------------------------

	align 4

fx_vibrato_vol_slide:
	push dx				; Call tone portamento without parameter
	call fx_vibrato_no_param
	pop dx
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
	mov bl, [si + channel(tremolo_param)]
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
	mov [si + channel(tremolo_param)], bl

.no_param:
	mov bl, [si + channel(tremolo_pos)]
	mov dl, bl			; DL: tremolo position
	shr bl, 2
	and bl, 0x1f
	mov bh, [si + channel(wave_control)]
	and bh, 0x30			; BH: tremolo waveform
	jz .sine_wave
	shl bl, 3			; BL: adjusted tremolo position
	cmp bh, 0x10
	je .ramp_down
	mov bl, 255			; BL: tremolo multiplier
	jmp .use_multiplier		; Square wave

	align 4

.ramp_down:
	test byte [si + channel(vibrato_pos)], 0x80 ; Sawtooth
	; ^^^ Yes, this is a copy-paste monster from ProTracker. They forgot to
	; update the variable name. The correct command here would be:
	; test dl, 0x80
	jz .use_multiplier
	xor bl, 255			; BL: tremolo multiplier (invert)
	jmp .use_multiplier

	align 4

.sine_wave:
	xor bh, bh			; Sine wave
	mov bl, fs:[vibtab + bx]	; BL: tremolo multiplier

.use_multiplier:
	mov dh, [si + channel(tremolo_param)]
	mov bh, dh
	and bh, 0x0f			; BH: tremolo depth
	push ax				; Calculate period delta for tremolo
	mov al, bl
	mul bh
	mov bx, ax
	pop ax
	shr bx, 6			; BL: volume delta
	mov bh, [si + channel(volume)]	; BH: volume
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
	mov [si + channel(play_volume)], bh
	or byte [si + channel(mixer_flags)], MIX_SET_VOL
	shr dh, 2			; Update tremolo position
	and dh, 0x3c
	add [si + channel(tremolo_pos)], dh
	retn


;------------------------------------------------------------------------------
; 8xx - Set channel fine panning
; Set the panning of the channel to x, where 00 is far left and FF is far
; right.
;------------------------------------------------------------------------------

	align 4

fx_set_fine_panning:
	mov [si + channel(pan)], dh
	or byte [si + channel(mixer_flags)], MIX_SET_PAN
	retn


;------------------------------------------------------------------------------
; 9xx - Sample offset
; Play the instrument from the xxth byte sample offset.
;------------------------------------------------------------------------------

	align 4

fx_sample_offset:
	test dh, dh
	jz .no_offset			; No new position, do nothing
	mov bh, dh			; New position, set it
	xor bl, bl
	mov [si + channel(sample_ofs)], bx
	mov [si + channel(sample_pos)], bx
	retn

.no_offset:
	mov bx, [si + channel(sample_ofs)]
	mov [si + channel(sample_pos)], bx
	retn


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
	add dh, [si + channel(volume)]
	cmp dh, 64
	jbe .set_volume

.slide_down:
	mov dl, dh			; Slide down
	and dl, 0x0f
	mov dh, [si + channel(volume)]
	sub dh, dl
	jns .set_volume
	xor dh, dh

.set_volume:
	mov [si + channel(volume)], dh	; Apply new volume
	mov [si + channel(play_volume)], dh
	or byte [si + channel(mixer_flags)], MIX_SET_VOL
	retn


;------------------------------------------------------------------------------
; Bxx - Position jump
; Jump to the first row of the pattern specified in position xx. If the pattern
; break effect is present in a following channel, the row number specified in
; the Dxx command will be used as the target.
;------------------------------------------------------------------------------

	align 4

fx_position_jump:
	mov [state(break_position)], dh	; Save target position
	mov byte [state(break_row)], 0
	or byte [state(flags)], STATE_BREAK | STATE_SET_POS
	retn


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
	or byte [si + channel(mixer_flags)], MIX_SET_VOL
	mov [si + channel(volume)], dh
	mov [si + channel(play_volume)], dh
	retn


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
	mov [state(break_row)], dl
	or byte [state(flags)], STATE_BREAK
	cmp byte [state(patt_del_count)], 0
	je .exit
	or byte [state(flags)], STATE_SKIP

.exit:
	retn


;------------------------------------------------------------------------------
; Fxx - Set speed / tempo
; Set the speed (ticks per row) if xx < 20 (hex) or tempo (BPM) if xx >= 20.
;------------------------------------------------------------------------------

	align 4

fx_set_speed:
	cmp dh, 0x20
	jae .set_bpm
	mov [state(speed)], dh		; Set ticks per row
	retn

	align 4

.set_bpm:
	mov [state(bpm)], dh		; Set tempo (BPM)
	mov bl, dh
	xor bh, bh
	shl bx, 3
	mov cx, bx
	add bx, cx
	add bx, cx			; BX: ticks per minute = BPM * 24
	call [out_fn(set_tick_rate)]
	retn


;------------------------------------------------------------------------------
; E3x - Glissando control
; Controls whether tone portamento performs slides in semitones or not.
; x = 0: Disable glissando (default slide)
; x = 1: Enable glissando (slide by semitones)
;------------------------------------------------------------------------------

	align 4

fx_glissando_ctrl:
	mov [si + channel(glissando_flag)], dh
	retn


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
	mov ah, [si + channel(wave_control)]
	and ah, 0xf0
	or ah, dh
	mov [si + channel(wave_control)], ah
	retn


;------------------------------------------------------------------------------
; E5x - Set finetune
; Set finetune. Applies to current playing note only, does not change the
; sample preset finetune value.
; This effect may not make destructive changes to any registers as it is called
; from critical parts of the playroutine.
;------------------------------------------------------------------------------

	align 4

fx_set_finetune:
	test dh, 0x08			; Extend to signed small int - this is
	jz .save_finetune		; harmless since it's done anyways for
	or dh, 0xf0			; this effect.

.save_finetune:
	mov [si + channel(finetune)], dh
	retn


;------------------------------------------------------------------------------
; E6x - Pattern loop
; Set loop start position within pattern if x = 0, repeat pattern between
; start position and end of current row x times if x > 0.
;------------------------------------------------------------------------------

	align 4

fx_pattern_loop:
	test dh, dh
	jz .set_loop
	mov ah, [state(loop_count)]
	test ah, ah
	jz .start_loop
	dec ah
	mov [state(loop_count)], ah
	jz .exit

.loop_jump:
	mov dl, [state(loop_start_row)]
	mov [state(break_row)], dl
	mov dl, [state(position)]
	mov [state(break_position)], dl
	or byte [state(flags)], STATE_SET_POS | STATE_BREAK

.exit:
	retn

	align 4

.start_loop:
	mov [state(loop_count)], dh
	jmp .loop_jump

	align 4

.set_loop:
	mov ah, [state(row)]
	mov [state(loop_start_row)], ah
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
	mov ah, [si + channel(wave_control)]
	and ah, 0x0f
	or ah, dh
	mov [si + channel(wave_control)], ah
	retn


;------------------------------------------------------------------------------
; E8x - Set channel panning
; Set the panning of the channel to x, where 0 is far left and F is far right
;------------------------------------------------------------------------------

	align 4

fx_set_panning:
	mov ah, dh
	shl ah, 4
	or ah, dh
	mov [si + channel(pan)], ah
	or byte [si + channel(mixer_flags)], MIX_SET_PAN
	retn


;------------------------------------------------------------------------------
; E9x - Retrig note
; Reset sample position every x ticks.
;------------------------------------------------------------------------------

	align 4

fx_retrig_note:
	cmp dh, 1
	jb .exit			; x = 0: do nothing
	je .retrig			; x = 1: retrig each tick
	push ax
	mov al, [state(tick)]
	xor ah, ah
	div dh
	mov dh, ah			; DH: tick MOD x
	pop ax
	test dh, dh			; Remainder: not xth tick
	jnz .exit

.retrig:
	mov word [si + channel(sample_pos)], 0
	mov cx, [si + channel(period)]
	mov [si + channel(play_period)], cx
	or word [si + channel(flags)], wflags(SMP_SET_POS, MIX_SET_SPD)

.exit:
	retn


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
	cmp [state(tick)], dh
	jne .exit
	mov byte [si + channel(volume)], 0
	mov byte [si + channel(play_volume)], 0
	or byte [si + channel(mixer_flags)], MIX_SET_VOL

.exit:
	retn


;------------------------------------------------------------------------------
; EDx - Note delay
; Delay start of the instrument for x ticks.
;------------------------------------------------------------------------------

	align 4

fx_note_delay:
	cmp [state(tick)], dh
	jne .exit
	cmp byte [si + channel(current_note)], 0
	je .exit
	mov word [si + channel(sample_pos)], 0
	mov cx, [si + channel(period)]
	mov [si + channel(play_period)], cx
	or word [si + channel(flags)], wflags(SMP_SET_POS, MIX_SET_SPD)

.exit:
	retn


;------------------------------------------------------------------------------
; EEx - Pattern delay
; Repeat the current row x times without retriggering notes.
;------------------------------------------------------------------------------

	align 4

fx_pattern_delay:
	mov [state(patt_delay)], dh
	retn


;==============================================================================
; Data area
;==============================================================================

segment modplayer_data public use16 class=DATA align=16
segment modplayer_data

		; Effects which should be processed on the first tick

		alignb 2
fxtab_note	dw 0			; 0xy: arpeggio
		dw 0			; 1xx: portamento up
		dw 0			; 2xx: portamento down
		dw 0			; 3xx: tone portamento
		dw 0			; 4xy: vibrato
		dw 0			; 5xy: volume slide + tone portamento
		dw 0			; 6xy: volume slide + vibrato
		dw 0			; 7xy: tremolo
		dw fx_set_fine_panning	; 8xx: set fine panning
		dw fx_sample_offset	; 9xx: sample offset
		dw 0			; Axy: volume slide
		dw fx_position_jump	; Bxx: position jump
		dw fx_set_volume	; Cxx: set volume
		dw fx_pattern_break	; Dxx: pattern break
		dw 0			; Padding
		dw fx_set_speed		; Fxx: set speed / tempo
		dw 0			; E0x: set filter
		dw fx_portamento_up	; E1x: fine portamento up
		dw fx_portamento_down	; E2x: fine portamento down
		dw fx_glissando_ctrl	; E3x: glissando control
		dw fx_set_vibrato_ctrl	; E4x: set vibrato waveform
		dw fx_set_finetune	; E5x: set finetune
		dw fx_pattern_loop	; E6x: pattern loop
		dw fx_set_tremolo_ctrl	; E7x: set tremolo waveform
		dw fx_set_panning	; E8x: set panning
		dw fx_retrig_note	; E9x: retrigger
		dw fx_fine_vol_up	; EAx: fine volume slide up
		dw fx_fine_vol_down	; EBx: fine volume slide down
		dw fx_note_cut		; ECx: note cut
		dw fx_note_delay	; EDx: note delay
		dw fx_pattern_delay	; EEx: pattern delay
		dw 0			; EFx: invert loop

		; Effects which should be processed on subsequent ticks

fxtab_intra	dw fx_arpeggio		; 0xy: arpeggio
		dw fx_portamento_up	; 1xx: portamento up
		dw fx_portamento_down	; 2xx: portamento down
		dw fx_tone_portamento	; 3xx: tone portamento
		dw fx_vibrato		; 4xy: vibrato
		dw fx_tone_vol_slide	; 5xy: volume slide + tone portamento
		dw fx_vibrato_vol_slide	; 6xy: volume slide + vibrato
		dw fx_tremolo		; 7xy: tremolo
		dw 0			; 8xx: set panning
		dw 0			; 9xx: sample offset
		dw fx_volume_slide	; Axy: volume slide
		dw 0			; Bxx: position jump
		dw 0			; Cxx: set volume
		dw 0			; Dxx: pattern break
		dw 0			; Padding
		dw 0			; Fxx: set speed / tempo
		dw 0			; E0x: set filter
		dw 0			; E1x: fine portamento up
		dw 0			; E2x: fine portamento down
		dw 0			; E3x: glissando control
		dw 0			; E4x: set vibrato waveform
		dw 0			; E5x: set finetune
		dw 0			; E6x: pattern loop
		dw 0			; E7x: set tremolo waveform
		dw 0			; E8x: set panning
		dw fx_retrig_note	; E9x: retrigger
		dw 0			; EAx: fine volume slide up
		dw 0			; EBx: fine volume slide down
		dw fx_note_cut		; ECx: note cut
		dw fx_note_delay	; EDx: note delay
		dw 0			; EEx: pattern delay
		dw 0			; EFx: invert loop

		; Effects which should be processed on the first tick during
		; pattern delay

fxtab_pattdel	dw fx_arpeggio		; 0xy: arpeggio
		dw fx_portamento_up	; 1xx: portamento up
		dw fx_portamento_down	; 2xx: portamento down
		dw fx_tone_portamento	; 3xx: tone portamento
		dw fx_vibrato		; 4xy: vibrato
		dw fx_tone_vol_slide	; 5xy: volume slide + tone portamento
		dw fx_vibrato_vol_slide	; 6xy: volume slide + vibrato
		dw fx_tremolo		; 7xy: tremolo
		dw 0			; 8xx: set panning
		dw 0			; 9xx: sample offset
		dw fx_volume_slide	; Axy: volume slide
		dw fx_position_jump	; Bxx: position jump
		dw 0			; Cxx: set volume
		dw fx_pattern_break	; Dxx: pattern break
		dw 0			; Padding
		dw 0			; Fxx: set speed / tempo
		dw 0			; E0x: set filter
		dw fx_portamento_up	; E1x: fine portamento up
		dw fx_portamento_down	; E2x: fine portamento down
		dw 0			; E3x: glissando control
		dw 0			; E4x: set vibrato waveform
		dw 0			; E5x: set finetune
		dw 0			; E6x: pattern loop
		dw 0			; E7x: set tremolo waveform
		dw 0			; E8x: set panning
		dw 0			; E9x: retrigger
		dw fx_fine_vol_up	; EAx: fine volume slide up
		dw fx_fine_vol_down	; EBx: fine volume slide down
		dw 0			; ECx: note cut
		dw 0			; EDx: note delay
		dw 0			; EEx: pattern delay
		dw 0			; EFx: invert loop

		; Vibrato/tremolo sine table

vibtab		db   0,  24,  49,  74,  97, 120, 141, 161
		db 180, 197, 212, 224, 235, 244, 250, 253
		db 255, 253, 250, 244, 235, 224, 212, 197
		db 180, 161, 141, 120,  97,  74,  49,  24

		; Arpeggio effect note "offset" table

arptab		db 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2
		db 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2
