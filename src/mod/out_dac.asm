;==============================================================================
; MOD player - DAC output device
;------------------------------------------------------------------------------
; Supports via software wavetable rendering:
; - PC speaker: 5.3 - 7.2-bits, up to 29000 Hz sample rate
;   (resolution inversely proportional to sample rate)
; - Parallel port DACs (aka. Covox): 8-bits, up to 100000 Hz sample rate:
;   - Mono LPT DAC on any parallel port
;   - Dual LPT DACs on any parallel ports in mono or stereo
;   - Stereo LPT DAC ("Stereo-on-1") on any parallel port in mono or stereo
;------------------------------------------------------------------------------
; All output devices utilize the timer interrupt (IRQ 0) for playback, which
; makes it unavailable for other purposes.
;------------------------------------------------------------------------------
; 86Box performance with 1x amplification, 20 msec buffer (PC speaker
; performance is the same as a mono LPT DAC):
;
; DAC     Channels   Samplerate   Stereo mode   Interpolation   CPU
; Mono        4        49716 Hz   -             Nearest         386dx/16
; Dual        4        44192 Hz   Hard          Nearest         386dx/16
; Stereo      4        41144 Hz   Hard          Nearest         386dx/16
; Mono        4        42617 Hz   -             Linear          386dx/20
; Dual        4        56818 Hz   Hard          Nearest         386dx/20
; Stereo      4        51878 Hz   Hard          Nearest         386dx/20
; Dual        4        39773 Hz   Hard          Linear          386dx/20
; Stereo      4        37287 Hz   Hard          Linear          386dx/20
; Dual        4        31400 Hz   Real          Linear          386dx/20
; Stereo      4        29830 Hz   Real          Linear          386dx/20
; Mono        4        74574 Hz   -             Nearest         386dx/25
; Mono        4        54236 Hz   -             Linear          386dx/25
; Dual        4        49716 Hz   Hard          Linear          386dx/25
; Stereo      4        47727 Hz   Hard          Linear          386dx/25
; Dual        4        49716 Hz   Real          Nearest         386dx/25
; Stereo      4        47727 Hz   Real          Nearest         386dx/25
; Mono        8        49716 Hz   -             Nearest         386dx/25
; Dual        8        47727 Hz   Hard          Nearest         386dx/25
; Stereo      8        45892 Hz   Hard          Nearest         386dx/25
; Dual        4        38490 Hz   Real          Linear          386dx/25
; Stereo      4        37287 Hz   Real          Linear          386dx/25
; Mono        4        99432 Hz   -             Nearest         386dx/33
; Mono        4        70187 Hz   -             Linear          386dx/33
; Dual        4        91783 Hz   Hard          Nearest         386dx/33
; Stereo      4        85227 Hz   Hard          Nearest         386dx/33
; Dual        4        51878 Hz   Real          Linear          386dx/33
; Stereo      4        49716 Hz   Real          Linear          386dx/33
; Mono        8        42614 Hz   -             Linear          386dx/33
; Dual        8        39773 Hz   Real          Nearest         386dx/33
; Stereo      8        38490 Hz   Real          Nearest         386dx/33
; Dual        8        41144 Hz   Hard          Linear          386dx/33
; Stereo      8        39773 Hz   Hard          Linear          386dx/33
; Mono        8        49716 Hz   -             Linear          386dx/40
; Dual        8        39773 Hz   Real          Linear          386dx/40
; Stereo      8        38490 Hz   Real          Linear          386dx/40
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "system/api/pit.inc"
%include "system/api/pic.inc"
%include "mod/consts/global.inc"
%include "mod/consts/public.inc"
%include "mod/structs/global.inc"
%include "mod/structs/out_dac.inc"
%include "mod/consts/out.inc"
%include "mod/consts/out_dac.inc"
%include "mod/api/wtbl_sw.inc"
%include "mod/api/routine.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	state(var) mod.out_state + mod_out_dac_state. %+ var
%define	params(var) mod.out_params + mod_out_params. %+ var
%define	set_out_fn(name, lbl) at mod_out_fns. %+ name, dw %+ (lbl)

; Printer control bits to select left/right channel for stereo LPT DAC

LPTST_CHN_LEFT	EQU 00000001b
LPTST_CHN_RIGHT	EQU 00000010b
LPTST_CHN_BOTH	EQU 00000011b

segment modplayer public use16 class=CODE align=16
segment modplayer


;------------------------------------------------------------------------------
; Set up the DAC output device.
;------------------------------------------------------------------------------
; -> AL - Output device type (MOD_DAC_*)
;    CX - Amplification as 8.8-bit fixed point value
;    EDX - Requested sample rate
;    DS - Player instance segment
; <- CF - Set if error
;    EAX - Error code if CF set or actual sample rate
;    EBX - Number of extra samples that will be generated at the end of each
;          sample (must reserve enough space) if no error
;------------------------------------------------------------------------------

	align 4

setup:
	push ecx
	push edx
	push ebx

	; Validate configuration

	mov [state(dev_type)], al
	mov [state(amplify)], cx
	cmp al, MOD_DAC_LPTDUAL
	ja .unknown_device

.check_sample_rate_min:
	cmp edx, 8000			; Force minimum 8 kHz samplerate (which
	jae .check_sample_rate_max	; will still sound awful)
	mov edx, 8000

.check_sample_rate_max:
	cmp al, MOD_DAC_SPEAKER
	je .check_sample_rate_max_speaker
	cmp edx, 100000			; Limit maximum to 100 kHz for LPT DACs
	jbe .save_config
	mov edx, 100000
	jmp .save_config

.check_sample_rate_max_speaker:
	cmp edx, 29000			; Limit maximum to 44 kHz for PC speaker
	jbe .save_config		; Higher values would reduce bitdepth
	mov edx, 29000			; too much

.save_config:
	mov ah, FMT_UNSIGNED		; Set output bitstream format flags
	or ah, FMT_8BIT
	cmp al, MOD_DAC_LPTST
	je .stereo_device
	cmp al, MOD_DAC_LPTDUAL
	je .stereo_device
	or ah, FMT_MONO
	jmp .save_out_format

.stereo_device:
	cmp byte [params(stereo_mode)], MOD_PAN_MONO
	je .save_out_format
	or ah, FMT_STEREO

.save_out_format:
	mov [state(output_format)], ah

	%ifdef __DEBUG__

	; Show configuration when debug is enabled

	cmp al, MOD_DAC_SPEAKER
	jne .check_lpt
	log {'Output device: Internal PC speaker', 13, 10}
	jmp .calc_pit_rate

.check_lpt:
	cmp al, MOD_DAC_LPT
	jne .check_lptst
	log {'Output device: Mono LPT DAC on port {X16}', 13, 10}, [params(port)]
	jmp .calc_pit_rate

.check_lptst:
	cmp al, MOD_DAC_LPTST
	jne .check_lptdual
	log {'Output device: Stereo LPT DAC on port {X16}', 13, 10}, [params(port)]
	jmp .calc_pit_rate

.check_lptdual:
	cmp al, MOD_DAC_LPTDUAL
	jne .calc_pit_rate
	log {'Output device: Dual LPT DACs on port {X16} and {X16}', 13, 10}, [params(port)], [params(port + 2)]

	%endif

.calc_pit_rate:

	; Calculate PIT/actual sample rate

	call far sys_pit_calc_rate
	mov [state(pit_rate)], bx
	mov [state(sample_rate)], eax	; Save actual sample rate

	; Calculate period -> SW wavetable speed conversion base

	mov ebx, eax
	mov edx, 0x361
	mov eax, 0xf0f00000
	div ebx
	shr ebx, 1			; EBX: sample rate / 2
	cmp edx, ebx			; Remainder > sample rate / 2?
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx
	mov [state(period_base)], eax

	; Allocate memory for the output buffer

	mov eax, [state(sample_rate)]	; Convert msec to buffer size
	movzx ebx, word [params(buffer_size)]
	cmp ebx, 1000
	jae .check_buffer_size
	mul ebx
	mov ebx, 1000
	div ebx

.check_buffer_size:
	cmp eax, 4096			; Maximum sane buffer size
	jbe .use_buffer_size
	mov eax, 4096

.use_buffer_size:
	mov [params(buffer_size)], ax

	mov ebx, eax
	shl ebx, 3			; 32-bit stereo buffer of SW wavetable
	mov [state(buffer_size)], ebx
	lea ebx, [ebx + ebx * 2]	; Triple buffering
	mov al, SYS_MEM_HI_LO
	call far sys_mem_alloc
	jc .error
	mov [state(buffer_addr)], eax
	mov ecx, eax
	sub ecx, [mod.instance_addr]
	mov [state(buffer_ofs)], ecx

	log {'Allocated {u} bytes for output device buffer @{X}', 13, 10}, ebx, eax

	cmp byte [state(dev_type)], MOD_DAC_SPEAKER
	jne .setup_wt

	; Create speaker sample lookup table

	xor bx, bx			; BX: sample (0 - 255)
	mov cx, [state(pit_rate)]	; CX: timer IRQ PIT rate (always 8-bit)

.speakertab_loop:
	mov al, bl
	mul cl
	inc ah
	mov [state(speakertab) + bx], ah
	inc bl
	jnz .speakertab_loop

.setup_wt:

	; Setup wavetable

	mov al, [params(interpolation)]
	mov ah, [params(stereo_mode)]
	mov bx, [state(amplify)]
	mov cx, 0
	mov dl, [state(output_format)]
	mov dh, [params(initial_pan)]
	call mod_swt_setup
	jc .error
	mov [state(amplify)], bx
	mov ebx, eax

	; Done

	add sp, 4			; Discard EBX from stack
	mov eax, [state(sample_rate)]
	clc

.exit:
	pop edx
	pop ecx
	retn

.error:
	pop ebx
	stc
	jmp .exit

.unknown_device:
	mov eax, MOD_ERR_DEV_UNK
	jmp .error


;------------------------------------------------------------------------------
; Shutdown the output device. No further playback is possible until the setup
; function is called again.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

shutdown:
	push eax

	; Shutdown wavetable

	call mod_swt_shutdown

	; Release memory

	mov eax, [state(buffer_addr)]
	test eax, eax
	jz .done

	log {'Disposing output buffer @{X}', 13, 10}, eax

	call far sys_mem_free
	mov dword [state(buffer_addr)], 0

.done:
	pop eax
	retn


;------------------------------------------------------------------------------
; Set the amplification level.
;------------------------------------------------------------------------------
; -> AH.AL - Requested audio amplification in 8.8 fixed point value
;    DS - Player instance segment
; <- AH.AL - Actual audio amplification level
;------------------------------------------------------------------------------

	align 4

set_amplify:
	mov [state(amplify)], ax
	call mod_swt_set_amplify
	mov [state(amplify)], ax

	retn


;------------------------------------------------------------------------------
; Start playback on the DAC device.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- CF - Cleared
;------------------------------------------------------------------------------

	align 4

play:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push es

	; Reset audio output

	mov dh, [params(initial_pan)]
	call mod_swt_reset_channels

	mov byte [state(buffer_playprt)], 0
	mov word [state(play_sam_int)], 0
	mov word [state(play_sam_fr)], 0
	mov eax, [state(buffer_ofs)]
	mov [state(buffer_pos)], eax
	add eax, [state(buffer_size)]
	mov [state(buffer_limit)], eax

	; Pre-render into output buffer before starting playback

	mov byte [state(buffer_pending)], BUF_READY
	mov byte [state(buffer_status)], BUF_RENDER_1
	call render
	mov byte [state(buffer_status)], BUF_RENDER_2
	call render
	mov byte [state(buffer_status)], BUF_RENDER_3
	call render

	cli

	; Setup and install IRQ 0 handler

	xor cl, cl
	call far sys_pic_irq_to_int
	call far sys_get_int_handler
	mov cs:[irq0_prev_handler], bx
	mov cs:[irq0_prev_handler + 2], es
	mov ax, cs
	mov es, ax
	mov dx, [params(port)]		; DX: first port
	mov di, [params(port + 2)]	; DI: second port

	; Since all DAC device rely on the PC's timer interrupt, each device
	; has its own dedicated IRQ 0 handler with code optimized to be as fast
	; as possible since.

	cmp byte [state(dev_type)], MOD_DAC_SPEAKER
	je .start_speaker
	cmp byte [state(dev_type)], MOD_DAC_LPTST
	je .start_lpt_dac_stereo
	cmp byte [state(dev_type)], MOD_DAC_LPTDUAL
	je .start_lpt_dac_dual
	cmp byte [state(dev_type)], MOD_DAC_LPT
	je .start_lpt_dac

.start_speaker:

	; Setup PC speaker

	in al, 0x61			; Turn on speaker
	or al, 0x03
	out 0x61, al
	mov al, 0x90			; Set PIT channel 2 to mode 0
	out 0x43, al
	mov al, 0x01
	out 0x42, al
	mov cs:[speaker_irq0_player_segment], ds
	mov bx, speaker_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac_stereo:

	; Setup stereo LPT DAC. This device is attached to a single parallel
	; port and the currently output channel is switched via strobe and/or
	; auto linefeed pins. See LPTST_CHN_* constants for printer control
	; values.

	add dx, 2
	in al, dx
	mov [state(lpt_prn_ctrl)], al
	test byte [state(output_format)], FMT_STEREO
	jz .start_lpt_dac_stereo_mono
	mov cs:[lpt_dac_stereo_irq0_player_segment], ds
	mov cs:[lpt_dac_stereo_irq0_ctrl_port], dx
	mov bx, lpt_dac_stereo_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac_stereo_mono:

	; Stereo LPT DAC with forced mono output. Enable both channels, then
	; start output via normal mono LPT DAC output routine.

	mov al, LPTST_CHN_BOTH
	out dx, al
	sub dx, 2
	jmp .start_lpt_dac

.start_lpt_dac_dual:

	; Setup dual LPT DAC. This requires two parallel ports and two mono
	; 8-bit DACs connected to each. Device on first port is left channel
	; and device on second port is right channel.

	test byte [state(output_format)], FMT_STEREO
	jz .start_lpt_dac_dual_mono
	mov cs:[lpt_dac_dual_irq0_player_segment], ds
	mov cs:[lpt_dac_dual_irq0_port1], dx
	mov cs:[lpt_dac_dual_irq0_port2a], di
	mov cs:[lpt_dac_dual_irq0_port2b], di
	mov bx, lpt_dac_dual_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac_dual_mono:

	; Dual LPT DAC with forced mono output

	mov cs:[lpt_dac_dual_mono_irq0_player_segment], ds
	mov cs:[lpt_dac_dual_mono_irq0_port1], dx
	mov cs:[lpt_dac_dual_mono_irq0_port2a], di
	mov cs:[lpt_dac_dual_mono_irq0_port2b], di
	mov bx, lpt_dac_dual_mono_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac:

	; Mono (single) LPT DAC

	mov cs:[lpt_dac_irq0_player_segment], ds
	mov cs:[lpt_dac_irq0_port1], dx
	mov bx, lpt_dac_irq0_handler

.setup_irq_handler:
	call far sys_set_int_handler

	; Program the PIT

	mov word [state(pit_tick_count)], 0
	mov bx, [state(pit_rate)]
	call far sys_pit_set_irq0_rate

	; Enable IRQ 0

	xor cx, cx
	call far sys_pic_enable_irq

	sti

.exit:
	clc
	pop es
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Stop playback on the DAC device.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- CF - Cleared
;------------------------------------------------------------------------------

	align 4

stop:
	push eax
	push bx
	push cx
	push dx
	push es

	cli

	; Reset state

	mov word [state(pit_tick_count)], 0

	; Uninstall IRQ 0 handler

	xor cl, cl
	call far sys_pic_irq_to_int
	mov es, cs:[irq0_prev_handler + 2]
	mov bx, cs:[irq0_prev_handler]
	call far sys_set_int_handler

	cmp byte [state(dev_type)], MOD_DAC_SPEAKER
	jne .stop_dac_lptst

	; Restore speaker state

	in al,0x61			; Turn off speaker
	and al,0xfc
	out 0x61,al
	mov al, 0xb6			; Reset PIT channel 2 to square wave
	out 0x43, al
	xor al, al
	out 0x42, al
	out 0x42, al
  	jmp .reset_pit

.stop_dac_lptst:
	cmp byte [state(dev_type)], MOD_DAC_LPTST
	jne .reset_pit

	; Restore LPT printer controls for stereo LPT DAC

	mov dx, [params(port)]
	add dx, 2
	mov al, [state(lpt_prn_ctrl)]
	out dx, al

.reset_pit:

	; Reset the PIT

	call far sys_pit_reset_irq0_rate

	sti

	clc
	pop es
	pop dx
	pop cx
	pop bx
	pop eax
	retn


;------------------------------------------------------------------------------
; Set the volume, panning and playback speed of a channel.
;------------------------------------------------------------------------------
; -> AL - Channel
;    AH - Mask bits
;         bit 0: Set volume to BL
;         bit 1: Set panning (balance) to BH
;         bit 2: Set speed to CX
;    BL - Volume (0 - 64)
;    BH - Panning (0 - 255)
;    CX - Playback note periods
;    DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

set_mixer:
	test ah, 0x04
 	jz mod_swt_set_mixer

	; Convert MOD period to playback speed if speed must be set for the
	; wavetable mixer.

	push ecx
	push edx

	push eax
	test cx, cx			; Guard against division by zero hangs
	setz al
	xor ah, ah
	add cx, ax
	xor edx, edx
	mov eax, [state(period_base)]
	and ecx, 0xffff
	div ecx
	shr ecx, 1			; ECX: period / 2
	cmp edx, ecx			; Remainder > period / 2?
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx
	mov dx, ax
	shr eax, 16
	mov cx, ax
	pop eax

	call mod_swt_set_mixer

	pop edx
	pop ecx
	retn


;------------------------------------------------------------------------------
; Set the playroutine callback tick rate.
;------------------------------------------------------------------------------
; -> BX - Number of playroutine ticks per minute
;    DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

set_tick_rate:
	push eax
	push ebx
	push edx

	; Calculate number of samples between player ticks

	mov eax, [state(sample_rate)]
	mov edx, eax
	shl eax, 6
	shl edx, 2
	sub eax, edx			; EAX: sample rate * 60
	mov edx, eax
	shr edx, 16
	shl eax, 16			; EDX:EAX: sample rate * 60 * 65536
	and ebx, 0xffff
	div ebx

	mov ebx, eax
	shr ebx, 16			; BX.AX: Number of samples between ticks
	mov [state(play_tickr_int)], bx
	mov [state(play_tickr_fr)], ax

	pop edx
	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Render channels into the output buffer.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	align 4

render:
	mov al, BUF_RENDERING
	xchg al, byte [state(buffer_status)]
	cmp al, BUF_RENDERING
	je .exit			; BUF_RENDERING: already rendering audio
	jb .noop			; BUF_READY: nothing to render

	; Initialize state

	mov dx, [params(buffer_size)]	; DX: number of samples to render
	mov bx, [state(play_sam_int)]	; BX: samples until playroutine tick
	mov edi, [state(buffer_addr)]
	cmp al, BUF_RENDER_1
	je .loop_render
	mov esi, [state(buffer_size)]	; 2nd part of buffer
	add edi, esi
	cmp al, BUF_RENDER_2
	je .loop_render
	add edi, esi			; 3rd part of buffer

	; Render samples to the output audio buffer
	; BX: number of samples until next playroutine tick
	; CX: number of samples to render by software wavetable in current pass
	; DX: number of samples to render into output audio buffer
	; EDI: linear address of output audio buffer position to render into

.loop_render:

	; Call playroutine tick when necessary

	test bx, bx
	jnz .calc_render_count

	push bx
	push cx
	push dx
	push edi
	call mod_playroutine_tick
	pop edi
	pop dx
	pop cx
	pop bx

	mov ax, [state(play_tickr_fr)]
	add [state(play_sam_fr)], ax
	adc bx, 0
	add bx, [state(play_tickr_int)]

.calc_render_count:

	; Determine number of samples to render in this pass

	movzx ecx, dx			; CX: number of samples to render
	cmp cx, bx			; Don't render past playroutine tick
	jb .render_swt
	mov cx, bx

.render_swt:

	; Render channels using software wavetable

	push bx
	push ecx
	push dx
	call mod_swt_render_direct
	pop dx
	pop ecx
	pop bx

	; Calculate number of samples left to render

	lea edi, [edi + ecx * 8]
	sub bx, cx
	sub dx, cx
	jnz .loop_render

	; Output buffer completely rendered

	mov [state(play_sam_int)], bx	; Update samples until playroutine tick

.noop:

	; Done rendering or nothing to do (no part of the buffer needs new audio
	; data)

	mov al, BUF_READY
	xchg al, [state(buffer_pending)]
	mov [state(buffer_status)], al

.exit:
	retn


;==============================================================================
; DAC playback timer interrupt handlers.
;==============================================================================


;------------------------------------------------------------------------------
; Macro to push registers onto the stack at IRQ 0 entry.
;------------------------------------------------------------------------------

%macro	irq0_start 0

	push eax
	push edx
	push esi
	push ds

	cld

%endmacro


;------------------------------------------------------------------------------
; Macro to restore registers from the stack and return from the interrupt.
;------------------------------------------------------------------------------

%macro	irq0_exit 0

	pop ds
	pop esi
	pop edx
	pop eax
	iret

%endmacro


;------------------------------------------------------------------------------
; Send the end of interrupt signal to the interrupt controller and call the
; original IRQ 0 handler at the standard 18.2 Hz rate.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;    %1 - Jump target when ready or 0 to exit IRQ 0
; <- Destroys AL and DX
;------------------------------------------------------------------------------

%macro	irq0_eoi 1

	mov dx, [state(pit_rate)]	; Call old handler if internal tick
	add [state(pit_tick_count)], dx	; counter overflows 16-bit
	jc %%call_prev_handler

	sys_pic_eoi 0			; Destroys AL

	%if (%1 = 0)			; Exit IRQ 0 or jump to target label
	irq0_exit
	%else
	jmp %1
	%endif

	align 4

%%call_prev_handler:
	call prev_irq0_handler		; Call previous handler

	%if (%1 = 0)			; Exit IRQ 0 or jump to target label
	irq0_exit
	%else
	jmp %1
	%endif

%endmacro


;------------------------------------------------------------------------------
; Advance buffer position, flip buffer if end is reached.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;    %1 - Number of bytes to add to ESI
; <- Destroys AL, DX and ESI
;------------------------------------------------------------------------------

%macro	advance_buffer 1

	%if (%1 != 0)
	add esi, %1			; Skip bytes in buffer (right channel)
	%endif

	; Save new buffer position, flip buffer if end is reached

	mov [state(buffer_pos)], esi
	cmp esi, [state(buffer_limit)]
	jae toggle_buffer

%endmacro


;------------------------------------------------------------------------------
; Call the original IRQ 0 handler.
;------------------------------------------------------------------------------

	align 4

prev_irq0_handler:
	pushf
	call 0x1234:0x1234
	irq0_prev_handler EQU $ - 4
	retn


;------------------------------------------------------------------------------
; PC speaker IRQ 0 handler.
;------------------------------------------------------------------------------

	align 4

speaker_irq0_handler:
	irq0_start

	; DS: player instance segment

	mov ax, 0x1234
	speaker_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [state(buffer_pos)]
	a32 lodsd
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip			; Clipping
	movzx eax, ah
	mov al, [state(speakertab) + eax]
	out 0x42, al

	advance_buffer 4
	irq0_eoi 0

	align 4

.clip:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	movzx eax, al
	mov al, [state(speakertab) + eax]
	out 0x42, al

	advance_buffer 4
	irq0_eoi 0


;------------------------------------------------------------------------------
; Mono LPT DAC IRQ 0 handler.
;------------------------------------------------------------------------------

	align 4

lpt_dac_irq0_handler:
	irq0_start

	; DS: player instance segment

	mov ax, 0x1234
	lpt_dac_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [state(buffer_pos)]
	a32 lodsd
	mov dx, 0x1234
	lpt_dac_irq0_port1 EQU $ - 2
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip			; Clipping
	mov al, ah
	out dx, al

	advance_buffer 4
	irq0_eoi 0

	align 4

.clip:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	advance_buffer 4
	irq0_eoi 0


;------------------------------------------------------------------------------
; Stereo LPT DAC output IRQ 0 handler.
;------------------------------------------------------------------------------

	align 4

lpt_dac_stereo_irq0_handler:
	irq0_start

	; DS: player instance segment

	mov ax, 0x1234
	lpt_dac_stereo_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [state(buffer_pos)]
	mov dx, 0x1234
	lpt_dac_stereo_irq0_ctrl_port EQU $ - 2

	; Select DAC left channel

	mov al, LPTST_CHN_LEFT
	out dx, al
	sub dx, 2

	; Output left channel sample

	a32 lodsd			; Left channel sample
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip_left			; Clipping
	mov al, ah
	out dx, al

	; Select DAC right channel

	add dx, 2
	mov al, LPTST_CHN_RIGHT
	out dx, al
	sub dx, 2

	; Output right channel sample

	a32 lodsd			; Right channel sample
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	advance_buffer 0
	irq0_eoi 0

	align 4

.clip_left:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	; Select DAC right channel

	add dx, 2
	mov al, LPTST_CHN_RIGHT
	out dx, al
	sub dx, 2

	; Output right channel sample

	a32 lodsd			; Right channel
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	advance_buffer 0
	irq0_eoi 0

	align 4

.clip_right:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	advance_buffer 0
	irq0_eoi 0


;------------------------------------------------------------------------------
; Dual LPT DAC output IRQ 0 handler.
;------------------------------------------------------------------------------

	align 4

lpt_dac_dual_irq0_handler:
	irq0_start

	; DS: player instance segment

	mov ax, 0x1234
	lpt_dac_dual_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [state(buffer_pos)]
	a32 lodsd			; Left channel
	mov dx, 0x1234
	lpt_dac_dual_irq0_port1 EQU $ - 2
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip_left			; Clipping
	mov al, ah
	out dx, al

	a32 lodsd			; Right channel
	mov dx, 0x1234
	lpt_dac_dual_irq0_port2a EQU $ - 2
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	advance_buffer 0
	irq0_eoi 0

	align 4

.clip_left:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	a32 lodsd			; Right channel
	mov dx, 0x1234
	lpt_dac_dual_irq0_port2b EQU $ - 2
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	advance_buffer 0
	irq0_eoi 0

	align 4

.clip_right:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	advance_buffer 0
	irq0_eoi 0


;------------------------------------------------------------------------------
; Dual LPT DAC forced mono output IRQ 0 handler.
;------------------------------------------------------------------------------

	align 4

lpt_dac_dual_mono_irq0_handler:
	irq0_start

	; DS: player instance segment

	mov ax, 0x1234
	lpt_dac_dual_mono_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [state(buffer_pos)]
	a32 lodsd
	mov dx, 0x1234
	lpt_dac_dual_mono_irq0_port1 EQU $ - 2
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip			; Clipping
	mov al, ah
	out dx, al			; Left channel DAC

	mov dx, 0x1234
	lpt_dac_dual_mono_irq0_port2a EQU $ - 2
	out dx, al			; Right channel DAC (same sample)

	advance_buffer 4
	irq0_eoi 0

	align 4

.clip:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al			; Left channel DAC

	mov dx, 0x1234
	lpt_dac_dual_mono_irq0_port2b EQU $ - 2
	out dx, al			; Right channel DAC (same sample)

	advance_buffer 4
	irq0_eoi 0


;------------------------------------------------------------------------------
; Toggles and renders samples into the output audio buffer when the currently
; played part of the triple buffer reaches its end.
;------------------------------------------------------------------------------
; <- Destroys AH and DX
;------------------------------------------------------------------------------

	align 4

toggle_buffer_render:

	; Jumping here from toggle_buffer / .reset_buffer. Must be defined in
	; advance, otherwise the macro throws an error.
	; -> AH: new buffer status

	cmp byte [state(buffer_status)], BUF_RENDERING
	je .render_pending

	; Render into update pending buffer part

	push ax
	push ebx
	push ecx
	push edi
	push ebp
	sti				; Enable interrupts (important!)
	call render			; Render audio into output buffer
	cli				; Disable interrupts
	pop ebp
	pop edi
	pop ecx
	pop ebx
	pop ax

	; Update pending buffer part unless a render was already in progress

	cmp byte [state(buffer_status)], BUF_READY
	jne .exit
	mov byte [state(buffer_status)], ah

.exit:
	irq0_exit

	align 4

.render_pending:
	mov byte [state(buffer_pending)], ah

	irq0_exit

	align 4

toggle_buffer:

	; End of buffer reached, play next part of the triple buffer

	mov ah, [state(buffer_playprt)]
	inc ah
	cmp ah, 2
	ja .reset_buffer		; Re-init to first part
	mov [state(buffer_playprt)], ah	; Continue to 2nd/3rd part
	mov edx, [state(buffer_size)]
	add [state(buffer_limit)], edx	; Adjust buffer upper limit for playback
	add ah, BUF_RENDER_1 - 1	; Target render buffer: playing part - 1
	irq0_eoi toggle_buffer_render

	align 4

.reset_buffer:

	; Wrap back to first part of the buffer

	mov byte [state(buffer_playprt)], 0
	mov edx, [state(buffer_ofs)]
	mov [state(buffer_pos)], edx
	add edx, [state(buffer_size)]
	mov [state(buffer_limit)], edx	; Buffer upper limit: end of 1st part
	mov ah, BUF_RENDER_3		; Target render buffer: 3rd part
	irq0_eoi toggle_buffer_render


;==============================================================================
; Data area
;==============================================================================

		; Output device function pointers

		alignb 4

global mod_out_dac_fns
mod_out_dac_fns	istruc mod_out_fns
		set_out_fn(setup, setup)
		set_out_fn(shutdown, shutdown)
		set_out_fn(upload_sample, mod_swt_upload_sample)
		set_out_fn(free_sample, mod_swt_free_sample)
		set_out_fn(set_amplify, set_amplify)
		set_out_fn(set_interpol, mod_swt_set_interpolation)
		set_out_fn(set_stereomode, mod_swt_set_stereo_mode)
		set_out_fn(play, play)
		set_out_fn(stop, stop)
		set_out_fn(set_tick_rate, set_tick_rate)
		set_out_fn(set_mixer, set_mixer)
		set_out_fn(set_sample, mod_swt_set_sample)
		set_out_fn(render, render)
		iend
