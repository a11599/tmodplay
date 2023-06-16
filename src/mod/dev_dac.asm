;==============================================================================
; MOD player - DAC output device
;------------------------------------------------------------------------------
; Supports via software wavetable rendering:
; - PC speaker: 5.3 - 6-bits, up to 29000 Hz sample rate
;   (resolution inversely proportional to sample rate)
; - Parallel port DACs (aka. Covox): 8-bits, up to 100000 Hz sample rate:
;   - Mono LPT DAC on any parallel port
;   - Dual LPT DACs on any parallel ports in mono or stereo
;   - Stereo LPT DAC ("Stereo-on-1") on any parallel port in mono or stereo
;------------------------------------------------------------------------------
; All output devices utilize the timer interrupt (IRQ 0) for playback, which
; makes it unavailable for other purposes.
;------------------------------------------------------------------------------
; Performance same as software wavetable rendering, plus playback overhead due
; to timer interrupts (86Box 386DX clocks at 44192 Hz):
;
; DAC                        Cross mixing overhead MHz    Playback overhead MHz
; ---                        -------------------------    ---------------------
; Speaker/Mono LPT DAC                               -                      6.0
; Dual LPT DACs in stereo                         2.12                      6.0
; Stereo LPT DAC in stereo                        2.12                      7.2
;
; Cross mixing overhead (2.12 MHz) applies to stereo output with cross stereo
; mode only.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/string.inc"
%include "rtl/api/log.inc"
%include "rtl/api/irq.inc"
%include "rtl/api/timer.inc"

%include "mod/config.inc"
%include "mod/api/wtbl_sw.inc"
%include "mod/api/routine.inc"
%include "mod/structs/public.inc"
%include "mod/consts/public.inc"
%include "mod/structs/dev.inc"
%include "mod/consts/dev.inc"

%ifdef MOD_USE_PROFILER
extern mod_perf_ticks
%include "rtl/api/profiler.inc"
%endif

; Shortcut macros for easier access to nested structures

%define	params(var) params + mod_dev_params. %+ var
%define	set_api_fn(name, lbl) at mod_dev_api. %+ name, dd %+ (lbl)

; Printer control bits to select left/right channel for stereo LPT DAC

LPTST_CHN_LEFT	EQU 00000001b
LPTST_CHN_RIGHT	EQU 00000010b
LPTST_CHN_BOTH	EQU 00000011b


;------------------------------------------------------------------------------
; Set up the DAC output device.
;------------------------------------------------------------------------------
; -> AL - Output device type (MOD_DAC_*)
;    EBX - Pointer to mod_dev_params structure
;    CH.CL - Amplification in 8.8-bit fixed point format
;    EDX - Requested sample rate
; <- CF - Set if error
;    EAX - Error code if CF set or actual sample rate
;    EBX - Number of extra samples that will be generated at the end of each
;          sample (must reserve enough space) if no error
;    ECX - Number of extra samples that will be generated at the beginning of
;          each sample (must reserve enough space) if no error
;------------------------------------------------------------------------------

setup:
	push edx
	push esi
	push edi
	push ebx
	push ecx

	cld

	mov [dev_type], al
	mov [amplify], cx

	; Copy parameters to local instance

	mov esi, ebx
	mov edi, params
	mov ecx, (mod_dev_params.strucsize + 3) / 4
	rep movsd

	; Validate configuration

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
	cmp edx, 29000			; Limit maximum to 29 kHz for PC speaker
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
	jmp .use_output_format

.stereo_device:
	cmp byte [params(stereo_mode)], MOD_PAN_MONO
	je .use_output_format
	or ah, FMT_STEREO

.use_output_format:
	mov [output_format], ah

	%if (LOG_LEVEL >= LOG_INFO)

	; Log configuration

	cmp al, MOD_DAC_SPEAKER
	jne .check_lpt
	log LOG_INFO, {'Output device: Internal PC speaker', 13, 10}
	jmp .calc_pit_rate

.check_lpt:
	cmp al, MOD_DAC_LPT
	jne .check_lptst
	log LOG_INFO, {'Output device: Mono LPT DAC on port 0x{X16}', 13, 10}, [params(port)]
	jmp .calc_pit_rate

.check_lptst:
	cmp al, MOD_DAC_LPTST
	jne .check_lptdual
	log LOG_INFO, {'Output device: Stereo LPT DAC on port 0x{X16}', 13, 10}, [params(port)]
	jmp .calc_pit_rate

.check_lptdual:
	cmp al, MOD_DAC_LPTDUAL
	jne .calc_pit_rate
	log LOG_INFO, {'Output device: Dual LPT DACs on port 0x{X16} and 0x{X16}', 13, 10}, [params(port)], [params(port + 2)]

	%endif

.calc_pit_rate:

	; Calculate PIT/actual sample rate

	call timer_calc_rate
	mov [pit_rate], bx
	mov [sample_rate], eax		; Save actual sample rate

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
	mov [period_base], eax

	; Allocate memory for the output buffer

	mov eax, [sample_rate]		; Convert microsec to buffer size
	mov ebx, [params(buffer_size)]
	cmp ebx, 1000000
	jae .limit_buffer_size
	mul ebx
	mov ebx, 1000000
	div ebx
	cmp edx, 500000			; Rounding
	setae dl
	movzx edx, dl
	add eax, edx

.check_buffer_size:
	cmp eax, 4096			; Maximum sane buffer size
	jbe .use_buffer_size

.limit_buffer_size:
	mov eax, 4096

.use_buffer_size:
	mov [params(buffer_size)], eax

	mov ebx, eax
	shl ebx, 3			; 32-bit stereo buffer of SW wavetable
	mov [buffer_size], ebx
	lea ecx, [ebx + ebx * 2]	; Triple buffering
	mov al, PMI_MEM_HI_LO
	call pmi(mem_alloc)
	jc .error
	mov [buffer_addr], eax

	log {'Allocated {u} bytes for output device buffer at 0x{X}', 13, 10}, ecx, eax

	cmp byte [dev_type], MOD_DAC_SPEAKER
	jne .setup_wt

	; Create speaker sample lookup table

	xor ebx, ebx			; EBX: sample (0 - 255)
	movzx ecx, word [pit_rate]	; ECX: timer IRQ PIT rate (always 8-bit)
	cmp ecx, 64
	jbe .speakertab_loop
	mov ecx, 64

.speakertab_loop:
	mov al, bl
	mul cl
	inc ah
	mov [speakertab + ebx], ah
	inc bl
	jnz .speakertab_loop

.setup_wt:

	; Setup wavetable

	mov al, [params(interpolation)]
	mov ah, [params(stereo_mode)]
	mov bx, [amplify]
	xor ecx, ecx			; Render to output buffer directly
	mov dl, [output_format]
	mov dh, [params(initial_pan)]
	call mod_swt_setup
	jc .error
	mov [amplify], bx
	mov ebx, eax

	; Done

	add esp, 8			; Discard EBX and ECX from stack
	mov eax, [sample_rate]
	clc

.exit:
	pop edi
	pop esi
	pop edx
	ret

.unknown_device:
	mov eax, MOD_ERR_DEV_UNK

.error:
	pop ecx
	pop ebx
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown the output device. No further playback is possible until the setup
; function is called again.
;------------------------------------------------------------------------------

shutdown:
	push eax

	log LOG_INFO, {'Shutting down DAC output device', 13, 10}

	; Shutdown wavetable

	call mod_swt_shutdown

	; Release memory

	mov eax, [buffer_addr]
	test eax, eax
	jz .done

	log {'Disposing output buffer at 0x{X}', 13, 10}, eax

	call pmi(mem_free)
	mov dword [buffer_addr], 0

.done:
	pop eax
	ret


;------------------------------------------------------------------------------
; Set the number of active channels.
;------------------------------------------------------------------------------
; -> AL - Number of channels
;------------------------------------------------------------------------------

set_channels:
	mov [num_channels], al
	call mod_swt_set_channels

	ret


;------------------------------------------------------------------------------
; Set the amplification level.
;------------------------------------------------------------------------------
; -> AH.AL - Requested audio amplification in 8.8 fixed point value
; <- AH.AL - Actual audio amplification level
;------------------------------------------------------------------------------

	align 4

set_amplify:
	mov [amplify], ax
	call mod_swt_set_amplify
	mov [amplify], ax

	ret


;------------------------------------------------------------------------------
; Start playback on the DAC device.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

play:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	; Reset audio output

	mov dh, [params(initial_pan)]
	call mod_swt_reset_channels

	mov byte [buffer_playprt], 0
	mov dword [play_sam_int], 0
	mov dword [play_sam_fr], 0
	mov eax, [buffer_addr]
	mov [buffer_pos], eax
	add eax, [buffer_size]
	mov [buffer_limit], eax
	mov byte [playing], 1

	; Pre-render into output buffer before starting playback

	mov byte [buffer_pending], BUF_READY
	mov byte [buffer_status], BUF_RENDER_1
	call render
	mov byte [buffer_status], BUF_RENDER_2
	call render
	mov byte [buffer_status], BUF_RENDER_3
	call render

	; Setup and install IRQ 0 handler

	xor al, al
	call pmi(get_irq_hndlr)
	mov [irq0_prev_handler], edx
	mov [irq0_prev_handler + 4], cx
	movzx edx, word [params(port)]	; EDX, EDI: parallel port DAC I/O ports
	movzx edi, word [params(port + 2)]

	; Since all DAC device rely on the PC's timer interrupt, each device
	; has its own dedicated IRQ 0 handler with code optimized to be as fast
	; as possible since it's called at the frequency of the sample rate.

	cmp byte [dev_type], MOD_DAC_SPEAKER
	je .start_speaker
	cmp byte [dev_type], MOD_DAC_LPTST
	je .start_lpt_dac_stereo
	cmp byte [dev_type], MOD_DAC_LPTDUAL
	je .start_lpt_dac_dual
	cmp byte [dev_type], MOD_DAC_LPT
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
	mov word [speaker_irq0_player_segment], ds
	mov edx, speaker_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac_stereo:

	; Setup stereo LPT DAC. This device is attached to a single parallel
	; port and the currently output channel is switched via strobe and/or
	; auto linefeed pins. See LPTST_CHN_* constants for printer control
	; values.

	add edx, 2
	in al, dx
	mov [lpt_prn_ctrl], al
	test byte [output_format], FMT_STEREO
	jz .start_lpt_dac_stereo_mono
	mov word [lpt_dac_stereo_irq0_player_segment], ds
	mov [lpt_dac_stereo_irq0_ctrl_port], edx
	mov edx, lpt_dac_stereo_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac_stereo_mono:

	; Stereo LPT DAC with forced mono output. Enable both channels, then
	; start output via normal mono LPT DAC output routine.

	mov al, LPTST_CHN_BOTH
	out dx, al
	sub edx, 2
	jmp .start_lpt_dac

.start_lpt_dac_dual:

	; Setup dual LPT DAC. This requires two parallel ports with a mono
	; 8-bit DAC connected to each. Device on first port is left channel and
	; device on second port is right channel.

	test byte [output_format], FMT_STEREO
	jz .start_lpt_dac_dual_mono
	mov word [lpt_dac_dual_irq0_player_segment], ds
	mov [lpt_dac_dual_irq0_port1], edx
	mov [lpt_dac_dual_irq0_port2a], edi
	mov [lpt_dac_dual_irq0_port2b], edi
	mov edx, lpt_dac_dual_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac_dual_mono:

	; Dual LPT DAC with forced mono output

	mov word [lpt_dac_dual_mono_irq0_player_segment], ds
	mov [lpt_dac_dual_mono_irq0_port1], edx
	mov [lpt_dac_dual_mono_irq0_port2a], edi
	mov [lpt_dac_dual_mono_irq0_port2b], edi
	mov edx, lpt_dac_dual_mono_irq0_handler
	jmp .setup_irq_handler

.start_lpt_dac:

	; Mono (single) LPT DAC

	mov word [lpt_dac_irq0_player_segment], ds
	mov [lpt_dac_irq0_port1], edx
	mov edx, lpt_dac_irq0_handler

.setup_irq_handler:
	xor al, al			; AL might be destroyed above
	mov cx, cs
	call pmi(set_irq_hndlr)

	; Set the rate of the timer interrupt

	mov word [pit_tick_count], 0
	mov bx, [pit_rate]
	call timer_set_rate

	; Enable IRQ 0

	xor cl, cl
	call irq_enable

.exit:
	clc
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Stop playback on the DAC device.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

stop:
	mov byte [playing], 0

	push eax
	push ebx
	push ecx
	push edx

	; Restore the rate of the timer

	call timer_reset_rate

	; Uninstall IRQ 0 handler

	xor al, al
	mov cx, [irq0_prev_handler + 4]
	mov edx, [irq0_prev_handler]
	call pmi(set_irq_hndlr)

	; Reset state

	mov word [pit_tick_count], 0

	cmp byte [dev_type], MOD_DAC_SPEAKER
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
  	jmp .done

.stop_dac_lptst:
	cmp byte [dev_type], MOD_DAC_LPTST
	jne .done

	; Restore LPT printer controls for stereo LPT DAC

	mov dx, [params(port)]
	add dx, 2
	mov al, [lpt_prn_ctrl]
	out dx, al

.done:
	clc
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret


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
;    ECX - Playback note periods
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
	xor eax, eax
	test ecx, ecx			; Guard against division by zero hangs
	setz al
	add ecx, eax
	xor edx, edx
	mov eax, [period_base]
	and ecx, 0xffff
	div ecx
	shr ecx, 1			; ECX: period / 2
	cmp edx, ecx			; Remainder > period / 2?
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx
	mov edx, eax
	shr eax, 16
	mov ecx, eax			; ECX.DX: playback speed
	pop eax

	call mod_swt_set_mixer

	pop edx
	pop ecx
	ret


;------------------------------------------------------------------------------
; Set the playroutine callback tick rate.
;------------------------------------------------------------------------------
; -> EBX - Number of playroutine ticks per minute
;------------------------------------------------------------------------------

	align 4

set_tick_rate:
	push eax
	push ebx
	push edx

	; Calculate number of samples between player ticks

	mov eax, [sample_rate]
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
	shr ebx, 16
	shl eax, 16			; EBX.EAX: samples between ticks
	mov [play_tickr_int], ebx
	mov [play_tickr_fr], eax

	pop edx
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Render channels into the output buffer.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

	align 4

render:
	%ifdef MOD_USE_PROFILER
	call profiler_get_counter
	push eax
	%endif
	mov al, BUF_RENDERING
	xchg al, byte [buffer_status]
	cmp al, BUF_RENDERING
	je .rendering			; BUF_RENDERING: already rendering audio
	jb .noop			; BUF_READY: nothing to render
	cmp byte [playing], 1		; Not playing, don't render
	jne .exit

	; Initialize state

	push dword 0			; Update channel_info by tick counter
	push eax

	mov edx, [params(buffer_size)]	; EDX: number of samples to render
	mov ebx, [play_sam_int]		; EBX: samples until playroutine tick
	mov edi, [buffer_addr]
	cmp al, BUF_RENDER_1
	je .loop_render
	mov esi, [buffer_size]		; 2nd part of buffer
	add edi, esi
	cmp al, BUF_RENDER_2
	je .loop_render
	add edi, esi			; 3rd part of buffer

	; Render samples to the output audio buffer
	; EBX: number of samples until next playroutine tick
	; ECX: number of samples to render by software wavetable in current pass
	; EDX: number of samples to render into output audio buffer
	; EDI: linear address of output audio buffer position to render into

.loop_render:

	; Call playroutine tick when necessary

	test ebx, ebx
	jnz .calc_render_count

	push ebx
	push ecx
	push edx
	push edi
	call mod_playroutine_get_position
	movzx ebp, dl			; EBP: current tick
	call mod_playroutine_tick
	pop edi
	pop edx
	pop ecx
	pop ebx

	mov eax, [esp]			; Restore AL from stack
	test ebp, ebp
	jz .main_tick			; Force channel_info update on main tick
	cmp dword [esp + 4], 0
	ja .skip_channel_info

.main_tick:
	call update_channel_info
	inc dword [esp + 4]		; Increase channel_info tick update

.skip_channel_info:
	mov eax, [play_tickr_fr]
	add [play_sam_fr], eax
	adc ebx, 0
	add ebx, [play_tickr_int]

.calc_render_count:

	; Determine number of samples to render in this pass

	mov ecx, edx			; ECX: number of samples to render
	cmp ecx, ebx			; Don't render past playroutine tick
	jb .render_swt
	mov ecx, ebx

.render_swt:

	; Render channels using software wavetable

	push ebx
	push ecx
	push edx
	call mod_swt_render_direct
	pop edx
	pop ecx
	pop ebx

	; Calculate number of samples left to render

	lea edi, [edi + ecx * 8]
	sub ebx, ecx
	sub edx, ecx
	jnz .loop_render

	pop eax
	pop ebp				; EBP: channel_info update by tick ctr

	; Output buffer completely rendered

	mov [play_sam_int], ebx		; Update samples until playroutine tick
	call update_buffer_position
	test ebp, ebp
	jnz .noop
	call update_channel_info	; Update channel_info when no tick

.noop:

	; Done rendering or nothing to do (no part of the buffer needs new audio
	; data)

	mov al, BUF_READY
	xchg al, [buffer_pending]
	mov [buffer_status], al

.exit:
	%ifdef MOD_USE_PROFILER
	call profiler_get_counter
	pop ebx
	sub eax, ebx
	add [mod_perf_ticks], eax
	%endif
	ret

.rendering:
	mov al, [buffer_pending]
	cmp al, BUF_RENDER_1
	jb .exit
	call update_buffer_position
	jmp .exit


;------------------------------------------------------------------------------
; Return information about output device.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_channel_info structures
; <- ESI - Filled with data
;------------------------------------------------------------------------------

get_info:
	push eax
	push ecx

	; Buffer info

	mov eax, [sample_rate]
	mov [esi + mod_output_info.sample_rate], eax
	mov eax, [buffer_addr]
	mov [esi + mod_output_info.buffer_addr], eax
	mov eax, [buffer_size]
	lea eax, [eax + eax * 2]	; Triple buffering
	mov [esi + mod_output_info.buffer_size], eax
	mov eax, [buffer_pos]
	sub eax, [buffer_addr]
	mov [esi + mod_output_info.buffer_pos], eax

.format:

	; Calculate buffer format, always 2 channel 16-bit dword unsigned (same
	; as software wavetable render buffer format), but only left channel
	; used when output device is mono

	mov cl, [output_format]
	and cl, FMT_CHANNELS
	cmp cl, FMT_STEREO
	je .stereo
	mov byte [esi + mod_output_info.buffer_format], MOD_BUF_1632BIT | MOD_BUF_2CHNL | MOD_BUF_INT
	jmp .done

.stereo:
	mov byte [esi + mod_output_info.buffer_format], MOD_BUF_1632BIT | MOD_BUF_2CHN | MOD_BUF_INT

.done:
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Return current MOD playback position.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_position_info structures
; <- ESI - Filled with data
;------------------------------------------------------------------------------

get_position:
	push ecx
	push esi
	push edi

	mov edi, esi
	xor esi, esi
	cmp byte [buffer_playprt], 1
	jb .done
	mov esi, mod_position_info.strucsize
	je .done
	add esi, esi

.done:
	add esi, position_info
	mov ecx, (mod_position_info.strucsize) / 4
	rep movsd

	pop edi
	pop esi
	pop ecx
	ret


;------------------------------------------------------------------------------
; Update song position information for a specific buffer part.
;------------------------------------------------------------------------------
; -> AL - Buffer part number (BUF_RENDER_n)
;------------------------------------------------------------------------------

update_buffer_position:
	push esi

	xor esi, esi			; Save playback position for this buffer
	cmp al, BUF_RENDER_2
	jb .get_position
	mov esi, mod_position_info.strucsize
	je .get_position
	add esi, esi

.get_position:
	add esi, position_info
	call mod_playroutine_get_position_info

	pop esi
	ret


;------------------------------------------------------------------------------
; Return current MOD channel info.
;------------------------------------------------------------------------------
; -> ESI - Pointer to buffer receiving mod_channel_info structures
; <- ESI - Filled with data
;------------------------------------------------------------------------------

get_channel_info:
	push ecx
	push esi
	push edi

	mov edi, esi
	mov ecx, (mod_channel_info.strucsize) / 4
	movzx esi, byte [num_channels]
	imul ecx, esi			; ECX: channel_info[] size
	xor esi, esi
	cmp byte [buffer_playprt], 1
	jb .done
	mov esi, mod_channel_info.strucsize * MOD_MAX_CHANS
	je .done
	add esi, esi

.done:
	add esi, channel_info
	rep movsd

	pop edi
	pop esi
	pop ecx
	ret


;------------------------------------------------------------------------------
; Update channel information for a specific buffer part.
;------------------------------------------------------------------------------
; -> AL - Buffer part number (BUF_RENDER_n)
;------------------------------------------------------------------------------

update_channel_info:
	push esi

	xor esi, esi			; Save channel_info for this buffer
	cmp al, BUF_RENDER_2
	jb .get_info
	mov esi, mod_channel_info.strucsize * MOD_MAX_CHANS
	je .get_info
	add esi, esi

.get_info:
	add esi, channel_info
	call mod_playroutine_get_channel_info
	call mod_swt_get_mixer_info

	pop esi
	ret


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
; -> %1 - Jump target when ready or 0 to exit IRQ 0
; <- Destroys AL and DX
;------------------------------------------------------------------------------

%macro	irq0_eoi 1

	mov dx, [pit_rate]		; Call old handler if internal tick
	add [pit_tick_count], dx	; counter overflows 16-bit
	jc %%call_prev_handler

	irq_pic_eoi 0			; Destroys AL

	%if (%1 = 0)			; Exit IRQ 0 or jump to target label
	irq0_exit
	%else
	jmp %1
	%endif

	align 16

%%call_prev_handler:
	call prev_irq0_handler		; Call previous handler

	%if (%1 = 0)			; Exit IRQ 0 or jump to target label
	irq0_exit
	%else
	jmp %1
	%endif

%endmacro


;------------------------------------------------------------------------------
; Flip buffer if end is reached.
;------------------------------------------------------------------------------
; -> ESI - Current buffer position
;------------------------------------------------------------------------------

%macro	advance_buffer 0

	; Flip buffer if end is reached

	cmp esi, [buffer_limit]
	jae toggle_buffer

%endmacro


;------------------------------------------------------------------------------
; Call the original IRQ 0 handler.
;------------------------------------------------------------------------------

	align 16

prev_irq0_handler:
	pushfd
	call 0x1234:0x12345678
	irq0_prev_handler EQU $ - 6
	ret


;------------------------------------------------------------------------------
; PC speaker IRQ 0 handler.
;------------------------------------------------------------------------------

	align 16

speaker_irq0_handler:
	irq0_start

	; DS: player instance segment

	mov ax, 0x1234
	speaker_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [buffer_pos]
	mov eax, [esi]
	add eax, 0x8080			; Convert to unsigned
	add esi, 8
	test eax, 0xffff0000
	jnz .clip			; Clipping
	xor edx, edx
	mov dl, ah
	mov al, [speakertab + edx]
	out 0x42, al
	mov [buffer_pos], esi

	advance_buffer
	irq0_eoi 0

	align 16

.clip:
	xor edx, edx
	cmp eax, 0
	setg dl				; AL: 1 if positive clip, else 0
	neg dl				; AL: 255 if positive clip, else 0
	mov al, [speakertab + edx]
	out 0x42, al
	mov [buffer_pos], esi

	advance_buffer
	irq0_eoi 0


;------------------------------------------------------------------------------
; Mono LPT DAC IRQ 0 handler.
;------------------------------------------------------------------------------

	align 16

lpt_dac_irq0_handler:
	irq0_start

	mov ax, 0x1234
	lpt_dac_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [buffer_pos]
	mov eax, [esi]
	mov edx, 0x12345678
	lpt_dac_irq0_port1 EQU $ - 4
	add eax, 0x8080			; Convert to unsigned
	add esi, 8
	test eax, 0xffff0000
	jnz .clip			; Clipping
	mov [buffer_pos], esi
	mov al, ah
	out dx, al

	advance_buffer
	irq0_eoi 0

	align 16

.clip:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	mov [buffer_pos], esi
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	advance_buffer
	irq0_eoi 0


;------------------------------------------------------------------------------
; Stereo LPT DAC output IRQ 0 handler.
;------------------------------------------------------------------------------

	align 16

lpt_dac_stereo_irq0_handler:
	irq0_start

	mov ax, 0x1234
	lpt_dac_stereo_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [buffer_pos]
	mov edx, 0x12345678
	lpt_dac_stereo_irq0_ctrl_port EQU $ - 4

	; Select DAC left channel

	mov al, LPTST_CHN_LEFT
	out dx, al
	add esi, 8
	sub edx, 2

	; Output left channel sample

	mov eax, [esi - 8]		; Left channel sample
	mov [buffer_pos], esi
	mov esi, [esi - 4]		; Right channel sample
	add eax, 0x8080			; Convert to unsigned
	add esi, 0x8080
	test eax, 0xffff0000
	jnz .clip_left			; Clipping
	mov al, ah
	out dx, al

	; Select DAC right channel

	add edx, 2
	mov al, LPTST_CHN_RIGHT
	out dx, al
	sub edx, 2

	; Output right channel sample

	mov eax, esi
	test esi, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	mov esi, [buffer_pos]
	advance_buffer
	irq0_eoi 0

	align 16

.clip_left:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	; Select DAC right channel

	add edx, 2
	mov al, LPTST_CHN_RIGHT
	out dx, al
	sub edx, 2

	; Output right channel sample

	mov eax, esi
	test esi, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	mov esi, [buffer_pos]
	advance_buffer
	irq0_eoi 0

	align 16

.clip_right:
	cmp esi, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	mov esi, [buffer_pos]
	advance_buffer
	irq0_eoi 0


;------------------------------------------------------------------------------
; Dual LPT DAC output IRQ 0 handler.
;------------------------------------------------------------------------------

	align 16

lpt_dac_dual_irq0_handler:
	irq0_start

	mov ax, 0x1234
	lpt_dac_dual_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [buffer_pos]
	mov edx, 0x1234
	lpt_dac_dual_irq0_port1 EQU $ - 4
	add esi, 8
	mov eax, [esi - 8]		; Left channel sample
	mov [buffer_pos], esi
	mov esi, [esi - 4]		; Right channel sample
	add eax, 0x8080			; Convert to unsigned
	add esi, 0x8080
	test eax, 0xffff0000
	jnz .clip_left			; Clipping
	mov al, ah
	out dx, al

	mov eax, esi
	mov edx, 0x1234
	lpt_dac_dual_irq0_port2a EQU $ - 4
	test esi, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	mov esi, [buffer_pos]
	advance_buffer
	irq0_eoi 0

	align 16

.clip_left:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	mov edx, 0x1234
	lpt_dac_dual_irq0_port2b EQU $ - 4
	mov eax, esi
	test esi, 0xffff0000
	jnz .clip_right			; Clipping
	mov al, ah
	out dx, al

	mov esi, [buffer_pos]
	advance_buffer
	irq0_eoi 0

	align 16

.clip_right:
	cmp esi, 0
	setg al				; AL: 1 if positive clip, else 0
	neg al				; AL: 255 if positive clip, else 0
	out dx, al

	mov esi, [buffer_pos]
	advance_buffer
	irq0_eoi 0


;------------------------------------------------------------------------------
; Dual LPT DAC forced mono output IRQ 0 handler.
;------------------------------------------------------------------------------

	align 16

lpt_dac_dual_mono_irq0_handler:
	irq0_start

	mov ax, 0x1234
	lpt_dac_dual_mono_irq0_player_segment EQU $ - 2
	mov ds, ax

	; Output next sample

	mov esi, [buffer_pos]
	mov edx, 0x1234
	lpt_dac_dual_mono_irq0_port1 EQU $ - 4
	mov eax, [esi]
	add esi, 8
	add eax, 0x8080			; Convert to unsigned
	test eax, 0xffff0000
	jnz .clip			; Clipping
	mov [buffer_pos], esi
	mov al, ah
	out dx, al			; Left channel DAC

	mov edx, 0x1234
	lpt_dac_dual_mono_irq0_port2a EQU $ - 4
	out dx, al			; Right channel DAC (same sample)

	advance_buffer
	irq0_eoi 0

	align 16

.clip:
	cmp eax, 0
	setg al				; AL: 1 if positive clip, else 0
	mov [buffer_pos], esi
	neg al				; AL: 255 if positive clip, else 0
	out dx, al			; Left channel DAC

	mov edx, 0x1234
	lpt_dac_dual_mono_irq0_port2b EQU $ - 4
	out dx, al			; Right channel DAC (same sample)

	advance_buffer
	irq0_eoi 0


;------------------------------------------------------------------------------
; Toggles and renders samples into the output audio buffer when the currently
; played part of the triple buffer reaches its end.
;------------------------------------------------------------------------------
; <- Destroys AH and DX
;------------------------------------------------------------------------------

toggle_buffer_render:

	; Jumping here from toggle_buffer / .reset_buffer. Must be defined in
	; advance, otherwise the macro throws an error.
	; -> AH: new buffer status

	cmp byte [buffer_status], BUF_RENDERING
	je .render_pending

	; Render into update pending buffer part

	push eax
	push ebx
	push ecx
	push edi
	push ebp
	push es
	mov ax, ds
	mov es, ax			; ES: flat memory model data selector
	sti				; Enable interrupts (important!)
	call render			; Render audio into output buffer
	cli				; Disable interrupts
	pop es
	pop ebp
	pop edi
	pop ecx
	pop ebx
	pop eax

	; Update pending buffer part unless a render was already in progress

	cmp byte [buffer_status], BUF_READY
	jne .exit
	mov byte [buffer_status], ah
	mov al, ah
	call update_buffer_position

.exit:
	irq0_exit

.render_pending:
	mov byte [buffer_pending], ah

	irq0_exit

toggle_buffer:

	; End of buffer reached, play next part of the triple buffer

	mov ah, [buffer_playprt]
	inc ah
	cmp ah, 2
	ja .reset_buffer		; Re-init to first part
	mov [buffer_playprt], ah	; Continue to 2nd/3rd part
	mov edx, [buffer_size]
	add [buffer_limit], edx		; Adjust buffer upper limit for playback
	add ah, BUF_RENDER_1 - 1	; Target render buffer: playing part - 1
	irq0_eoi toggle_buffer_render

.reset_buffer:

	; Wrap back to first part of the buffer

	mov byte [buffer_playprt], 0
	mov edx, [buffer_addr]
	mov [buffer_pos], edx
	add edx, [buffer_size]
	mov [buffer_limit], edx		; Buffer upper limit: end of 1st part
	mov ah, BUF_RENDER_3		; Target render buffer: 3rd part
	irq0_eoi toggle_buffer_render


;==============================================================================
; Data area
;==============================================================================

section .data

		; Output device API jump table

global mod_dev_dac_api
mod_dev_dac_api	istruc mod_dev_api
		set_api_fn(setup, setup)
		set_api_fn(shutdown, shutdown)
		set_api_fn(upload_sample, mod_swt_upload_sample)
		set_api_fn(free_sample, mod_swt_free_sample)
		set_api_fn(set_channels, set_channels)
		set_api_fn(set_amplify, set_amplify)
		set_api_fn(set_interpol, mod_swt_set_interpolation)
		set_api_fn(set_stereomode, mod_swt_set_stereo_mode)
		set_api_fn(play, play)
		set_api_fn(stop, stop)
		set_api_fn(set_tick_rate, set_tick_rate)
		set_api_fn(set_mixer, set_mixer)
		set_api_fn(set_sample, mod_swt_set_sample)
		set_api_fn(render, render)
		set_api_fn(get_chn_info, get_channel_info)
		set_api_fn(get_info, get_info)
		set_api_fn(get_position, get_position)
		set_api_fn(reset_channels, mod_swt_reset_channels)
		iend

num_channels	db 0			; Number of active channels

section .bss

position_info	resb mod_position_info.strucsize * 3
channel_info	resb mod_channel_info.strucsize * MOD_MAX_CHANS * 3
params		resd (mod_dev_params.strucsize + 3) / 4
period_base	resd 1			; Period to speed conversion base
sample_rate	resd 1			; Actual playback sample rate
buffer_addr	resd 1			; Linear address of output buffer
buffer_size	resd 1			; Size of the output buffer
buffer_limit	resd 1			; End of current playing buffer
buffer_pos	resd 1			; Buffer playback position

play_tickr_int	resd 1			; Number of samples between player ticks
play_tickr_fr	resd 1			; Fraction part of the above
play_sam_int	resd 1			; Number of samples until next tick
play_sam_fr	resd 1			; Fraction part of the above
amplify		resw 1			; Output amplification

pit_rate	resw 1			; IRQ 0 rate
pit_tick_count	resw 1			; Counter for old IRQ 0 handler callback

buffer_playprt	resb 1			; Which of the double buffers is playing
buffer_status	resb 1			; Flag to indicate need for rendering
buffer_pending	resb 1			; Pending render into buffer
dev_type	resb 1			; Output DAC device type
output_format	resb 1			; Output bitstream format
lpt_prn_ctrl	resb 1			; Old LPT printer controls byte value
playing		resb 1			; Flag for playback ongoing
speakertab	resb 256		; PC speaker PWM conversion lookup table
