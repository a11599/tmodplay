;==============================================================================
; MOD player - Sound Blaster output device
;------------------------------------------------------------------------------
; Supports via software wavetable rendering:
; - Sound Blaster: 8-bit mono, up to 22050 Hz sample rate
; - Sound Blaster 2.0: 8-bit mono, up to 44100 Hz sample rate
; - Sound Blaster Pro:
;   - 8-bit mono, up to 44100 Hz sample rate
;   - 8-bit stereo, up to 22050 Hz sample rate
; - Sound Blaster 16: 16-bit stereo, up to 44100 Hz sample rate
;------------------------------------------------------------------------------
; Performance basically same as software wavetable rendering + mixing,
; negligible overhead due to DMA transfers.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/env_arg.inc"
%include "rtl/api/string.inc"
%include "rtl/api/log.inc"
%include "rtl/api/irq.inc"
%include "rtl/api/systimer.inc"

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


;------------------------------------------------------------------------------
; Read data from the Sound Blaster DSP.
;------------------------------------------------------------------------------
; -> DX - Any Sound Blaster port
;    %1 - Current Sound Blaster port offset ("y" of 2xy from port in DX)
; <- AL - Value read
;    DX - 2xA, DSP read data port
;------------------------------------------------------------------------------

%macro	read_dsp 1

	%if (%1 != 0x0e)
	add dl, (0x0e - %1)		; DX: 2xE, DSP read-buffer status port
	%endif

%%busy_loop:
	in al, dx			; Check if data is available
	or al, al
	jns %%busy_loop

	sub dl, 4			; DX: 2xA, DSP read data port
	in al, dx			; Get data

%endmacro


;------------------------------------------------------------------------------
; Write data to the Sound Blaster DSP.
;------------------------------------------------------------------------------
; -> DX - Any Sound Blaster port
;    %1 - Current Sound Blaster port offset ("y" of 2xy from port in DX)
;    %2 - Byte value to write (cannot be AL, but can be a constant)
; <- AL - Destroyed
;    DX - 2xC, DSP write command/data port
;------------------------------------------------------------------------------

%macro	write_dsp 2

	%if (%1 != 0x0c)
	add dl, (0x0c - %1)		; DX: 2xC, DSP write command/data port
	%endif

%%busy_loop:
	in al, dx			; Check if DSP is ready to accept data
	or al, al
	js %%busy_loop

	mov al, %2
	out dx, al			; Write data

%endmacro


;------------------------------------------------------------------------------
; Detects the presence and parameters of a Sound Blaster device using the
; BLASTER environment variable.
;------------------------------------------------------------------------------
; -> EBX - mod_dev_params structure receiving detected parameters
; <- CF - Set if Sound Blaster is not present
;    AL - Device type when CF not set (MOD_SB_*)
;    EBX - Filled with Sound Blaster-specific parameters when CF not set
;------------------------------------------------------------------------------

global mod_sb_detect
mod_sb_detect:
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push eax

	mov ebp, ebx			; EBP: mod_dev_params pointer

	; Initialize detected values

	xor edi, edi			; DI: port
	xor edx, edx			; DL: 8-bit DMA, DH: 16-bit DMA
	xor cl, cl			; CL: IRQ
	mov ch, -1			; CH: type

	mov esi, blaster_env		; ESI: "BLASTER"
	call env_get_value
	jc .error			; No environment variable, bail out

.get_params_loop:
	mov al, [esi]			; Get next character
	inc esi

.check_param_type:
	test al, al			; End of variable value, sanity check
	jz .check_params
	cmp al, " "			; Space, check next character
	je .get_params_loop

	cmp al, "A"
	jne .check_8bit_dma

	; Axxx: I/O port address

	mov bx, 0xff20			; Terminate on space or NUL
	call str_parse_hex		; Parse value after "A"
	jc .error
	cmp eax, 0xfff0			; Can't be above FFF0
	ja .error
	mov edi, eax			; Store I/O port
	jmp .next_param

.check_8bit_dma:
	cmp al, "D"
	jne .check_16bit_dma

	; Dx: 8-bit DMA channel

	mov bx, 0xff20			; Terminate on space or NUL
	call str_parse_int		; Parse value after "D"
	jc .error
	cmp eax, 7			; Can't be above 7
	ja .error
	mov dl, al
	jmp .next_param

.check_16bit_dma:
	cmp al, "H"
	jne .check_irq

	; Hx: 16-bit DMA channel

	mov bx, 0xff20			; Terminate on space or NUL
	call str_parse_int		; Parse value after "H"
	jc .error
	cmp eax, 7			; Can't be above 7
	ja .error
	mov dh, al
	jmp .next_param

.check_irq:
	cmp al, "I"
	jne .check_type

	; Ixx: IRQ number

	mov bx, 0xff20			; Terminate on space or NUL
	call str_parse_int		; Parse value after "I"
	jc .error
	cmp eax, 15			; Can't be above 15
	ja .error
	mov cl, al
	jmp .next_param

.check_type:
	cmp al, "T"
	jne .next_param

	; Tx: Sound Blaster card type

	mov bx, 0xff20			; Terminate on space or NUL
	call str_parse_int		; Parse value after "T"
	jc .error
	mov ch, MOD_SB_1
	cmp al, 1			; 1: Sound Blaster
	jb .error			; 0: invalid
	je .next_param
	mov ch, MOD_SB_2
	cmp al, 3			; 3: Sound Blaster 2.0
	je .next_param
	mov ch, MOD_SB_PRO
	cmp al, 6			; 2/4/5: Sound Blaster Pro
	jb .next_param
	mov ch, MOD_SB_16		; 6+: Sound Blaster 16 or newer

.next_param:
	mov al, [esi]			; Skip to next parameter in variable
	inc esi
	test al, al
	jz .check_params
	cmp al, " "
	je .next_param
	jmp .check_param_type

.check_params:

	; Check if enough parameters were read from the environment variable

	test edi, edi			; I/O port address
	jz .error
	test cl, cl			; IRQ
	jz .error
	test dl, dl			; 8-bit DMA
	jz .error
	cmp ch, -1			; Type
	je .error
	test dh, dh			; Fallback to 8-bit DMA if no 16-bit DMA
	jnz .save_params
	mov dh, dl

.save_params:
	mov ebx, ebp			; EBX: mod_dev_params pointer

	; Attempt to reset the DSP of the Sound Blaster as a sanity test of
	; variables parsed from BLASTER environment variable

	mov ebp, edx
	mov edx, edi
	call reset_dsp
	jc .error			; Failed, Sound Blaster not present

	; Save parsed parameter values

	mov [ebx + mod_dev_params.port], di
	mov [ebx + mod_dev_params.irq], cl
	mov [ebx + mod_dev_params.dma], bp

	pop eax
	mov al, ch			; Return device type in AL
	clc

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	ret

.error:
	pop eax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Set up the Sound Blaster output device.
;------------------------------------------------------------------------------
; -> AL - Output device type (MOD_SB_*)
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

	cmp al, MOD_SB_16
	ja .unknown_device
	cmp edx, 8000			; Force minimum 8 kHz samplerate (which
	jae .check_sample_rate_max	; will still sound awful)
	mov edx, 8000

.check_sample_rate_max:
	cmp edx, 44100			; Limit maximum to 44.1 kHz
	jbe .check_sb1
	mov edx, 44100

.check_sb1:
	cmp al, MOD_SB_1
	jne .check_sb2
	mov ah, FMT_8BIT | FMT_MONO | FMT_UNSIGNED
	jmp .limit_22khz

.check_sb2:
	cmp al, MOD_SB_2
	jne .check_sbpro
	mov ah, FMT_8BIT | FMT_MONO | FMT_UNSIGNED
	jmp .save_config

.check_sbpro:
	cmp al, MOD_SB_PRO
	jne .check_sb16
	mov ah, FMT_8BIT | FMT_STEREO | FMT_UNSIGNED
	cmp byte [params(stereo_mode)], MOD_PAN_MONO
	je .save_config

.limit_22khz:
	cmp edx, 22050			; Limit SB / Pro stereo max sample rate
	jbe .save_config
	mov edx, 22050
	jmp .save_config

.check_sb16:
	mov ah, FMT_16BIT | FMT_STEREO | FMT_UNSIGNED

.save_config:
	cmp byte [params(stereo_mode)], MOD_PAN_MONO
	jne .use_output_format
	and ah, ~FMT_CHANNELS
	or ah, FMT_MONO

.use_output_format:
	mov [output_format], ah

	%if (LOG_LEVEL >= LOG_INFO)

	; Log configuration

	movzx ecx, al
	mov al, [params(dma)]
	cmp byte [dev_type], MOD_SB_16
	jne .log_sb
	mov ah, [params(dma) + 1]
	test ah, ah
	jz .log_sb
	mov al, ah

.log_sb:
	log LOG_INFO, {'Output device: Sound Blaster{s} on port 0x{X16}, IRQ {u8}, DMA {u8}', 13, 10}, [types + ecx * 4], [params(port)], [params(irq)], al

	%endif

.reset_device:

	; Reset Sound Blaster DSP

	push edx
	mov dx, [params(port)]
	call reset_dsp
	pop edx
	jc .sb_error

.calc_sample_rate:

	; Calculate time constant/actual sample rate

	mov eax, edx			; SB16 can use sample rate directly
	cmp byte [dev_type], MOD_SB_16
	je .use_sample_rate

	; Time constant only for SB/SB Pro

	xor cl, cl
	cmp byte [dev_type], MOD_SB_PRO
	jne .mono_time_constant
	cmp byte [params(stereo_mode)], MOD_PAN_MONO
	setne cl			; CL: 1 if SB Pro (stereo), else 0

.mono_time_constant:
	mov ebx, edx
	shl ebx, cl			; * 2 for SB Pro stereo output
	mov eax, 256000000
	xor edx, edx
	div ebx
	add eax, 128
	xor al, al
	mov edx, 65536
	sub edx, eax
	mov [time_constant], dh		; Save time constant
	mov ebx, eax
	shl ebx, cl			; * 2 for SB Pro stereo output
	mov eax, 256000000
	xor edx, edx
	div ebx
	shr ebx, 1			; EBX: time constant / 2
	cmp edx, ebx			; Remainder > time constant / 2
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx

.use_sample_rate:
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
	cmp eax, 5456			; Maximum safe buffer size
	jbe .use_buffer_size

.limit_buffer_size:
	mov eax, 5456

.use_buffer_size:
	mov [params(buffer_size)], eax

	mov ebx, eax
	test byte [output_format], FMT_16BIT
	setnz cl			; CL: 1 when 16 bit, else 0
	test byte [output_format], FMT_STEREO
	setnz ch			; CH: 1 when stereo, else 0
	add cl, ch
	shl ebx, cl			; Calculate buffer size in bytes
	mov [shl_per_sample], cl	; Save shift left count per sample
	mov [buffer_size], ebx
	lea ecx, [ebx + ebx * 2]	; Triple buffering
	mov al, PMI_MEM_DMA
	call pmi(mem_alloc)
	jc .error
	mov [buffer_addr], eax
	mov [dma_addr], ebx

	log LOG_DEBUG, {'Allocated {u} bytes for output device DMA buffer at 0x{X}', 13, 10, 'DMA buffer physical address: 0x{X}', 13, 10}, ecx, eax, ebx

	; Setup wavetable

	mov al, [params(interpolation)]
	mov ah, [params(stereo_mode)]
	mov bx, [amplify]
	mov ecx, [params(buffer_size)]
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

.sb_error:
	mov eax, MOD_ERR_DEVICE
	jmp .error

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

	log LOG_INFO, {'Shutting down Sound Blaster output device', 13, 10}

	; Shutdown wavetable

	call mod_swt_shutdown

	; Release memory

	mov eax, [buffer_addr]
	test eax, eax
	jz .done

	log LOG_DEBUG, {'Disposing output device DMA buffer at 0x{X}', 13, 10}, eax

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
; Start playback on the Sound Blaster device.
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
	mov byte [playing], 1

	; Pre-render into output buffer before starting playback

	mov byte [buffer_pending], BUF_READY
	mov byte [buffer_status], BUF_RENDER_1
	call render
	mov byte [buffer_status], BUF_RENDER_2
	call render
	mov byte [buffer_status], BUF_RENDER_3
	call render

	; Setup and install IRQ handler

	mov word [irq_player_segment], ds
	mov al, [params(irq)]
	call pmi(get_irq_hndlr)
	mov [params(irq)], al		; Update with real IRQ
	mov [irq_prev_handler], edx
	mov [irq_prev_handler + 4], cx
	mov cx, cs
	mov edx, irq_handler
	call pmi(set_irq_hndlr)

	; Enable IRQ

	mov cl, [params(irq)]
	call irq_enable

	; Setup Sound Blaster for audio playback

	mov dx, [params(port)]
	mov ebx, [params(buffer_size)]
	test byte [output_format], FMT_STEREO
	setnz cl			; CL: 1 when stereo output, else 0
	shl ebx, cl			; Twice the count when stereo
	dec ebx
	mov byte [sbpro_init], 0

	cmp byte [dev_type], MOD_SB_16
	je .start_sb16
	cmp byte [dev_type], MOD_SB_PRO
	je .start_sbpro
	cmp byte [dev_type], MOD_SB_2
	je .start_sb2

	; Initialize Sound Blaster playback

	call .setup_autoinit_dma	; Setup DMA controller (with autoinit)

	write_dsp 0, 0xd1		; Turn on DAC speaker
	write_dsp 0x0c, 0x40		; Set time constant
	write_dsp 0x0c, [time_constant]
	write_dsp 0x0c, 0x14		; Set buffer size and start 8-bit
	write_dsp 0x0c, bl		; single-cycle DMA output
	write_dsp 0x0c, bh

	jmp .exit

.start_sb2:

	; Initialize Sound Blaster 2.0 playback

	call .setup_autoinit_dma	; Setup DMA controller

	write_dsp 0, 0xd1		; Turn on DAC speaker
	write_dsp 0x0c, 0x40		; Set time constant
	write_dsp 0x0c, [time_constant]
	write_dsp 0x0c, 0x48		; Set buffer size
	write_dsp 0x0c, bl
	write_dsp 0x0c, bh
	write_dsp 0x0c, 0x90		; Start auto-init high speed DMA

	jmp .exit

.start_sbpro:

	; Initialize Sound Blaster Pro playback

	write_dsp 0, 0xd1		; Turn on DAC speaker

	test byte [output_format], FMT_STEREO
	jz .sbpro_mono
	sub dl, 0x08			; DX: 2x4: mixer port
	mov al, 0x0e
	out dx, al
	inc dx
	in al, dx
	or al, 0x02
	out dx, al			; Enable stereo mode

	; Send a single 1-sample silence to fix channel swapping bug on some
	; SB Pro models (although this does cause channel swap on DOSBox).

	push ebx			; Preserve original buffer length
	mov byte [sbpro_init], 1
	mov dl, [params(dma)]
	call pmi(dma_stop)
	mov ebx, [buffer_addr]
	mov dword [ebx], 0x80808080
	mov ebx, [dma_addr]
	mov ecx, 1
	mov dh, PMI_DMA_READ | PMI_DMA_SINGLE
	call pmi(dma_start)
	pop ebx

	mov dx, [params(port)]
	write_dsp 0, 0x14
	write_dsp 0x0c, 0
	write_dsp 0x0c, 0
	mov ecx, [0x46c]		; Save timer tick count

.wait_sbpro_init:
	mov eax, [0x46c]		; Wait for up to 2 ticks (110 ms)
	sub eax, ecx
	cmp eax, 2
	ja .sbpro_mono
	cmp byte [sbpro_init], 1
	je .wait_sbpro_init

.sbpro_mono:
	mov dx, [params(port)]

	; Start output

	call .setup_autoinit_dma	; Setup DMA controller

	write_dsp 0, 0x40		; Set time constant
	write_dsp 0x0c, [time_constant]

	sub dl, 0x08			; DX: 2x4: mixer port
	mov al, 0x0e
	out dx, al
	inc dx
	in al, dx
	mov [sbpro_filter], al
	or al, 0x20
	out dx, al			; Turn off output filter

	write_dsp 0x05, 0x48		; Set buffer size
	write_dsp 0x0c, bl
	write_dsp 0x0c, bh
	write_dsp 0x0c, 0x90		; Start auto-init high speed DMA

	jmp .exit

.start_sb16:
	mov ecx, [sample_rate]

	; Initialize Sound Blaster 16 playback

	call .setup_autoinit_dma	; Setup DMA controller

	mov ah, 0x20
	test byte [output_format], FMT_STEREO
	jne .mono_sb16
	xor ah, ah

.mono_sb16:
	write_dsp 0, 0x41		; Set sample rate
	write_dsp 0x0c, ch
	write_dsp 0x0c, cl
	write_dsp 0x0c, 0xb6		; Initialize 16-bit stereo output
	write_dsp 0x0c, ah		; 16-bit unsigned stereo PCM
	write_dsp 0x0c, bl		; Buffer size in samples
	write_dsp 0x0c, bh

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


; Setup DMA controller for auto-init audio data transfer.

.setup_autoinit_dma:
	push ebx
	push ecx
	push edx

	mov dl, [params(dma)]
	cmp byte [dev_type], MOD_SB_16
	jne .setup_dma
	mov dh, [params(dma) + 1]	; Use high DMA for 16-bit output if
	test dh, dh			; specified
	jz .setup_dma
	mov dl, dh

.setup_dma:
	call pmi(dma_stop)
	mov ebx, [dma_addr]
	mov ecx, [buffer_size]
	lea ecx, [ecx + ecx * 2]	; Triple buffering
	mov dh, PMI_DMA_READ | PMI_DMA_AUTO | PMI_DMA_SINGLE
	call pmi(dma_start)

	pop edx
	pop ecx
	pop ebx
	ret


;------------------------------------------------------------------------------
; Stop playback on the Sound Blaster device.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

stop:
	mov byte [playing], 0

	push eax
	push ebx
	push ecx

	; Halt DMA operation, then terminate auto-initialize DMA

	mov dl, [params(dma)]
	cmp byte [dev_type], MOD_SB_16
	jne .setup_dma
	mov dh, [params(dma) + 1]	; Use high DMA for 16-bit output if
	test dh, dh			; specified
	jz .setup_dma
	mov dl, dh

.setup_dma:
	call pmi(dma_stop)

	mov dx, [params(port)]

	cmp byte [dev_type], MOD_SB_PRO
	je .stop_sbpro
	cmp byte [dev_type], MOD_SB_16
	je .stop_sb16

	; Stop Sound Blaster 2.0 playback

	call reset_dsp
	write_dsp 0, 0xd3		; Turn off DAC speaker

	jmp .reset_irq

.stop_sbpro:

	; Stop Sound Blaster Pro playback

	call reset_dsp

	add dl, 0x04
	mov al, 0x0e
	out dx, al
	inc dx
	mov al, [sbpro_filter]
	out dx, al			; Restore filter
	dec dx
	mov al, 0x0e
	out dx, al
	inc dx
	in al, dx
	and al, 0xfd
	out dx, al			; Disable stereo mode

	write_dsp 0x05, 0xd3		; Turn off DAC speaker

	jmp .reset_irq

.stop_sb16:

	; Stop Sound Blaster 16 playback

	write_dsp 0, 0xd5
	write_dsp 0x0c, 0xd9

.reset_irq:

	; Uninstall IRQ handler

	mov al, [params(irq)]
	mov cx, [irq_prev_handler + 4]
	mov edx, [irq_prev_handler]
	call pmi(set_irq_hndlr)

	clc
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
	call mod_swt_render
	pop edx
	pop ecx
	pop ebx

	; Calculate number of samples left to render

	sub ebx, ecx
	sub edx, ecx
	jnz .loop_render

	pop eax
	pop ebp				; EBP: channel_info update by tick ctr

	; Output buffer completely rendered

	mov [play_sam_int], ebx		; Update samples until playroutine tick
	call update_buffer_position	; Update position_info for buffer
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
; -> ESI - Pointer to buffer receiving mod_output_info structures
; <- ESI - Filled with data
;------------------------------------------------------------------------------

get_info:
	push eax
	push ecx
	push edx

	; Buffer info

	mov eax, [sample_rate]
	mov [esi + mod_output_info.sample_rate], eax
	mov eax, [buffer_addr]
	mov [esi + mod_output_info.buffer_addr], eax
	mov eax, [buffer_size]
	mov edx, eax			; EDX: buffer size
	lea eax, [eax + eax * 2]	; Triple buffering
	mov [esi + mod_output_info.buffer_size], eax
	xor eax, eax			; EAX: current buffer start position
	cmp byte [buffer_playprt], 1
	jb .add_partial			; EAX: 0 (first buffer)
	mov eax, edx
	je .add_partial			; EAX: 1 * buffer_size (second buffer)
	add eax, eax			; EAX: 2 * buffer_size (third buffer)

.add_partial:
	mov ecx, [systimer_ticks]	; Add progress since buffer flip
	mov edx, [sample_rate]
	sub ecx, [buffer_systicks]	; ECX: systimer ticks since buffer flip
	imul edx, ecx
	shr edx, 10			; EDX: samples since buffer flip
	mov cl, [shl_per_sample]	; Adjust for channel/bitdepth
	shl edx, cl
	add eax, edx			; Add to buffer start position
	mov [esi + mod_output_info.buffer_pos], eax

	; Calculate buffer format

	mov dl, [output_format]
	xor ecx, ecx			; CH: buffer format
	mov dh, dl
	and dl, FMT_BITDEPTH
	cmp dl, FMT_16BIT
	jne .channels
	or ch, MOD_BUF_16BIT

.channels:
	mov dl, dh
	and dl, FMT_CHANNELS
	cmp dl, FMT_STEREO
	jne .range
	or ch, MOD_BUF_2CHN

.range:
	and dh, FMT_RANGE
	cmp dh, FMT_UNSIGNED
	jne .done
	or ch, MOD_BUF_UINT

.done:
	mov [esi + mod_output_info.buffer_format], ch

	pop edx
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
; Sound Blaster playback functions.
;==============================================================================

;------------------------------------------------------------------------------
; Reset the Sound Blaster DSP.
;------------------------------------------------------------------------------
; -> DX - Sound Blaster I/O port
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

reset_dsp:
	push ecx
	push edx
	push eax

	; Reset Sound Blaster DSP

	add dl, 6			; DX: 2x6, DSP reset port
	mov al, 1
	out dx, al			; DSP reset start
	call wait_55ms			; Wait >=55 ms
	xor al, al
	out dx, al			; DSP reset stop

	; Wait for reset response code

	add dl, 8			; DX: 2xE, DSP read-buffer status port
	mov ecx, 10			; DSP data available timeout, ~550 ms

.check_reset_done_loop:
	in al, dx			; Check if reset response is available
	test al, 0x80
	jnz .check_dsp
	call wait_55ms
	dec ecx
	jnz .check_reset_done_loop

.check_dsp:
	sub dl, 4			; DX: 2xA, DSP read data port
	in al, dx			; Get reset response code
	cmp al, 0xaa
	jne .error			; Invalid response, device not present

	pop eax
	clc

.exit:
	pop edx
	pop ecx
	ret

.error:
	add esp, 4			; Discard EAX from stack
	mov eax, MOD_ERR_DEVICE
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Wait (at least) 55 milliseconds.
;------------------------------------------------------------------------------

wait_55ms:
	push eax

	mov eax, [0x46c]

.wait_timer_tick:
	cmp eax, [0x46c]
	je .wait_timer_tick

	pop eax
	ret


;------------------------------------------------------------------------------
; Sound Blaster IRQ handler.
;------------------------------------------------------------------------------

	align 16

irq_handler:
	push eax
	push ecx
	push edx
	push ds
	push es

	; Setup DS and ES for rendering and playroutine

	mov ax, 0x1234
	irq_player_segment EQU $ - 2
	mov ds, ax
	mov es, ax

	mov dx, [params(port)]
	cmp byte [dev_type], MOD_SB_16
	je .ack_sb16
	cmp byte [dev_type], MOD_SB_1
	jne .ack_sb2
	cmp byte [playing], 1		; Pre-SB 2.0, but playback stopped,
	jne .ack_sb2			; don't restart, just ACK IRQ

	; Start transfer of next block on Sound Blaster pre-2.0 and acknowledge
	; IRQ

	mov ecx, [params(buffer_size)]
	dec ecx
	write_dsp 0x0, 0x14		; Set buffer size and restart 8-bit
	write_dsp 0x0c, cl		; single-cycle DMA output
	write_dsp 0x0c, ch
	add dl, 0x0e - 0x0c		; Acknowledge IRQ
	in al, dx
	jmp .ack_pic_irq

	align 4

.ack_sb2:

	; Acknowledge IRQ on Sound Blaster / 2.0 / Pro

	add dl, 0x0e
	in al, dx
	jmp .ack_pic_irq

	align 4

.ack_sb16:

	; Acknowledge IRQ on Sound Blaster 16

	add dl, 0x04			; Check the source of the IRQ
	mov al, 0x82
	out dx, al
	inc dl
	in al, dx
	test al, 0x02
	jz .prev_handler		; Not 16-bit IRQ, jump to old handler
	add dl, 0x0a
	in al, dx

.ack_pic_irq:
	irq_pic_eoi byte [params(irq)]	; Destroys AL

	cmp byte [sbpro_init], 1
	je .ack_sbpro_init_irq

	; End of buffer reached, play next part of the triple buffer

	mov ah, [buffer_playprt]
	inc ah
	cmp ah, 2
	ja .reset_buffer		; Re-init to first part
	mov [buffer_playprt], ah	; Continue to 2nd/3rd part
	add ah, BUF_RENDER_1 - 1	; Target render buffer: playing part - 1
	jmp .render_buffer

	align 4

.ack_sbpro_init_irq:
	mov byte [sbpro_init], 0
	jmp .exit

	align 4

.reset_buffer:

	; Wrap back to first part of the buffer

	mov byte [buffer_playprt], 0
	mov ah, BUF_RENDER_3		; Target render buffer: 3rd part

.render_buffer:
	mov edx, [systimer_ticks]
	mov [buffer_systicks], edx
	cmp byte [buffer_status], BUF_RENDERING
	je .render_pending

	; Render into update pending buffer part

	push eax
	push ebx
	push esi
	push edi
	push ebp
	sti				; Enable interrupts (important!)
	call render			; Render audio into output buffer
	cli				; Disable interrupts
	pop ebp
	pop edi
	pop esi
	pop ebx
	pop eax

	; Update pending buffer part unless a render was already in progress

	cmp byte [buffer_status], BUF_READY
	jne .exit
	mov byte [buffer_status], ah

.exit:
	pop es
	pop ds
	pop edx
	pop ecx
	pop eax
	iret

	align 4

.render_pending:
	mov byte [buffer_pending], ah
	mov al, ah
	call update_buffer_position

	align 4				; Already aligned, but just in case...

.prev_handler:
	pop es
	pop ds
	pop edx
	pop ecx
	pop eax
	jmp 0x1234:0x12345678
	irq_prev_handler EQU $ - 6


;==============================================================================
; Data area
;==============================================================================

section .data

		; Output device API jump table

global mod_dev_sb_api
mod_dev_sb_api	istruc mod_dev_api
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

blaster_env	db 'BLASTER', 0		; Environment var. name for detection
num_channels	db 0			; Number of active channels

		%if (LOG_LEVEL >= LOG_INFO)
type_sb1	db '', 0
type_sb2	db ' 2.0', 0
type_sbpro	db ' Pro', 0
type_sb16	db ' 16', 0
types		dd type_sb1, type_sb2, type_sbpro, type_sb16
		%endif

section .bss

position_info	resb mod_position_info.strucsize * 3
channel_info	resb mod_channel_info.strucsize * MOD_MAX_CHANS * 3
params		resd (mod_dev_params.strucsize + 3) / 4
period_base	resd 1			; Period to speed conversion base
sample_rate	resd 1			; Actual playback sample rate
buffer_addr	resd 1			; Linear address of output buffer
dma_addr	resd 1			; Physical address of output buffer
buffer_size	resd 1			; Size of the output buffer
buffer_systicks	resd 1			; Systimer tick at buffer flip

play_tickr_int	resd 1			; Number of samples between player ticks
play_tickr_fr	resd 1			; Fraction part of the above
play_sam_int	resd 1			; Number of samples until next tick
play_sam_fr	resd 1			; Fraction part of the above
amplify		resw 1			; Output amplification

buffer_playprt	resb 1			; Which of the double buffers is playing
buffer_status	resb 1			; Flag to indicate need for rendering
buffer_pending	resb 1			; Pending render into buffer
dev_type	resb 1			; Output Sound Blaster card type
output_format	resb 1			; Output bitstream format
time_constant	resb 1			; Time constant value
sbpro_filter	resb 1			; Output filter state for SB Pro
sbpro_init	resb 1			; Flag for SB Pro stereo fix init stage
playing		resb 1			; Flag for playback ongoing
shl_per_sample	resb 1			; Bytes per sample
