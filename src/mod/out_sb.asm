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
; 86Box performance with Sound Blaster 16, 1x amplification, 20 msec buffer:
;
; Channels   Samplerate   Stereo mode   Interpolation   CPU
;     4        44100 Hz   Mono          Linear          386dx/16
;     4        44100 Hz   Cross         Nearest         386dx/16
;     4        44100 Hz   Real          Nearest         386dx/20
;     8        44100 Hz   Hard          Nearest         386dx/20
;     4        44100 Hz   Real          Linear          386dx/25
;     8        44100 Hz   Real          Nearest         386dx/33
;     8        44100 Hz   Hard          Linear          386dx/33
;     8        44100 Hz   Cross         Linear          386dx/40
;     8        44100 Hz   Real          Linear          486dlc/33
;    28        44100 Hz   Hard          Linear          486dx2/66
;    28        28000 Hz   Real          Linear          486dx2/66
;    28        44100 Hz   Real          Nearest         486dx2/80
;    28        42000 Hz   Real          Linear          486dx4/100
;    28        44100 Hz   Real          Linear          486dx4/120
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "system/api/pic.inc"
%include "system/api/dma.inc"
%include "system/api/env.inc"
%include "system/api/string.inc"
%include "mod/consts/global.inc"
%include "mod/consts/public.inc"
%include "mod/structs/global.inc"
%include "mod/structs/out_sb.inc"
%include "mod/consts/out.inc"
%include "mod/consts/out_sb.inc"
%include "mod/api/convert.inc"
%include "mod/api/wtbl_sw.inc"
%include "mod/api/routine.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	state(var) mod.out_state + mod_out_sb_state. %+ var
%define	params(var) mod.out_params + mod_out_params. %+ var
%define	set_out_fn(name, lbl) at mod_out_fns. %+ name, dw %+ (lbl)

segment modplayer public use16 class=CODE align=16
segment modplayer


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
; -> DS:EBX - mod_out_params structure receiving detected parameters
; <- CF - Set if Sound Blaster is not present
;    AL - Device type when CF not set (MOD_SB_*)
;    DS:EBX - Filled with Sound Blaster-specific parameters when CF not set
;------------------------------------------------------------------------------

	align 4

global mod_sb_detect
mod_sb_detect:
	push ecx
	push edx
	push esi
	push edi
	push ebp
	push ds
	push es
	push eax

	cld

	mov ax, ds
	mov es, ax			; ES:EBX: mod_out_params pointer
	mov ax, cs
	mov ds, ax
	mov ebp, ebx			; ES:EBP: mod_out_params pointer

	; Initialize detected values

	xor di, di			; DI: port
	xor dx, dx			; DL: 8-bit DMA, DH: 16-bit DMA
	xor cl, cl			; CL: IRQ
	mov ch, -1			; CH: type

	mov esi, blaster_env		; DS:ESI: "BLASTER"
	call far sys_env_get_var
	jc .error			; No environment variable, bail out

.get_params_loop:
	a32 lodsb			; Get next character

.check_param_type:
	test al, al			; End of variable value, sanity check
	jz .check_params
	cmp al, " "			; Space, check next character
	je .get_params_loop

	cmp al, "A"
	jne .check_8bit_dma

	; Axxx: I/O port address

	mov bx, 0xff20			; Terminate on space or NUL
	call far sys_str_parse_hex	; Parse value after "A"
	jc .error
	cmp eax, 0xfff0			; Can't be above FFF0
	ja .error
	mov di, ax			; Store I/O port
	jmp .next_param

.check_8bit_dma:
	cmp al, "D"
	jne .check_16bit_dma

	; Dx: 8-bit DMA channel

	mov bx, 0xff20			; Terminate on space or NUL
	call far sys_str_parse_int	; Parse value after "D"
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
	call far sys_str_parse_int	; Parse value after "H"
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
	call far sys_str_parse_int	; Parse value after "I"
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
	call far sys_str_parse_int	; Parse value after "T"
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
	a32 lodsb			; Skip to next parameter in variable
	test al, al
	jz .check_params
	cmp al, " "
	je .next_param
	jmp .check_param_type

.check_params:

	; Check if enough parameters were read from the environment variable

	test di, di			; I/O port address
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
	mov ebx, ebp			; ES:EBX: mod_out_params pointer

	; Attempt to reset the DSP of the Sound Blaster as a sanity test of
	; variables parsed from BLASTER environment variable

	mov bp, dx
	mov dx, di
	call reset_dsp
	jc .error			; Failed, Sound Blaster not present

	; Save parsed parameter values

	mov es:[ebx + mod_out_params.port], di
	mov es:[ebx + mod_out_params.irq], cl
	mov es:[ebx + mod_out_params.dma], bp

	pop eax
	mov al, ch			; Return device type in AL
	clc

.exit:
	pop es
	pop ds
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	retf

.error:
	pop eax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Set up the Sound Blaster output device.
;------------------------------------------------------------------------------
; -> AL - Output device type (MOD_SB_*)
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
	mov byte [state(output_format)], ah

	%ifdef __DEBUG__

	; Show configuration when debug is enabled

	movzx ecx, al
	movzx ecx, word cs:[types + ecx * 2]
	mov al, [params(dma)]
	cmp byte [state(dev_type)], MOD_SB_16
	jne .log_sb
	mov ah, [params(dma) + 1]
	test ah, ah
	jz .log_sb
	mov al, ah

.log_sb:
	log {'Output device: Sound Blaster{s} on port {X16}h, IRQ {u8}, DMA {u8}', 13, 10}, cs, ecx, [params(port)], [params(irq)], al

	%endif

.reset_device:

	; Reset Sound Blaster DSP

	push dx
	mov dx, [params(port)]
	call reset_dsp
	pop dx
	jc .sb_error

.calc_sample_rate:

	; Calculate time constant/actual sample rate

	mov eax, edx			; SB16 can use sample rate directly
	cmp byte [state(dev_type)], MOD_SB_16
	je .use_sample_rate

	; Time constant only for SB/SB Pro

	xor cl, cl
	cmp byte [state(dev_type)], MOD_SB_PRO
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
	mov [state(time_constant)], dh	; Save time constant
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

	log {'Sound Blaster initialized successfully', 13, 10}

	; Allocate memory for the output buffer

	mov eax, [state(sample_rate)]	; Convert msec to buffer size
	movzx ebx, word [params(buffer_size)]
	cmp ebx, 1000
	jae .check_buffer_size
	mul ebx
	mov ebx, 1000
	div ebx
	cmp edx, 500			; Rounding
	setae dl
	movzx edx, dl
	add eax, edx

.check_buffer_size:
	cmp eax, 5456			; Maximum safe buffer size
	jbe .use_buffer_size
	mov eax, 5456

.use_buffer_size:
	mov [params(buffer_size)], ax

	mov ebx, eax
	test byte [state(output_format)], FMT_16BIT
	setnz cl			; CL: 1 when 16 bit, else 0
	test byte [state(output_format)], FMT_STEREO
	setnz ch			; CH: 1 when stereo, else 0
	add cl, ch
	shl ebx, cl			; Calculate buffer size in bytes
	mov [state(buffer_size)], ebx
	lea ebx, [ebx + ebx * 2]	; Triple buffering
	mov al, SYS_MEM_DMA
	call far sys_mem_alloc
	jc .error
	mov [state(buffer_addr)], eax

	log {'Allocated {u} bytes for output device DMA buffer @{X}', 13, 10}, ebx, eax

	; Setup wavetable

	mov al, [params(interpolation)]
	mov ah, [params(stereo_mode)]
	mov bx, [state(amplify)]
	mov cx, [params(buffer_size)]
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

.sb_error:
	mov eax, MOD_ERR_DEVICE
	jmp .error

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

	log {'Disposing output device DMA buffer @{X}', 13, 10}, eax

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
; Start playback on the Sound Blaster device.
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

	; Pre-render into output buffer before starting playback

	mov byte [state(buffer_status)], BUF_RENDER_1
	call render
	mov byte [state(buffer_status)], BUF_RENDER_2
	call render
	mov byte [state(buffer_status)], BUF_RENDER_3
	call render

	cli

	; Setup and install IRQ handler

	mov cs:[irq_player_segment], ds
	mov cl, [params(irq)]
	call far sys_pic_irq_to_int
	call far sys_get_int_handler
	mov cs:[irq_prev_handler + 2], es
	mov cs:[irq_prev_handler], bx
	mov ax, cs
	mov es, ax
	mov bx, irq_handler
	call far sys_set_int_handler

	; Enable IRQ

	xor ax, ax
	mov es, ax			; ES: zeropage
	movzx cx, byte [params(irq)]
	call far sys_pic_enable_irq

	sti

	; Setup Sound Blaster for audio playback

	mov dx, [params(port)]
	mov bx, [params(buffer_size)]
	test byte [state(output_format)], FMT_STEREO
	setnz cl			; CL: 1 when stereo output, else 0
	shl bx, cl			; Twice the count when stereo
	dec bx
	mov byte [state(sbpro_init)], 0

	cmp byte [state(dev_type)], MOD_SB_16
	je .start_sb16
	cmp byte [state(dev_type)], MOD_SB_PRO
	je .start_sbpro
	cmp byte [state(dev_type)], MOD_SB_2
	je .start_sb2

	; Initialize Sound Blaster playback

	call .setup_autoinit_dma	; Setup DMA controller (with autoinit)

	write_dsp 0, 0xd1		; Turn on DAC speaker
	write_dsp 0x0c, 0x40		; Set time constant
	write_dsp 0x0c, [state(time_constant)]
	dec bx
	write_dsp 0x0c, 0x14		; Set buffer size and start 8-bit
	write_dsp 0x0c, bl		; single-cycle DMA output
	write_dsp 0x0c, bh

	jmp .exit

.start_sb2:

	; Initialize Sound Blaster 2.0 playback

	call .setup_autoinit_dma	; Setup DMA controller

	write_dsp 0, 0xd1		; Turn on DAC speaker
	write_dsp 0x0c, 0x40		; Set time constant
	write_dsp 0x0c, [state(time_constant)]
	write_dsp 0x0c, 0x48		; Set buffer size
	write_dsp 0x0c, bl
	write_dsp 0x0c, bh
	write_dsp 0x0c, 0x90		; Start auto-init high speed DMA

	jmp .exit

.start_sbpro:

	; Initialize Sound Blaster Pro playback

	write_dsp 0, 0xd1		; Turn on DAC speaker

	test byte [state(output_format)], FMT_STEREO
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

	push bx				; Preserve original buffer length
	mov byte [state(sbpro_init)], 1
	mov dl, [params(dma)]
	call far sys_dma_disable_channel
	xor ebx, ebx
	mov bx, cs
	shl ebx, 4
	add ebx, silence_sample
	xor ecx, ecx
	mov dh, SYS_DMA_READ | SYS_DMA_SINGLE
	call far sys_dma_setup_channel
	call far sys_dma_enable_channel
	pop bx

	mov dx, [params(port)]
	write_dsp 0, 0x14
	write_dsp 0x0c, 0
	write_dsp 0x0c, 0
	mov ecx, es:[0x46c]		; Save timer tick count

.wait_sbpro_init:
	mov eax, es:[0x46c]		; Wait for up to 2 ticks (110 ms)
	sub eax, ecx
	cmp eax, 2
	ja .sbpro_mono
	cmp byte [state(sbpro_init)], 1
	je .wait_sbpro_init

.sbpro_mono:
	mov dx, [params(port)]

	; Start output

	call .setup_autoinit_dma	; Setup DMA controller

	write_dsp 0, 0x40		; Set time constant
	write_dsp 0x0c, [state(time_constant)]

	sub dl, 0x08			; DX: 2x4: mixer port
	mov al, 0x0e
	out dx, al
	inc dx
	in al, dx
	mov [state(sbpro_filter)], al
	or al, 0x20
	out dx, al			; Turn off output filter

	write_dsp 0x05, 0x48		; Set buffer size
	write_dsp 0x0c, bl
	write_dsp 0x0c, bh
	write_dsp 0x0c, 0x90		; Start auto-init high speed DMA

	jmp .exit

.start_sb16:
	mov ecx, [state(sample_rate)]

	; Initialize Sound Blaster 16 playback

	call .setup_autoinit_dma	; Setup DMA controller

	mov ah, 0x20
	test byte [state(output_format)], FMT_STEREO
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
; Setup DMA controller for auto-init audio data transfer.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;------------------------------------------------------------------------------

	align 4

.setup_autoinit_dma:
	push ebx
	push ecx
	push dx

	mov dl, [params(dma)]
	cmp byte [state(dev_type)], MOD_SB_16
	jne .setup_dma
	mov dh, [params(dma) + 1]	; Use high DMA for 16-bit output if
	test dh, dh			; specified
	jz .setup_dma
	mov dl, dh

.setup_dma:
	call far sys_dma_disable_channel
	mov ebx, [state(buffer_addr)]
	mov ecx, [state(buffer_size)]
	lea ecx, [ecx + ecx * 2]
	mov dh, SYS_DMA_READ | SYS_DMA_AUTO | SYS_DMA_SINGLE
	call far sys_dma_setup_channel
	call far sys_dma_enable_channel

	pop dx
	pop ecx
	pop ebx
	retn


;------------------------------------------------------------------------------
; Stop playback on the Sound Blaster device.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- CF - Cleared
;------------------------------------------------------------------------------

	align 4

stop:
	push eax
	push bx
	push cx
	push es

	; Halt DMA operation, then terminate auto-initialize DMA

	mov dx, [params(port)]

	cmp byte [state(dev_type)], MOD_SB_PRO
	je .stop_sbpro
	cmp byte [state(dev_type)], MOD_SB_16
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
	mov al, [state(sbpro_filter)]
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
	cli

	; Uninstall IRQ handler

	mov cl, [params(irq)]
	call far sys_pic_irq_to_int
	mov es, cs:[irq_prev_handler + 2]
	mov bx, cs:[irq_prev_handler]
	call far sys_set_int_handler

	sti

	pop es
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
	call mod_swt_render
	pop dx
	pop ecx
	pop bx

	; Calculate number of samples left to render

	sub bx, cx
	sub dx, cx
	jnz .loop_render

	; Output buffer completely rendered

	mov [state(play_sam_int)], bx	; Update samples until playroutine tick

.noop:

	; Done rendering or nothing to do (no part of the buffer needs new audio
	; data)

	mov byte [state(buffer_status)], BUF_READY

.exit:
	retn


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

	align 4

reset_dsp:
	push cx
	push dx
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
	mov cx, 10			; DSP data available timeout, ~550 ms

.check_reset_done_loop:
	in al, dx			; Check if reset response is available
	test al, 0x80
	jnz .check_dsp
	call wait_55ms
	dec cx
	jnz .check_reset_done_loop

.check_dsp:
	sub dl, 4			; DX: 2xA, DSP read data port
	in al, dx			; Get reset response code
	cmp al, 0xaa
	jne .error			; Invalid response, device not present

	pop eax
	clc

.exit:
	pop dx
	pop cx
	retn

.error:
	add sp, 4			; Discard EAX from stack
	mov eax, MOD_ERR_DEVICE
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Wait (at least) 55 milliseconds.
;------------------------------------------------------------------------------

	align 4

wait_55ms:
	push ax
	push es

	xor ax, ax
	mov es, ax
	mov ax, es:[0x46c]

.wait_tick:
	cmp ax, es:[0x46c]
	je .wait_tick

	pop es
	pop ax
	retn


;------------------------------------------------------------------------------
; Sound Blaster IRQ handler.
;------------------------------------------------------------------------------

	align 4

irq_handler:
	push eax
	push ecx
	push edx
	push ds

	; DS: player instance segment

	mov ax, 0x1234
	irq_player_segment EQU $ - 2
	mov ds, ax

	; Distinguish between hardware IRQ and others (exceptions, software
	; interrupts): call old handler if not a hardware IRQ
	; sys_pic_irq_serviced destroys AL, CL and DX!

	sys_pic_irq_serviced [params(irq)]
	jz .prev_handler

	mov dx, [params(port)]
	cmp byte [state(dev_type)], MOD_SB_16
	je .ack_sb16
	cmp byte [state(dev_type)], MOD_SB_1
	jne .ack_sb2

	; Start transfer of next block on Sound Blaster pre-2.0 and acknowledge
	; IRQ

	mov cx, [params(buffer_size)]
	dec cx
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
	inc dx
	in al, dx
	test al, 0x02
	jz .prev_handler		; Not 16-bit IRQ, jump to old handler
	add dl, 0x0a
	in al, dx

.ack_pic_irq:
	sys_pic_eoi byte [params(irq)]	; Destroys AL

	cmp byte [state(sbpro_init)], 1
	je .ack_sbpro_init_irq

	; End of buffer reached, play next part of the triple buffer

	mov ah, [state(buffer_playprt)]
	inc ah
	cmp ah, 2
	ja .reset_buffer		; Re-init to first part
	mov [state(buffer_playprt)], ah	; Continue to 2nd/3rd part
	add ah, BUF_RENDER_1 - 1	; Target render buffer: playing part - 1
	jmp .render_buffer

	align 4

.ack_sbpro_init_irq:
	mov byte [state(sbpro_init)], 0
	jmp .exit

	align 4

.reset_buffer:

	; Wrap back to first part of the buffer

	mov byte [state(buffer_playprt)], 0
	mov ah, BUF_RENDER_3		; Target render buffer: 3rd part

.render_buffer:
	cmp byte [state(buffer_status)], BUF_RENDERING
	je .exit

	; Render into update pending buffer part

	push ax
	push ebx
	push esi
	push edi
	push ebp
	sti				; Enable interrupts (important!)
	call render			; Render audio into output buffer
	cli				; Disable interrupts
	pop ebp
	pop esi
	pop edi
	pop ebx
	pop ax

	; Update pending buffer part unless a render was already in progress

	cmp byte [state(buffer_status)], BUF_READY
	jne .exit
	mov byte [state(buffer_status)], ah

.exit:
	pop ds
	pop edx
	pop ecx
	pop eax
	iret

	align 4

.prev_handler:
	pop ds
	pop edx
	pop ecx
	pop eax
	jmp 0x1234:0x1234
	irq_prev_handler EQU $ - 4


;==============================================================================
; Data area
;==============================================================================

		; Output device function pointers

		alignb 4

global mod_out_sb_fns
mod_out_sb_fns	istruc mod_out_fns
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

silence_sample	db 0x80
blaster_env	db 'BLASTER', 0

		%ifdef __DEBUG__

type_sb1	db '', 0
type_sb2	db ' 2.0', 0
type_sbpro	db ' Pro', 0
type_sb16	db ' 16', 0
types		dw type_sb1, type_sb2, type_sbpro, type_sb16

		%endif
