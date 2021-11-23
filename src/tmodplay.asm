;==============================================================================
; Therapy MOD player - quarter century later edition
; A tribute to my teenage self
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "system/api/file.inc"
%include "system/api/env.inc"
%include "system/api/string.inc"
%include "mod/api/player.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	set_file_fn(name, lbl) at mod_file_fns. %+ name, dw %+ (lbl), seg %+ (lbl)

segment app private use16 class=CODE align=16
segment app


;------------------------------------------------------------------------------
; Program entry point
;------------------------------------------------------------------------------

..start:

	; Initialize system

	call far sys_mem_setup		; Memory management - first thing to do
	jc .sys_error
	call far sys_env_setup		; Environment
	call far sys_file_setup		; File management
	jc .sys_error

	; Initialize segment registers

	mov ax, app_data
	mov ds, ax
	mov es, ax

	; Initialize logging

	%ifdef __DEBUG__
	mov eax, LOG_FILE | LOG_AUTOCOMMIT
	mov esi, logfile
	call far log_setup
	%endif

	call far sys_file_get_buf_addr	; Save I/O buffer address rel. to DS
	movzx eax, ax
	shl eax, 4
	sub ebx, eax
	mov [io_buf_addr], ebx

	mov esi, header			; Show application header
	call echo

	; Parse arguments for player

	mov esi, arg_help		; Display usage if /? argument present
	call far sys_env_get_named_arg
	jnc .usage
	mov ebx, out_params		; Parse arguments
	call parse_args
	jc .exit

	; Initialize MOD player

	log {'Initializing MOD player, requested sample rate: {u} Hz', 13, 10}, edx

	mov edi, file_fns
	call far mod_setup
	jc .mod_error

	log {'Player initialized, actual sample rate: {u} Hz', 13, 10}, eax

	; Load the file

	call far mod_load
	jc .mod_error

	log {'Module loaded successfully', 13, 10}

	call far mod_get_flags
	log {'Flags: {X}', 13, 10}, eax

	; Start playback

	call far mod_play
	xor ah, ah
	int 16h
	call far mod_stop
	call far mod_unload
	call far mod_shutdown

	jmp .exit

.exit:
	%ifdef __DEBUG__
	call far log_shutdown
	%endif

	call far sys_file_shutdown
	call far sys_mem_shutdown

	mov ax, 0x4c00
	int 0x21

.usage:
	mov esi, usage			; Display usage
	call echo
	jmp .exit

.sys_error:
	mov esi, errtab_sys		; Display system error messages
	call echo_error
	jmp .exit

.mod_error:
	mov esi, errtab_mod		; Display modplayer error messages
	call echo_error
	jmp .exit


;------------------------------------------------------------------------------
; Write text to standard output.
;------------------------------------------------------------------------------
; -> DS:ESI - Pointer to ASCIIZ string
;------------------------------------------------------------------------------

echo:
	push eax
	push ebx
	push ecx
	push esi

	call far sys_str_len		; ECX: length of string
	xor ebx, ebx
	mov bx, ds
	shl ebx, 4
	add esi, ebx			; ESI: linear address of DS:ESI
	mov ebx, 1			; Write to file handle 1 (stdout)
	call far sys_file_write

	pop esi
	pop ecx
	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Print formatted string to stdout.
;------------------------------------------------------------------------------
; -> DS:ESI - Source string to format
;    SS:BP - Pointer just above first variable value
;    DS - app_data segment
;------------------------------------------------------------------------------
; See sys_str_format for usage details.
;------------------------------------------------------------------------------

printf:
	push esi
	push edi

	mov edi, [io_buf_addr]
	call far sys_str_format
	mov esi, edi
	call echo

	pop edi
	pop esi
	retn


;------------------------------------------------------------------------------
; Show error message for an error code.
;------------------------------------------------------------------------------
; -> EAX - Error code
;    DS:ESI - Error message table
;------------------------------------------------------------------------------

echo_error:
	push eax
	push ebx
	push esi

	cld

	mov ebx, eax

.find_error_loop:
	a32 lodsd
	test eax, eax
	jz .echo
	cmp eax, ebx
	je .echo
	add esi, 4
	jmp .find_error_loop

.echo:
	mov esi, [esi]
	call echo

	pop esi
	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Parse command line arguments.
;------------------------------------------------------------------------------
; -> DS:EBX - Pointer to mod_out_params structure to fill with parsed values
; <- CF - Set if a required parameter is missing
;    AH - Output device or 0 if not specified
;    AL - Output device type or 0 if not specified
;    CX - Amplification or 0 if not specified
;    EDX - Requested sample rate or 0 if not specified
;    DS:ESI - Pointer to ASCIIZ MOD filename
;------------------------------------------------------------------------------

parse_args:
	push ebx
	push edi
	push ebp
	push es

	push eax
	push ecx
	push edx
	push esi

	xor ax, ax
	mov es, ax			; ES: zeropage

	; Set default output device parameters

	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_REAL
	mov byte [ebx + mod_out_params.initial_pan], 0x60
	mov word [ebx + mod_out_params.buffer_size], 20

	; /o:device - output device type

	mov esi, arg_out
	call far sys_env_get_named_arg
	jnc .check_out_device

	; No output device type, detect

	mov ah, MOD_OUT_SB		; Detect Sound Blaster
	call far mod_sb_detect
	jnc .check_port

	mov ah, MOD_OUT_DAC		; Nothing found, fallback to speaker
	mov al, MOD_DAC_SPEAKER
	jmp .check_port

.check_out_device:

	; /o:device - Parse output device type

	mov ah, MOD_OUT_SB		; /o:sb
	mov edi, arg_out_sb
	mov ecx, -1
	call far sys_str_cmp
	jc .check_out_sb16
	call far mod_sb_detect		; Detect SB type and parameters
	jc .sb_error			; No SB found, bail out
	jmp .check_port

.sb_error:
	mov esi, err_out_sb		; Can't find SB
	call echo
	jmp .error

.check_out_sb16:
	mov al, MOD_SB_16		; /o:sb16
	mov edi, arg_out_sb16
	mov ecx, -1
	call far sys_str_cmp
	jnc .check_sb_port
	mov al, MOD_SB_PRO		; /o:sbpro
	mov edi, arg_out_sbpro
	call far sys_str_cmp
	jnc .check_sb_port
	mov al, MOD_SB_2		; /o:sb2
	mov edi, arg_out_sb2
	call far sys_str_cmp
	jnc .check_sb_port

	mov ah, MOD_OUT_DAC		; /o:lpt
	mov al, MOD_DAC_LPT
	mov edi, arg_out_lpt
	call far sys_str_cmp
	jnc .check_lpt_port
	mov al, MOD_DAC_LPTST		; /o:lptst
	mov edi, arg_out_lptst
	call far sys_str_cmp
	jnc .check_lpt_port
	mov al, MOD_DAC_SPEAKER		; /o:speaker
	mov edi, arg_out_speaker
	call far sys_str_cmp
	jnc .check_lpt_port

	mov bp, sp			; Invalid output device
	push esi
	push dword arg_out
	mov esi, err_arg_out
	call printf
	mov sp, bp
	jmp .error

.check_sb_port:

	; Set Sound Blaster default parameters

	mov word [ebx + mod_out_params.port], 0x220
	mov byte [ebx + mod_out_params.irq], 5
	mov byte [ebx + mod_out_params.dma], 1
	mov byte [ebx + mod_out_params.dma + 1], 5
	push ax
	call far mod_sb_detect		; Detect SB parameters
	pop ax
	jmp .check_port

.check_lpt_port:

	; Set LPT DAC default parameters

	mov cx, es:[0x408]		; LPT1 base address
	mov word [ebx + mod_out_params.port], cx

.check_port:
	mov bp, ax			; BP: output device / output device type

	; /p:port[,port2] - output device I/O port base address(es)

	mov esi, arg_port
	call far sys_env_get_named_arg
	jc .check_irq
	mov ah, ','
	mov ecx, -1
	call far sys_str_char_pos	; More, than one I/O port provided?
	jnc .multi_port

	; /p:port - single I/O port base address

	mov ecx, -1
	call .get_port_address
	jc .port_error
	mov [ebx + mod_out_params.port], dx
	jmp .check_irq

.port_error:
	mov bp, sp			; Invalid I/O port
	push esi
	push dword arg_port
	mov esi, err_arg_port
	call printf
	mov sp, bp
	jmp .error

.multi_port:

	; /p:port,port2 - two I/O port base addresses

	mov ecx, eax			; Get first up to separator (,)
	call .get_port_address
	jc .port_error
	mov [ebx + mod_out_params.port], dx

	add esi, ecx			; Get second up to end of argument
	inc esi
	mov ecx, -1
	call .get_port_address
	jc .port_error
	mov [ebx + mod_out_params.port + 2], dx

	; Change output device type to dual LPT DAC

	cmp bp, MOD_OUT_DAC * 256 + MOD_DAC_LPT
	jne .check_irq
	mov bp, MOD_OUT_DAC * 256 + MOD_DAC_LPTDUAL

.check_irq:

	; /i:irq - output device IRQ number

	mov esi, arg_irq
	call far sys_env_get_named_arg
	jc .check_dma
	push bx
	mov bx, 0xff00
	call far sys_str_parse_int	; EAX: IRQ number
	pop bx
	jc .irq_error
	cmp eax, 15			; IRQ number sanity check
	ja .irq_error
	mov [ebx + mod_out_params.irq], al
	jmp .check_dma

.irq_error:
	mov bp, sp			; Invalid IRQ number
	push esi
	push dword arg_irq
	mov esi, err_arg_irq
	call printf
	mov sp, bp
	jmp .error

.check_dma:

	; /d:dma[,dma16] - output device DMA channel(s)

	mov esi, arg_dma
	call far sys_env_get_named_arg
	jc .check_stereo
	mov ah, ','
	mov ecx, -1
	call far sys_str_char_pos	; More, than one I/O port provided?
	jnc .multi_dma

	; /d:dma - single DMA channel

	mov ecx, -1
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_out_params.dma], dl
	mov [ebx + mod_out_params.dma + 1], dl
	jmp .check_stereo

.dma_error:
	mov bp, sp			; Invalid DMA channel
	push esi
	push dword arg_dma
	mov esi, err_arg_dma
	call printf
	mov sp, bp
	jmp .error

.multi_dma:

	; /d:dma,dma16 - two DMA channels

	mov ecx, eax			; Get first up to separator (,)
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_out_params.dma], dl

	add esi, ecx			; Get second up to end of argument
	inc esi
	mov ecx, -1
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_out_params.dma + 1], dl

.check_stereo:

	; /s:mode[,initialpan%]

	mov esi, arg_stereo
	call far sys_env_get_named_arg
	jc .check_samplerate

	mov edi, arg_stereo_mono	; /s:mono
	mov ecx, -1
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_MONO
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_hard	; /s:hard
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_HARD
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_x		; /s:cross
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_CROSS
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_real	; /s:real
	mov byte [ebx + mod_out_params.stereo_mode], MOD_PAN_REAL
	call far sys_str_cmp
	jnc .check_samplerate
	mov edi, arg_stereo_rpan	; /s:real,initialpan%
	mov ecx, 5
	call far sys_str_cmp
	jc .stereo_error
	add esi, 5
	push bx
	mov bx, 0xff00
	call far sys_str_parse_int	; EAX: initial pan %
	pop bx
	jc .realpan_error
	cmp eax, 100
	ja .realpan_error
	mov ecx, 128			; Convert pan percentage to initial pan
	mul ecx				; EAX: initial pan % * 128
	mov ecx, 100
	div ecx				; EAX: floor(initial pan % * 128 / 100)
	cmp edx, 50			; Rounding
	setae dl			; DL: 1 if remainder >= 50, else 0
	movzx edx, dl
	add eax, edx			; EAX: round(initial pan % * 128 / 100)
	mov ecx, 128
	sub ecx, eax			; ECX: 128 - round(128 * initial pan %)
	mov byte [ebx + mod_out_params.initial_pan], cl
	jmp .check_samplerate

.stereo_error:
	mov bp, sp			; Invalid stereo mode
	push esi
	push dword arg_stereo
	mov esi, err_arg_stereo
	call printf
	mov sp, bp
	jmp .error

.realpan_error:
	mov bp, sp			; Invalid initial pan %
	push esi
	push dword arg_stereo
	push dword arg_stereo_real
	mov esi, err_arg_realpan
	call printf
	mov sp, bp
	jmp .error

.check_samplerate:

	; /sr:samplerate

	mov edi, 44100			; EDI: default samplerate
	mov esi, arg_samplerate
	call far sys_env_get_named_arg
	jc .check_amp
	push bx
	mov bx, 0xff00
	call far sys_str_parse_int	; EAX: requested sample rate
	pop bx
	jc .samplerate_error
	cmp eax, 8000			; Minimum samplerate: 8000 Hz
	jl .samplerate_error
	mov edi, eax			; EDI: requested samplerate
	jmp .check_amp

.samplerate_error:
	mov bp, sp			; Invalid samplerate
	push esi
	push dword arg_samplerate
	mov esi, err_arg_smprate
	call printf
	mov sp, bp
	jmp .error

.check_amp:
	mov ax, 0x0100
	mov esi, arg_amp
	call far sys_env_get_named_arg
	jc .get_filename
	push bx
	mov bx, 0xff00
	mov ecx, 0x100
	call far sys_str_parse_fixed	; EAX: amplification
	pop bx
	jc .amp_error
	cmp eax, 0x400			; Maximum amplification: 4x
	ja .amp_error
	jmp .get_filename

.amp_error:
	mov bp, sp			; Invalid amplification
	push esi
	push dword arg_amp
	mov esi, err_arg_amp
	call printf
	mov sp, bp
	jmp .error

.get_filename:

	; Get filename

	xor cl, cl
	call far sys_env_get_arg	; DS:ESI: filename (first argument)
	jc .filename_error
	cmp byte [esi], '/'
	je .filename_error

	; Set output values

	add sp, 8			; Discard ESI and EDX from stack
	mov edx, edi			; EDX: requested samplerate
	pop ecx				; ECX: restore high word
	mov cx, ax			; CX: amplification
	pop eax				; EAX: restore high word
	mov ax, bp			; AH/AL: output device

	clc
	jmp .exit

.filename_error:
	mov esi, err_arg_fname		; Missing filename
	call echo
	jmp .error

.exit:
	pop es
	pop ebp
	pop edi
	pop ebx
	retn

.error:
	mov esi, err_args
	call echo

	pop esi
	pop edx
	pop ecx
	pop eax
	stc
	jmp .exit


;------------------------------------------------------------------------------
; Get port address from current argument value.
;------------------------------------------------------------------------------
; -> ECX - Maximum number of argument characters to parse
;    DS:ESI - Pointer to argument value
;    ES - Zeropage
; <- CF - Set if error
;    DX - Parsed port address when no error
;------------------------------------------------------------------------------

.get_port_address:
	push edi
	push dx

	mov dx, es:[0x408]		; DX: LPT1 port address
	mov edi, arg_port_lpt1		; /p:lpt1
	call far sys_str_cmp
	jnc .check_port_address
	mov dx, es:[0x40a]		; DX: LPT2 port address
	mov edi, arg_port_lpt2		; /p:lpt2
	call far sys_str_cmp
	jnc .check_port_address
	mov dx, es:[0x40c]		; DX: LPT3 port address
	mov edi, arg_port_lpt3		; /p:lpt3
	call far sys_str_cmp
	jnc .check_port_address
	mov dx, es:[0x40e]		; DX: LPT4 port address
	mov edi, arg_port_lpt4		; /p:lpt4
	call far sys_str_cmp
	jnc .check_port_address

	push eax
	push bx				; /p:XXXX
	mov bh, cl			; BH: number of characters to convert
	xor bl, bl			; BL: no terminator character
	call far sys_str_parse_hex	; EAX: port address
	mov edi, eax
	pop bx
	pop eax
	jc .error_port_address		; Invalid hexadecimal value
	test edi, 0xffff0000		; Invalid port address
	jnz .error_port_address
	mov dx, di			; DX: port address

.check_port_address:
	test dx, dx			; Port address sanity check
	jz .error_port_address
	add sp, 2			; Discard DX from stack
	clc

.exit_port_address:
	pop edi
	retn

.error_port_address:
	stc
	pop dx
	jmp .exit_port_address


;------------------------------------------------------------------------------
; Get DMA channel from current argument value.
;------------------------------------------------------------------------------
; -> ECX - Maximum number of argument characters to parse
;    DS:ESI - Pointer to argument value
; <- CF - Set if error
;    DL - Parsed DMA channel when no error
;------------------------------------------------------------------------------

.get_dma_channel:
	push eax
	push bx
	push ecx

	mov bh, cl			; BH: number of characters to convert
	xor bl, bl			; BL: no terminator character
	call far sys_str_parse_int	; EAX: DMA channel
	jc .error_dma_channel		; Invalid hexadecimal value
	cmp eax, 7			; Invalid DMA channel
	ja .error_dma_channel
	mov dl, al			; DL: DMA channel
	clc

.exit_dma_channel:
	pop ecx
	pop bx
	pop eax
	retn

.error_dma_channel:
	stc
	jmp .exit_dma_channel


;==============================================================================
; Data area
;==============================================================================

segment app_data public class=DATA align=16
segment app_data

io_buf_addr	dd 0			; Address of the file I/O buffer

header		db 'Therapy MOD player - quarter century later edition', 13, 10, 13, 10, 0

		; Command line usage

usage		db 'Usage: tmodplay <filename.mod> [options]',13, 10, 13, 10
		db 'Options: (case-sensitive)', 13, 10, 13, 10
		db '/o:device           Select output device. Available options for "device" are:', 13, 10
		db '                    - speaker: Internal PC speaker', 13, 10
		db '                    - lpt: One or two parallel port D/A converters', 13, 10
		db '                    - lptst: Stereo parallel port D/A converter (stereo-on-1)', 13, 10
		db '                    - sb: Sound Blaster (detect type from BLASTER env. var.)', 13, 10
		db '                    - sb2: Sound Blaster 2.0', 13, 10
		db '                    - sbpro: Sound Blaster Pro', 13, 10
		db '                    - sb16: Sound Blaster 16', 13, 10, 13, 10
		db '/p:port[,port2]     Output device base port in hexadecimal or as "lptX", where', 13, 10
		db '                    X is the LPT port number. To enable playback on two', 13, 10
		db '                    parallel port DACs, specify both printer ports in this', 13, 10
		db '                    option (for example: /p:lpt1,lpt2).', 13, 10, 13, 10
		db '/i:irq              IRQ number for the output device.', 13, 10, 13, 10
		db '/d:dma[,dma16]      DMA channel for the output device. Provide 16-bit DMA', 13, 10
		db '                    channel for SB 16. Alternatively, provide both 8-bit and', 13, 10
		db '                    16-bit DMA channels separated with a comma.', 13, 10, 13, 10
		db '/sr:samplerate      Sampling frequency in Hz.', 13, 10, 13, 10
		db '/s:mode[,panpct]    Output stereo mode for stereo devices. Accepted values for', 13, 10
		db '                    "mode":', 13, 10
		db '                    - mono: Force mono output (fast)', 13, 10
		db '                    - hard: Hard left/right panning as on Amiga (still fast)', 13, 10
		db '                    - cross: Set 25% left/right from center (slower)', 13, 10
		db '                    - real: Real panning via 8xx and E8x commands (slowest)', 13, 10, 13, 10
		db '                    "panpct" specifies the initial left/right panning from', 13, 10
		db '                    center for "real" mode.', 13, 10, 13, 10
		db '/amp:amplification  Output amplification between 0 - 4. Value is decimal.', 13, 10, 13, 10
		db '/?                  Display this command line usage information.', 13, 10
		db 0

		; Command line named arguments

arg_out		db '/o', 0
arg_out_sb	db 'sb', 0
arg_out_sb16	db 'sb16', 0
arg_out_sbpro	db 'sbpro', 0
arg_out_sb2	db 'sb2', 0
arg_out_lpt	db 'lpt', 0
arg_out_lptst	db 'lptst', 0
arg_out_speaker	db 'speaker', 0
arg_port	db '/p', 0
arg_port_lpt1	db 'lpt1', 0
arg_port_lpt2	db 'lpt2', 0
arg_port_lpt3	db 'lpt3', 0
arg_port_lpt4	db 'lpt4', 0
arg_irq		db '/i', 0
arg_dma		db '/d', 0
arg_stereo	db '/s', 0
arg_stereo_mono	db 'mono', 0
arg_stereo_hard	db 'hard', 0
arg_stereo_x	db 'cross', 0
arg_stereo_real	db 'real', 0
arg_stereo_rpan	db 'real,', 0
arg_samplerate	db '/sr', 0
arg_amp		db '/amp', 0
arg_help	db '/?', 0

		; Error messages

err_out_sb	db 'Cannot find Sound Blaster.', 13, 10
		db 'Make sure the BLASTER environment variable is set correctly.', 13, 10, 0
err_arg_out	db 'Invalid device "{s}" for option {s}.', 13, 10, 0
err_arg_port	db 'Invalid I/O port "{s}" for option {s}.', 13, 10
		db 'Make sure to not insert any spaces before and after comma when specifying', 13, 10
		db 'multiple ports for two parallel port D/A converters.', 13, 10, 0
err_arg_irq	db 'Invalid IRQ number "{s}" for option {s}.', 13, 10, 0
err_arg_dma	db 'Invalid DMA channel "{s}" for option {s}.', 13, 10
		db 'Make sure to not insert any spaces before and after comma when specifying', 13, 10
		db 'separate 8-bit and 16-bit DMA channels.', 13, 10, 0
err_arg_stereo	db 'Invalid stereo mode "{s}" for option {s}.', 13, 10, 0
err_arg_realpan	db 'Invalid initial pan % "{s}" for stereo option {s}:{s}.', 13, 10
		db 'Use a value between 0 (hard side pan) and 100 (mono).', 13, 10, 0
err_arg_smprate	db 'Invalid samplerate "{s}" for option {s}.', 13, 10
		db 'Make sure the value is not less, than 8000.', 13, 10, 0
err_arg_amp	db 'Invalid amplification "{s}" for option {s}.', 13, 10
		db 'Use a value between 0.0 (silence) and 4.0.', 13, 10, 0
err_arg_fname	db 'Please specify the name of the file to play.', 13, 10, 0
err_args	db 13, 10, 'Type tmodplay /? for help.', 13, 10, 0

err_mod_invalid	db 'Invalid MOD file format.', 13, 10, 0
err_mod_nb_chan	db 'Too many channels in the MOD file.', 13, 10, 0
err_mod_device	db 'Cannot initialize output device.', 13, 10, 0
err_sys_v86	db 'Cannot initialize system, CPU already in V86 mode. Please remove any offending'
		db 'memory managers (HIMEM.SYS can stay).', 13, 10, 0
err_dos_02	db 'File not found.', 13, 10, 0
err_dos_03	db 'Path not found.', 13, 10, 0
err_dos_04	db 'Too many open files.', 13, 10, 0
err_dos_05	db 'Access denied.', 13, 10, 0
err_dos_06	db 'Invalid handle.', 13, 10, 0
err_dos_07	db 'Memory control blocks destroyed, possible memory leak.', 13, 10, 0
err_dos_08	db 'Insufficient memory.', 13, 10, 0
err_dos_09	db 'Invalid memory block address.', 13, 10, 0
err_dos_0a	db 'Invalid environment.', 13, 10, 0
err_dos_0f	db 'Invalid drive.', 13, 10, 0
err_generic	db 'Unable to play the file.', 13, 10, 0

errtab_mod	dd MOD_ERR_INVALID, err_mod_invalid
		dd MOD_ERR_NB_CHN, err_mod_nb_chan
		dd MOD_ERR_DEVICE, err_mod_device
		dd 0x02, err_dos_02
		dd 0x03, err_dos_03
		dd 0x04, err_dos_04
		dd 0x05, err_dos_05
		dd 0x06, err_dos_06
		dd 0x07, err_dos_07
		dd 0x08, err_dos_08
		dd 0x09, err_dos_09
		dd 0x0a, err_dos_0a
		dd 0x0f, err_dos_0f
		dd 0, err_generic

errtab_sys	dd SYS_ERR_V86, err_sys_v86
		dd 0x02, err_dos_02
		dd 0x03, err_dos_03
		dd 0x04, err_dos_04
		dd 0x05, err_dos_05
		dd 0x06, err_dos_06
		dd 0x07, err_dos_07
		dd 0x08, err_dos_08
		dd 0x09, err_dos_09
		dd 0x0a, err_dos_0a
		dd 0x0f, err_dos_0f
		dd 0, err_generic

		alignb 4		; Output device parameters
out_params	db mod_out_params.strucsize dup (0)

		alignb 4		; File function pointers (all far)
file_fns	istruc mod_file_fns
		set_file_fn(open, sys_file_open)
		set_file_fn(read, sys_file_read)
		set_file_fn(close, sys_file_close)
		iend

		%ifdef __DEBUG__
logfile		db 'tmodplay.log', 0
		%endif


;==============================================================================
; Stack
;==============================================================================

segment stack stack class=STACK align=16

	resb 4096
