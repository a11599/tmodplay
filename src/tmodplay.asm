;==============================================================================
; Therapy MOD player
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/env_arg.inc"
%include "rtl/api/string.inc"
%include "rtl/api/timer.inc"
%include "rtl/api/systimer.inc"
%include "rtl/api/keyboard.inc"
%include "rtl/api/profiler.inc"
%include "rtl/api/log.inc"

%include "gui/api/gui.inc"
%include "mod/api/mod.inc"

; Flags for explicit playback parameters from command line arguments

ARG_IPOL_SET	EQU 0x01		; Interpolation method set by argument
ARG_PAN_SET	EQU 0x02		; Stereo mode set by argument

; Constants for scope rendering

SCOPE_H_SHIFTS	EQU 5			; >=3, width of scope, 2 ^ n characters
SCOPE_WIDTH	EQU (1 << SCOPE_H_SHIFTS) / 8
SCOPE_V_SHIFTS	EQU 1			; >=1, height of scope, 256 >> n pixels
SCOPE_HEIGHT	EQU 256 >> SCOPE_V_SHIFTS
SCOPE_BMP_SIZE	EQU SCOPE_HEIGHT * SCOPE_WIDTH * 64 * 2 / 8
SCOPE_PADDING	EQU 2			; Scope padding in 16 pixels
SCOPE_TOP	EQU 64			; Top Y coordinate of scope

; Constants for progress bar rendering

PB_HEIGHT	EQU 4			; Height of progress bar
PB_TOP		EQU 16			; Top Y coordinate of progress bare

; Constants for sample name rendering

SMP_COL_WIDTH	EQU 22 * 7
SMP_COL_1	EQU (SCOPE_WIDTH * 64 + SCOPE_PADDING * 8) - (256 - SMP_COL_WIDTH) / 2
SMP_COL_2	EQU (SCOPE_PADDING * 8) + (256 - SMP_COL_WIDTH) / 2
SMP_COL_HEIGHT	EQU 16 * 16

; Constants for help screen rendering

HELP_KEY_WIDTH	EQU 230			; Column width of keys

; CPU usage window in systimer ticks

CPU_WINDOW	EQU 789

; Colors

SCOPE_LINE_COL	EQU 0x01		; Color of the scope horizontal lines
SCOPE_LIMIT_COL	EQU 0x02		; Color of the scope edges
SCOPE_COL	EQU 0x04		; Color of the scope
					; Scope combos: 0x05, 0x06
PRI_TEXT_COL	EQU 0x03		; Primary text color
SEC_TEXT_COL	EQU 0x07		; Secondary text color
PB_BG_COL	EQU 0x08		; Progress bar background color
PB_FG_COL	EQU 0x09		; Progress bar progress color
INFO_TEXT_COL	EQU 0x0a		; Scope info text color
RMS_BAR_COL	EQU 0x0b		; RMS meter bar color

; UI flags

UI_UPD_SOUND	EQU 0x00000001		; Sound settings needs to be updated
UI_UPD_PAN	EQU 0x00000002		; Stereo panning needs to be updated
UI_KBD_HELP	EQU 0x00000004		; Keyboard help screen is displayed
UI_UPD_DEV	EQU 0x00000008		; Device needs to be updated


;------------------------------------------------------------------------------
; PMI application entry point
;------------------------------------------------------------------------------
; -> EBX: Linear address of MS-DOS environment variables
;    ESI: Linear address of ASCIIZ file name of the PE executable
;    EDI: Linear address of ASCIIZ arguments
;    GS: PMI public API jump table segment
;------------------------------------------------------------------------------

global _main
_main:
	push esi
	mov esi, header			; Print application header
	call echo
	pop esi
	call env_arg_setup		; Initialize env_arg module
	jc .error

	; Initialize logging

	call pmi(file_get_buf)		; Get address of I/O buffer
	jc .no_log
	mov [io_buf_addr], eax
	mov [io_buf_size], ecx
	mov edi, eax
	call str_copy			; Copy .EXE path to I/O buffer
	mov ah, '.'			; Find start of extension
	mov esi, edi			; ESI: I/O buffer
	call str_char_rpos
	jc .no_log
	mov dword [esi + eax + 1], 'LOG'

	log start, 512, LOG_FILE | LOG_AUTOCOMMIT, esi
	log LOG_DEBUG, {'Therapy MOD player started', 13, 10}

	%if (LOG_LEVEL >= LOG_INFO)

	call pmi(get_env_info)
	mov ecx, [ebx + pmi_env_info.cmb_base]
	mov eax, [ebx + pmi_env_info.cmb_size]
	mov edx, [ebx + pmi_env_info.xmb_base]
	mov ebx, [ebx + pmi_env_info.xmb_size]
	shr eax, 10
	adc eax, 0
	shr ebx, 10
	adc ebx, 0
	log LOG_INFO, {'Available memory: {u}k conventional at 0x{X}, {u}k extended at 0x{X}', 13, 10}, eax, ecx, ebx, edx

	%endif

.no_log:

	; Initialize protected mode drivers

	call systimer_start
	call profiler_start
	call timer_start
	call kbd_start

	; Initialize player

	mov esi, arg_help
	call arg_get_value
	jc .get_arguments
	mov esi, usage			; Display usage
	call echo
	jmp .exit

.get_arguments:
	mov ebx, dev_params		; Parse arguments
	call parse_args
	jc .arg_error

	mov [output_device], ax
	mov [amplification], cx
	mov [file_name], esi
	log LOG_INFO, {'Opening file "{s}"', 13, 10}, esi
	mov ebx, esi
	mov esi, msg_loading		; Display name of MOD file
	mov ebp, esp
	push ebx
	call str_format
	mov esp, ebp
	mov esi, edi
	call echo
	mov ebp, eax
	mov al, PMI_FILE_READ		; Open MOD file
	call pmi(file_open)
	jc .error
	mov [file_handle], eax
	mov eax, ebp

	push ecx
	mov esi, outtab			; Display output device info
	movzx eax, ax
	call lookup_message		; ESI: output device info string
	mov ebp, esp
	push dword [dev_params + mod_dev_params.port]
	push dword [dev_params + mod_dev_params.port + 2]
	push dword [dev_params + mod_dev_params.irq]
	push dword [dev_params + mod_dev_params.irq + 1]
	push dword [dev_params + mod_dev_params.dma]
	push dword [dev_params + mod_dev_params.dma + 1]
	mov edi, [io_buf_addr]
	mov ecx, [io_buf_size]
	call str_format
	mov esp, ebp
	mov esi, edi
	call echo
	pop ecx

	mov ebx, dev_params		; Initialize MOD player engine
	call mod_setup
	jc .error

	mov ebx, [file_handle]		; Load MOD file
	call mod_load
	jc .error
	log LOG_DEBUG, {'Closing MOD file', 13, 10}
	call pmi(file_close)		; Close MOD file
	mov dword [file_handle], 0

	; Get MOD info structure

	call mod_get_info
	jc .error
	mov [mod_info_addr], eax
	cmp byte [eax + mod_info.length], 0
	je .empty_mod
	mov esi, eax			; ESI: MOD info structure
	call alloc_channel_info

	; Set sample interpolation mode to linear for multichannel and
	; non-standard MOD files

	mov ecx, [esi + mod_info.flags]	; ECX: MOD flags
	test byte [arg_flags], ARG_IPOL_SET
	jnz .auto_stereo_mode
	cmp byte [esi + mod_info.num_channels], 4
	jne .linear_interpolation
	test ecx, MOD_FLG_PAN | MOD_FLG_EXT_OCT
	jz .auto_stereo_mode

.linear_interpolation:
	log LOG_INFO, {'MOD probably composed on PC, choosing linear interpolation', 13, 10}
	mov al, MOD_IPOL_LINEAR
	mov [dev_params + mod_dev_params.interpolation], al
	call mod_set_interpolation

.auto_stereo_mode:
	mov al, [dev_params + mod_dev_params.stereo_mode]

	; Set real stereo mode if pan effects are present

	test byte [arg_flags], ARG_PAN_SET
	jnz .set_stereo_mode
	test ecx, MOD_FLG_PAN
	jz .set_stereo_mode

	log LOG_INFO, {'MOD uses pan command, choosing real stereo mode', 13, 10}
	mov al, MOD_PAN_REAL

.set_stereo_mode:
	call mod_set_stereo_mode
	mov [dev_params + mod_dev_params.stereo_mode], al

.init_player:
	call ui_open			; Initialize GUI
	jc .error

	call mod_play			; Start playback
	jc .error
	mov eax, [systimer_ticks]
	mov [play_start_tick], eax
	mov dword [play_seconds], -1

	call ui_run

	call mod_stop			; Stop playback

.empty_mod:
	call mod_unload			; Unload MOD file

.exit:

	; Exit program

	xor al, al			; Exit with error code 0

.terminate:
	mov ebx, [file_handle]		; Close MOD file if open
	test ebx, ebx
	jz .restore_state
	log LOG_DEBUG, {'Closing MOD file', 13, 10}
	push eax
	call pmi(file_close)
	pop eax

.restore_state:

	; Discard/restore state

	call free_channel_info
	call ui_close
	call mod_shutdown
	call env_arg_close

	; Deinitialize protected mode drivers

	call kbd_stop
	call timer_stop
	call profiler_stop
	call systimer_stop

	%if (LOG_LEVEL >= LOG_DEBUG)
	log mem				; Log memory blocks
	%endif
	log LOG_DEBUG, {'Therapy MOD player stopped', 13, 10}
	log stop			; Stop logging
	call pmi(terminate)		; Return to DOS

.error:
	mov esi, errtab			; Print error message
	call lookup_message

.error_exit:
	log LOG_ERROR, {'{s}'}, esi
	call echo
	jmp .terminate_error

.arg_error:
	log LOG_ERROR, {'Invalid command line argument(s)', 13, 10}

.terminate_error:
	mov al, 127			; Exit with error code 127
	jmp .terminate


;------------------------------------------------------------------------------
; Print formatted string to stdout.
;------------------------------------------------------------------------------
; -> ESI - Source string to format
;    EBP - Pointer just above first variable value
;------------------------------------------------------------------------------
; See str_format for usage details.
;------------------------------------------------------------------------------

printf:
	push eax
	push ecx
	push esi
	push edi

	call pmi(file_get_buf)		; Get I/O buffer address
	jc .done
	mov edi, eax
	call str_format			; Replace tokens in string
	mov esi, edi
	call echo			; Print to standard output

.done:
	pop edi
	pop esi
	pop ecx
	pop eax
	ret


;------------------------------------------------------------------------------
; Write text to standard output.
;------------------------------------------------------------------------------
; -> ESI - Pointer to ASCIIZ string
;------------------------------------------------------------------------------

echo:
	push eax
	push ebx
	push ecx

	call str_len			; ECX: length of string
	mov ebx, 0x01			; Write to standard output
	call pmi(file_write)

	pop ecx
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Get message from lookup table
;------------------------------------------------------------------------------
; -> EAX - Lookup code
;    ESI - Pointer to code - message lookup table
; <- ESI - Pointer to message
;------------------------------------------------------------------------------

lookup_message:
	push eax
	push ebx

.find_message_loop:
	mov ebx, [esi]
	test ebx, ebx
	jz .found			; End of table, return fallback pointer
	cmp ebx, eax
	je .found			; Code found
	add esi, 8
	jmp .find_message_loop

.found:
	mov esi, [esi + 4]		; Return message pointer

	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Parse command line arguments.
;------------------------------------------------------------------------------
; -> EBX - Pointer to mod_dev_params structure to fill with parsed values
; <- CF - Set if a required parameter is missing
;    AH - Requested or detected output device
;    AL - Requested or detected output device type
;    EBX - Filled with device-specific values
;    CH.CL - Requested or default amplification
;    EDX - Requested or default sample rate
;    ESI - Pointer to ASCIIZ MOD filename
;------------------------------------------------------------------------------

parse_args:
	push ebx
	push edi
	push ebp

	push eax
	push ecx
	push edx
	push esi

	; Set default output device parameters

	mov byte [ebx + mod_dev_params.interpolation], MOD_IPOL_NN
	mov byte [ebx + mod_dev_params.stereo_mode], MOD_PAN_CROSS
	mov byte [ebx + mod_dev_params.initial_pan], 0x80
	mov byte [ebx + mod_dev_params.flags], MOD_FLG_FMT_CHG | MOD_FLG_SR_CHG
	mov dword [ebx + mod_dev_params.buffer_size], 17000

	;----------------------------------------------------------------------
	; /ipol:mode - Sample interpolation mode

	mov esi, arg_ipol
	call arg_get_value
	jc .check_device

	or byte [arg_flags], ARG_IPOL_SET
	mov edi, arg_ipol_watte		; /ipol:watte
	mov ecx, -1
	mov byte [ebx + mod_dev_params.interpolation], MOD_IPOL_WATTE
	call str_cmp
	je .check_device
	mov edi, arg_ipol_linear	; /ipol:linear
	mov byte [ebx + mod_dev_params.interpolation], MOD_IPOL_LINEAR
	call str_cmp
	je .check_device
	mov edi, arg_ipol_nn		; /ipol:nearest
	mov byte [ebx + mod_dev_params.interpolation], MOD_IPOL_NN
	call str_cmp
	je .check_device

	mov ebp, esp			; Invalid sample interpolation mode
	push esi
	push dword arg_ipol
	mov esi, err_arg_ipol
	call printf
	mov esp, ebp
	jmp .error

.check_device:

	;----------------------------------------------------------------------
	; /o:device - Output device type

	mov esi, arg_out
	call arg_get_value
	jnc .check_out_device

	; No output device type, detect

	mov ah, MOD_OUT_SB		; Detect Sound Blaster
	call mod_sb_detect
	jnc .check_port

	mov ah, MOD_OUT_DAC		; Nothing found, fallback to speaker
	mov al, MOD_DAC_SPEAKER
	jmp .check_port

.check_out_device:

	; /o:device - Parse output device type

	mov ah, MOD_OUT_SB		; /o:sb
	mov edi, arg_out_sb
	mov ecx, -1
	call str_cmp
	jne .check_out_sb16
	call mod_sb_detect		; Detect SB type and parameters
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
	call str_cmp
	je .check_sb_port
	mov al, MOD_SB_PRO		; /o:sbpro
	mov edi, arg_out_sbpro
	call str_cmp
	je .check_sb_port
	mov al, MOD_SB_2		; /o:sb2
	mov edi, arg_out_sb2
	call str_cmp
	je .check_sb_port
	mov al, MOD_SB_1		; /o:sb1
	mov edi, arg_out_sb1
	call str_cmp
	je .check_sb_port

	mov ah, MOD_OUT_DAC		; /o:lpt
	mov al, MOD_DAC_LPT
	mov edi, arg_out_lpt
	call str_cmp
	je .check_lpt_port
	mov al, MOD_DAC_LPTST		; /o:lptst
	mov edi, arg_out_lptst
	call str_cmp
	je .check_lpt_port
	mov al, MOD_DAC_SPEAKER		; /o:speaker
	mov edi, arg_out_speaker
	call str_cmp
	je .check_port

	mov ebp, esp			; Invalid output device
	push esi
	push dword arg_out
	mov esi, err_arg_out
	call printf
	mov esp, ebp
	jmp .error

.check_sb_port:

	; Set Sound Blaster default parameters

	mov word [ebx + mod_dev_params.port], 0x220
	mov byte [ebx + mod_dev_params.irq], 5
	mov byte [ebx + mod_dev_params.dma], 1
	mov byte [ebx + mod_dev_params.dma + 1], 5
	push eax
	call mod_sb_detect		; Detect SB parameters
	pop eax
	jmp .check_port

.check_lpt_port:

	; Set LPT DAC default parameters

	mov cx, es:[0x408]		; LPT1 base address
	mov word [ebx + mod_dev_params.port], cx

.check_port:
	mov ebp, eax			; BP: output device / output device type

	;----------------------------------------------------------------------
	; /p:port[,port2] - Output device I/O port base address(es)

	mov esi, arg_port
	call arg_get_value
	jc .check_irq
	mov ah, ','
	mov ecx, -1
	call str_char_pos		; More, than one I/O port provided?
	jnc .multi_port

	; /p:port - single I/O port base address

	mov ecx, -1
	call .get_port_address
	jc .port_error
	mov [ebx + mod_dev_params.port], dx
	jmp .check_irq

.port_error:
	mov ebp, esp			; Invalid I/O port
	push esi
	push dword arg_port
	mov esi, err_arg_port
	call printf
	mov esp, ebp
	jmp .error

.multi_port:

	; /p:port,port2 - two I/O port base addresses

	mov ecx, eax			; Get first port up to separator (,)
	call .get_port_address
	jc .port_error
	mov [ebx + mod_dev_params.port], dx

	add esi, ecx			; Get second port up to end of argument
	inc esi
	mov ecx, -1
	call .get_port_address
	jc .port_error
	mov [ebx + mod_dev_params.port + 2], dx

	; Change output device type to dual LPT DAC

	cmp bp, MOD_OUT_DAC * 256 + MOD_DAC_LPT
	jne .check_irq
	mov bp, MOD_OUT_DAC * 256 + MOD_DAC_LPTDUAL

.check_irq:

	;----------------------------------------------------------------------
	; /i:irq - Output device IRQ number

	mov esi, arg_irq
	call arg_get_value
	jc .check_dma
	push ebx
	mov bx, 0xff00
	call str_parse_int		; EAX: IRQ number
	pop ebx
	jc .irq_error
	cmp eax, 15			; IRQ number sanity check
	ja .irq_error
	mov [ebx + mod_dev_params.irq], al
	jmp .check_dma

.irq_error:
	mov ebp, esp			; Invalid IRQ number
	push esi
	push dword arg_irq
	mov esi, err_arg_irq
	call printf
	mov esp, ebp
	jmp .error

.check_dma:

	;----------------------------------------------------------------------
	; /d:dma[,dma16] - Output device DMA channel(s)

	mov esi, arg_dma
	call arg_get_value
	jc .check_stereo
	mov ah, ','
	mov ecx, -1
	call str_char_pos		; More, than one DMA channel provided?
	jnc .multi_dma

	; /d:dma - single DMA channel

	mov ecx, -1
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_dev_params.dma], dl
	mov [ebx + mod_dev_params.dma + 1], dl
	jmp .check_stereo

.dma_error:
	mov ebp, esp			; Invalid DMA channel
	push esi
	push dword arg_dma
	mov esi, err_arg_dma
	call printf
	mov esp, ebp
	jmp .error

.multi_dma:

	; /d:dma,dma16 - two DMA channels

	mov ecx, eax			; Get first DMA up to separator (,)
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_dev_params.dma], dl

	add esi, ecx			; Get second DMA up to end of argument
	inc esi
	mov ecx, -1
	call .get_dma_channel
	jc .dma_error
	mov [ebx + mod_dev_params.dma + 1], dl

.check_stereo:

	;----------------------------------------------------------------------
	; /s:mode[,initialpan%] - Stereo rendering mode

	mov esi, arg_stereo
	call arg_get_value
	jc .check_samplerate

	or byte [arg_flags], ARG_PAN_SET
	mov edi, arg_stereo_mono	; /s:mono
	mov ecx, -1
	mov byte [ebx + mod_dev_params.stereo_mode], MOD_PAN_MONO
	call str_cmp
	je .check_samplerate
	mov edi, arg_stereo_hard	; /s:hard
	mov byte [ebx + mod_dev_params.stereo_mode], MOD_PAN_HARD
	call str_cmp
	je .check_samplerate
	mov edi, arg_stereo_x		; /s:cross
	mov byte [ebx + mod_dev_params.stereo_mode], MOD_PAN_CROSS
	call str_cmp
	je .check_samplerate
	mov edi, arg_stereo_real	; /s:real
	mov byte [ebx + mod_dev_params.stereo_mode], MOD_PAN_REAL
	call str_cmp
	je .check_samplerate
	mov edi, arg_stereo_rpan	; /s:real,initialpan%
	mov ecx, 5
	call str_cmp
	jne .stereo_error
	add esi, 5
	push ebx
	mov bx, 0xff00
	call str_parse_int		; EAX: initial pan %
	pop ebx
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
	mov byte [ebx + mod_dev_params.initial_pan], cl
	jmp .check_samplerate

.stereo_error:
	mov ebp, esp			; Invalid stereo mode
	push esi
	push dword arg_stereo
	mov esi, err_arg_stereo
	call printf
	mov esp, ebp
	jmp .error

.realpan_error:
	mov ebp, esp			; Invalid initial pan %
	push esi
	push dword arg_stereo
	push dword arg_stereo_real
	mov esi, err_arg_realpan
	call printf
	mov esp, ebp
	jmp .error

.check_samplerate:

	;----------------------------------------------------------------------
	; /sr:samplerate - Output samplerate

	mov edi, 44100			; EDI: default samplerate
	mov esi, arg_samplerate
	call arg_get_value
	jc .check_amp
	push ebx
	mov bx, 0xff00
	call str_parse_int		; EAX: requested sample rate
	pop ebx
	jc .samplerate_error
	cmp eax, 8000			; Minimum samplerate: 8000 Hz
	jl .samplerate_error
	mov edi, eax			; EDI: requested samplerate
	jmp .check_amp

.samplerate_error:
	mov ebp, esp			; Invalid samplerate
	push esi
	push dword arg_samplerate
	mov esi, err_arg_smprate
	call printf
	mov esp, ebp
	jmp .error

.check_amp:

	;----------------------------------------------------------------------
	; /amp:amplification - Output amplification

	mov ax, 0x0100			; AX: default amplification
	mov esi, arg_amp
	call arg_get_value
	jc .get_filename
	push ebx
	mov bx, 0xff00
	mov ecx, 0x100
	call str_parse_fixed		; EAX: amplification
	pop ebx
	jc .amp_error
	cmp eax, 0x400			; Maximum amplification: 4x
	ja .amp_error
	jmp .get_filename

.amp_error:
	mov ebp, esp			; Invalid amplification
	push esi
	push dword arg_amp
	mov esi, err_arg_amp
	call printf
	mov esp, ebp
	jmp .error

.get_filename:

	;----------------------------------------------------------------------
	; Get filename

	xor cl, cl
	call arg_get			; ESI: filename (first argument)
	jc .filename_error
	cmp byte [esi], '/'		; Switch (not a filename)
	je .filename_error

	; Set output values

	add esp, 8			; Discard ESI and EDX from stack
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
	pop ebp
	pop edi
	pop ebx
	ret

.error:
	mov esi, err_args
	call echo

	pop esi
	pop edx
	pop ecx
	pop eax
	stc
	jmp .exit


; Get port address from current argument value.
; -> ECX - Maximum number of argument characters to parse
;    ESI - Pointer to argument value
; <- CF - Set if error
;    DX - Parsed port address when no error

.get_port_address:
	push edi
	push edx

	mov dx, [0x408]			; DX: LPT1 port address
	mov edi, arg_port_lpt1		; /p:lpt1
	call str_cmp
	je .check_port_address
	mov dx, [0x40a]			; DX: LPT2 port address
	mov edi, arg_port_lpt2		; /p:lpt2
	call str_cmp
	je .check_port_address
	mov dx, [0x40c]			; DX: LPT3 port address
	mov edi, arg_port_lpt3		; /p:lpt3
	call str_cmp
	je .check_port_address
	mov dx, [0x40e]			; DX: LPT4 port address
	mov edi, arg_port_lpt4		; /p:lpt4
	call str_cmp
	je .check_port_address

	push eax
	push ebx			; /p:XXXX
	mov bh, cl			; BH: number of characters to convert
	xor bl, bl			; BL: no terminator character
	call str_parse_hex		; EAX: port address
	mov edi, eax
	pop ebx
	pop eax
	jc .error_port_address		; Invalid hexadecimal value
	test edi, 0xffff0000		; Invalid port address
	jnz .error_port_address
	mov dx, di			; DX: port address

.check_port_address:
	test dx, dx			; Port address sanity check
	jz .error_port_address
	add sp, 4			; Discard EDX from stack
	clc

.exit_port_address:
	pop edi
	ret

.error_port_address:
	stc
	pop edx
	jmp .exit_port_address


; Get DMA channel from current argument value.
; -> ECX - Maximum number of argument characters to parse
;    ESI - Pointer to argument value
; <- CF - Set if error
;    DL - Parsed DMA channel when no error

.get_dma_channel:
	push eax
	push ebx
	push ecx

	mov bh, cl			; BH: number of characters to convert
	xor bl, bl			; BL: no terminator character
	call str_parse_int		; EAX: DMA channel
	jc .error_dma_channel		; Invalid decimal value
	cmp eax, 7			; Invalid DMA channel
	ja .error_dma_channel
	mov dl, al			; DL: DMA channel
	clc

.exit_dma_channel:
	pop ecx
	pop ebx
	pop eax
	retn

.error_dma_channel:
	stc
	jmp .exit_dma_channel


;------------------------------------------------------------------------------
; Allocate memory for mod_channel_info structure.
;------------------------------------------------------------------------------
; -> [mod_info_addr] - Pointer to mod_info structure.
; <- CF - Set if error
;    EAX - Error code if CF set of address of mod_channel_info structure
;------------------------------------------------------------------------------

alloc_channel_info:
	push ecx

	mov eax, [chn_info_addr]
	test eax, eax
	jnz .done

	mov eax, [mod_info_addr]
	xor ecx, ecx
	mov cl, byte [eax + mod_info.num_channels]
	imul ecx, mod_channel_info.strucsize
	mov al, PMI_MEM_HI_LO
	call pmi(mem_alloc)
	jc .error
	log LOG_DEBUG, {'Allocated {u} bytes for channel info buffer at 0x{X}', 13, 10}, ecx, eax
	mov [chn_info_addr], eax

.done:
	mov eax, [chn_info_addr]
	clc

.error:
	pop ecx
	ret


;------------------------------------------------------------------------------
; Discard mod_channel_info structure from memory.
;------------------------------------------------------------------------------

free_channel_info:
	push eax

	mov eax, [chn_info_addr]
	test eax, eax
	jz .done
	log LOG_DEBUG, {'Discarding channel info buffer at 0x{X}', 13, 10}, eax
	call pmi(mem_free)

.done:
	pop eax
	ret


;==============================================================================
; UI rendering
;==============================================================================

;------------------------------------------------------------------------------
; Initialize MOD player GUI.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set
;------------------------------------------------------------------------------

ui_open:
	push ecx
	push edi
	push eax

	call gui_setup			; Setup GUI engine
	jc .error
	xor al, al			; Setup VGA palette
	mov ecx, VGA_PALETTE_ENTRIES
	mov esi, vga_palette
	call gui_set_vga_palette
	jc .error

	mov al, PMI_MEM_HI_LO		; Allocate memory for oscilloscope bmp
	mov ecx, SCOPE_BMP_SIZE * 2
	call pmi(mem_alloc)
	jc .error
	mov [scope_bmp_addr], eax
	log {'Allocated {u} bytes for oscilloscope bitmap interleave at 0x{X}', 13, 10}, ecx, eax

	mov edi, eax			; Clear oscilloscope bitmap
	mov ecx, SCOPE_BMP_SIZE / 2
	xor eax, eax
	rep stosd

	mov ebx, PB_HEIGHT		; Draw empty progress bar
	mov ecx, [gui_scr_width]
	mov dl, PB_BG_COL
	xor esi, esi
	mov edi, PB_TOP
	call gui_draw_box
	mov dword [progress_pos], 0

	mov eax, [gui_scr_width]
	shr eax, 1
	sub eax, SMP_COL_1
	mov [sample_col_1_x], eax
	add eax, SMP_COL_1 + SMP_COL_2
	mov [sample_col_2_x], eax
	mov eax, [gui_scr_height]
	sub eax, SMP_COL_HEIGHT
	mov [sample_col_top], eax

	clc
	pop eax

.exit:
	pop ecx
	pop edi
	ret

.error:
	add esp, 4			; Discard EAX from stack
	jmp .exit


;------------------------------------------------------------------------------
; Shutdown MOD player GUI.
;------------------------------------------------------------------------------

ui_close:
	push eax

	call gui_shutdown		; Shutdown GUI engine

	mov eax, [scope_bmp_addr]	; Release scope bitmap memory
	test eax, eax
	jz .done
	call pmi(mem_free)

.done:
	pop eax
	ret


;------------------------------------------------------------------------------
; Render the MOD player GUI.
;------------------------------------------------------------------------------

ui_run:
	push eax
	push ebx
	push ecx
	push edx
	push esi
	push edi
	push ebp

	; Initial setup

	mov esi, output_info		; Calculate pane margin
	call mod_get_output_info
	mov bl, [output_info + mod_output_info.buffer_format]
	and bl, MOD_BUF_CHANNEL
	mov [output_channels], bl
	mov ecx, [output_info + mod_output_info.sample_rate]
	mov ecx, [gui_scr_width]
	shr ecx, 1
	sub ecx, SCOPE_WIDTH * 64 + SCOPE_PADDING * 8

.use_margin:
	mov [pane_margin], ecx

	mov esi, [mod_info_addr]	; Calculate pixels per row for progress
	xor ebx, ebx			; bar
	mov bl, [esi + mod_info.length]
	shl ebx, 6			; EBX: total number of rows in song
	mov eax, [gui_scr_width]
	xor edx, edx
	div ebx				; EAX: horizontal pixels per row
	mov [pixel_per_row], eax

	call draw_scope_lines		; Draw scope lines
	call draw_mod_title		; Print MOD title
	call draw_sample_names		; Print instrument names
	call draw_output_device		; Print output device and sample rate
	call draw_sound_settings	; Print sound settings
	call draw_stereo_mode		; Print stereo panning mode
	mov dword [ui_flags], 0		; Reset UI flags

	call profiler_get_counter	; Initialize performance counters
	mov [perf_tick_start], eax
	mov dword [mod_perf_ticks], 0
	mov eax, [systimer_ticks]
	mov [sys_tick_start], eax
	mov [prev_vblank], eax

	;----------------------------------------------------------------------
	; Render loop

.run_loop:
	mov edx, 0x3da
	mov ebx, [prev_vblank]		; EBX: time of previous vblank
	lea esi, [ebx + 18]		; ESI: expected time for vblank

	; Wait for vertical retrace. Poll the VGA, but also set a timeout until
	; we wait. This is important to prevent UI update stalls and general
	; unresponsiveness if the CPU load is high or some background task
	; kicks in during the wait.

.wait_retrace_start:
	mov ecx, [systimer_ticks]
	sub ecx, ebx			; ECX: ticks elapsed since prev vblank
	cmp ecx, 18			; Should happen within 16.6+ ms
	jae .render_frame
	in al, dx
	test al, 8
	jz .wait_retrace_start

.wait_retrace_end:
	mov ecx, [systimer_ticks]
	sub ecx, ebx			; ECX: ticks elapsed since prev vblank
	cmp ecx, 19			; Should happen within 17.6+ ms
	jae .render_frame
	in al, dx
	test al, 8
	jnz .wait_retrace_end
	mov esi, [systimer_ticks]	; ESI: actual time for vblank

.render_frame:
	mov [prev_vblank], esi

	; Get player position, channel and output information

	mov esi, [chn_info_addr]
	call mod_get_channel_info
	mov esi, output_info
	call mod_get_output_info
	mov esi, position_info
	call mod_get_position_info

	; Draw scopes and RMS bars to the screen (do early to avoid tearing)

	mov bl, [output_info + mod_output_info.buffer_format]
	and bl, MOD_BUF_CHANNEL
	cmp [output_channels], bl	; Redraw scopes when number of output
	je .draw_scopes			; channels changed
	mov [output_channels], bl
	call clear_scope_area
	call draw_scope_lines

.draw_scopes:
	call draw_scopes
	test dword [ui_flags], UI_KBD_HELP
	jnz .skip_rms
	call draw_rms_sample

	; Render scopes to scope bitmap memory and calculate RMS sample values

	call update_rms

.skip_rms:
	call update_scopes

	; Render audio into output device buffer

	call mod_render

	; Update progress bar

	call draw_progress_bar

	; Update sound settings

	test dword [ui_flags], UI_UPD_DEV
	jz .sound_settings
	call draw_output_device
	and dword [ui_flags], ~UI_UPD_DEV

.sound_settings:
	test dword [ui_flags], UI_UPD_SOUND
	jz .stereo_mode
	call draw_sound_settings
	and dword [ui_flags], ~UI_UPD_SOUND

.stereo_mode:
	test dword [ui_flags], UI_UPD_PAN
	jz .elapsed_time
	call draw_stereo_mode
	and dword [ui_flags], ~UI_UPD_PAN

.elapsed_time:

	; Update elapsed time since start

	mov eax, [systimer_ticks]
	sub eax, [play_start_tick]
	shr eax, 10			; Convert to seconds
	cmp eax, [play_seconds]
	je .cpu_usage			; Same second, don't update
	cmp eax, 24 * 60 * 60		; Reset on daily rollover
	jb .update_time
	mov eax, [systimer_ticks]
	mov [play_start_tick], eax
	xor eax, eax

.update_time:
	mov [play_seconds], eax
	call draw_elapsed_time

.cpu_usage:

	; Calculate and draw MOD renderer CPU usage on the screen

	mov ecx, [systimer_ticks]	; ECX: current systimer ticks
	mov ebx, ecx
	sub ebx, [sys_tick_start]
	cmp ebx, CPU_WINDOW
	jbe .handle_keyboard		; Within CPU usage monitoring window
	mov dword [sys_tick_start], ecx
	call profiler_get_counter
	mov ebx, eax			; EBX: current performance counter
	sub eax, [perf_tick_start]	; EAX: total perf cntr ticks in window
	mov dword [perf_tick_start], ebx
	mov ebx, eax			; EBX: total perf cntr ticks in window
	mov eax, [mod_perf_ticks]	; EAX: render perf cntr ticks in window
	mov dword [mod_perf_ticks], 0
	test ebx, ebx
	setz cl				; Guard against division by 0
	add bl, cl
	mov edx, 1000
	mul edx
	div ebx				; EAX: pct * 10 spent with MOD rendering
	cmp eax, 1000
	jbe .update_cpu_usage
	mov eax, 1000

.update_cpu_usage:
	call draw_cpu_usage

.handle_keyboard:

	;----------------------------------------------------------------------
	; Handle keyboard events

	call kbd_get_event		; Get keyboard event
	jz .run_loop			; No keyboard event
	cmp dl, KBD_EVT_UP		; Ignore key release events
	je .run_loop

	log LOG_DEBUG, {'Key pressed {X8}/{X8}, ASCII {u8}, modifiers: {X16}', 13, 10}, ah, dh, al, ebx

	mov ah, dh			; AH: virtual keycode
	mov esi, .keymap - 4		; -4 for pre-increment

.check_key_loop:
	add esi, 4			; Pre-increment
	mov ebx, [esi]
	cmp ebx, -1
	je .check_ascii			; -1: Switch to ASCII code check
	jl .handle_keyboard		; -2: No more entries
	test ebx, ebx
	jz .skip_key			; 0: End of codes, skip jump target
	cmp bl, ah			; Found virtual keycode or ASCII code?
	jne .check_key_loop		; No match, check next code

.find_target_loop:
	add esi, 4			; Code match, find handler address
	mov ebx, [esi]
	test ebx, ebx			; End of codes?
	jnz .find_target_loop		; No, it's another code, skip
	jmp [esi + 4]			; Jump to handler

.skip_key:
	add esi, 4			; No match, skip jump target
	jmp .check_key_loop

.check_ascii:
	mov ah, al			; AH: ASCII code of keypress
	jmp .check_key_loop

	align 4

	; Keyboard event handler map. Structure of an entry (all dwords):
	; virtual_keycode, [virtual_keycode, [...]], 0, handler_address

.keymap:
	dd KC_ESC, 0, .exit
	dd KC_KP_PLUS, KC_MM_VOL_UP, 0, .vol_up
	dd KC_KP_MINUS, KC_MM_VOL_DOWN, 0, .vol_down
	dd KC_KP_0, 0, .vol_reset
	dd KC_CURSOR_LEFT, KC_CURSOR_UP, 0, .seq_prev
	dd KC_CURSOR_RIGHT, KC_CURSOR_DOWN, 0, .seq_next
	dd KC_PAGE_UP, 0, .sr_increase
	dd KC_PAGE_DOWN, 0, .sr_decrease
	dd KC_HOME, 0, .seq_first
	dd KC_F1, 0, .kbd_help_toggle
	dd -1				; Check against ASCII codes from now
	dd '+', 0, .vol_up
	dd '-', 0, .vol_down
	dd '0', 0, .vol_reset
	dd 'w', 'W', 0, .ipol_watte
	dd 'l', 'L', 0, .ipol_linear
	dd 'n', 'N', 0, .ipol_nn
	dd 'i', 'I', 0, .ipol_toggle
	dd 'm', 'M', 0, .pan_mono
	dd 'h', 'H', 0, .pan_hard
	dd 'x', 'X', 'c', 'C', 0, .pan_cross
	dd 'r', 'R', 0, .pan_real
	dd 's', 'S', 0, .pan_toggle
	dd -2				; End of table

	; Exit from the UI

.exit:
	pop ebp
	pop edi
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	ret

	; Increase amplification

.vol_up:
	movzx eax, word [amplification]
	add eax, 0x10
	cmp eax, 0x0400
	jbe .set_vol
	mov eax, 0x0400

.set_vol:
	call mod_set_amplify
	cmp ax, [amplification]
	je .handle_keyboard
	mov [amplification], ax
	or dword [ui_flags], UI_UPD_SOUND
	jmp .handle_keyboard

	; Decrease amplification

.vol_down:
	movzx eax, word [amplification]
	sub eax, 0x10
	jns .set_vol
	xor eax, eax
	jmp .set_vol

.vol_reset:
	mov eax, 0x0100
	jmp .set_vol

	; Set Watte trilinear interpolation

.ipol_watte:
	mov al, MOD_IPOL_WATTE

.set_ipol:
	cmp [dev_params + mod_dev_params.interpolation], al
	je .handle_keyboard
	mov [dev_params + mod_dev_params.interpolation], al
	call mod_set_interpolation
	or dword [ui_flags], UI_UPD_SOUND
	jmp .handle_keyboard

	; Set linear interpolation

.ipol_linear:
	mov al, MOD_IPOL_LINEAR
	jmp .set_ipol

	; Set nearest neighbor interpolation

.ipol_nn:
	mov al, MOD_IPOL_NN
	jmp .set_ipol

	; Toggle interpolation mode: nearest -> linear -> watte -> nearest ...

.ipol_toggle:
	mov al, [dev_params + mod_dev_params.interpolation]
	cmp al, MOD_IPOL_NN
	je .ipol_linear
	cmp al, MOD_IPOL_LINEAR
	je .ipol_watte
	jmp .ipol_nn

	; Set Amiga-stlye hard panning

.pan_hard:
	mov al, MOD_PAN_HARD

.set_pan:
	test dword [ui_flags], UI_UPD_PAN
	jnz .handle_keyboard		; Needs a UI loop to update mod state
	mov ah, [dev_params + mod_dev_params.stereo_mode]
	cmp ah, al
	je .handle_keyboard
	call mod_set_stereo_mode
	cmp ah, al
	je .handle_keyboard
	mov [dev_params + mod_dev_params.stereo_mode], al
	or dword [ui_flags], UI_UPD_PAN | UI_UPD_DEV
	jmp .handle_keyboard

	; Use 75% crossfeed with Amiga-style hard panning

.pan_cross:
	mov al, MOD_PAN_CROSS
	jmp .set_pan

	; Use real stereo panning via E8x and 8xx MOD effects

.pan_real:
	mov al, MOD_PAN_REAL
	jmp .set_pan

	; Force output to mono

.pan_mono:
	mov al, MOD_PAN_MONO
	jmp .set_pan

	; Toggle stereo mode: hard -> cross -> real -> mono -> hard ...

.pan_toggle:
	mov al, [dev_params + mod_dev_params.stereo_mode]
	cmp al, MOD_PAN_HARD
	je .pan_cross
	cmp al, MOD_PAN_CROSS
	je .pan_real
	cmp al, MOD_PAN_REAL
	je .pan_mono
	jmp .pan_hard

	; Next pattern in sequence

.seq_next:
	call mod_get_position
	inc ah
	mov esi, [mod_info_addr]
	cmp ah, [esi + mod_info.length]
	jb .set_seq
	mov ah, [esi + mod_info.restart_pos]

.set_seq:
	xor al, al
	mov dl, 1
	call mod_set_position
	mov [position_info + mod_position_info.position], ah
	mov [position_info + mod_position_info.row], al
	mov byte [position_info + mod_position_info.tick], 0
	jmp .handle_keyboard

	; Previous pattern in sequence

.seq_prev:
	call mod_get_position
	dec ah
	jns .set_seq
	xor ah, ah
	jmp .set_seq

.seq_first:
	xor ah, ah
	jmp .set_seq

	; Increase sample rate

.sr_increase:
	test dword [ui_flags], UI_UPD_DEV
	jnz .handle_keyboard		; Needs a UI loop to update mod state
	mov eax, 1			; Get next closest sample rate
	call mod_get_nearest_sample_rate
	mov ecx, eax			; Increase by 1000 Hz when less increase
	mov ebx, [output_info + mod_output_info.sample_rate]
	sub ecx, eax
	cmp ecx, 1000
	jae .set_sample_rate
	mov eax, [output_info + mod_output_info.sample_rate]
	add eax, 1000
	call .snap_sr			; Snap to "standard" values

.set_sample_rate:
	call mod_set_sample_rate
	or dword [ui_flags], UI_UPD_DEV
	jmp .handle_keyboard

.snap_sr:
	mov ebx, sr_stdtab

.check_std_sample_rate_loop:
	mov ecx, eax
	sub ecx, [ebx]
	cmp ecx, 500			; Snap to "standard" value within 500 Hz
	jg .check_std_sample_rate_loop_next
	cmp ecx, -500
	jl .check_std_sample_rate_loop_next
	mov eax, [ebx]
	ret

.check_std_sample_rate_loop_next:
	add ebx, 4
	cmp ebx, SR_STDTAB_END
	jb .check_std_sample_rate_loop
	ret

	; Decrease sample rate

.sr_decrease:
	test dword [ui_flags], UI_UPD_DEV
	jnz .handle_keyboard		; Needs a UI loop to update mod state
	mov eax, -1			; Get previous closest sample rate
	call mod_get_nearest_sample_rate
	mov ebx, [output_info + mod_output_info.sample_rate]
	sub ebx, eax			; Decrease by 1000 Hz when less decrease
	cmp ebx, 1000
	jae .set_sample_rate
	mov eax, [output_info + mod_output_info.sample_rate]
	sub eax, 1000
	call .snap_sr
	jmp .set_sample_rate

	; Toggle keyboard help info display

.kbd_help_toggle:
	test dword [ui_flags], UI_KBD_HELP
	jz .show_help
	and dword [ui_flags], ~UI_KBD_HELP
	call clear_info_area
	call draw_sample_names
	jmp .handle_keyboard

.show_help:
	or dword [ui_flags], UI_KBD_HELP
	call clear_info_area
	call draw_keyboard_help
	jmp .handle_keyboard


;------------------------------------------------------------------------------
; Displays progress bar on the UI.
;------------------------------------------------------------------------------
; -> EAX - CPU usage percent * 10
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_progress_bar:
	cmp byte [position_info + mod_position_info.speed], 0
	je .stopped			; MOD stopped (speed = 0)

	mov esi, [mod_info_addr]
	xor ebx, ebx
	mov bl, [esi + mod_info.length]
	shl ebx, 6			; EBX: total number of rows in song
	xor eax, eax
	xor ecx, ecx
	mov al, [position_info + mod_position_info.position]
	mov cl, [position_info + mod_position_info.row]
	shl eax, 6
	add eax, ecx			; EAX: current row in song
	mov edx, [gui_scr_width]
	mul edx
	div ebx
	shr ebx, 1
	cmp edx, ebx
	setae cl
	add ecx, eax			; ECX: pixels for played rows
	xor ebx, ebx
	mov bl, [position_info + mod_position_info.tick]
	mov eax, [pixel_per_row]
	mul ebx
	mov bl, [position_info + mod_position_info.speed]
	div ebx
	shr ebx, 1
	cmp edx, ebx
	setae bl
	add eax, ebx
	add ecx, eax			; ECX: pixels for played rows + ticks

.render_pb:
	mov eax, [progress_pos]
	cmp ecx, eax
	je .done			; Same progress position, done
	mov [progress_pos], ecx		; Draw progress bar delta
	mov ebx, PB_HEIGHT
	mov esi, eax			; ESI: old position
	mov edi, PB_TOP
	mov dl, PB_FG_COL
	sub ecx, eax			; ECX: change
	jns .draw_pb
	mov dl, PB_BG_COL		; Wrapback: use background color

.draw_pb:
	call gui_draw_box

.done:
	ret

.stopped:
	mov ecx, [gui_scr_width]	; Stopped: fill entire progress bar
	jmp .render_pb


;------------------------------------------------------------------------------
; Updates RMS values for each instrument. This code relies on the fact that
; the software wavetable unrolls samples. Make sure the unroll happens for at
; least 256 samples!
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

update_rms:
	mov ecx, 31			; Reset RMS values for each sample
	mov edi, sample_rms
	xor eax, eax
	rep stosd

	; Setup registers for RMS calculation
	; BL: sample number
	; BH: RMS sample counter
	; CL: number of channels counter
	; CH: volume
	; EDX: pointer to mod_info structure
	; ESI: pointer to mod_channel_info structure
	; EDI: pointer to current sample position
	; EBP: RMS counter

	mov edx, [mod_info_addr]
	mov cl, [edx + mod_info.num_channels]
	mov esi, [chn_info_addr]

.channel_rms_loop:
	xor eax, eax
	mov edi, [esi + mod_channel_info.sample_pos_int]
	mov ch, [esi + mod_channel_info.volume]
	mov al, mod_sample_info.strucsize
	mov bl, [esi + mod_channel_info.sample]
	test bl, bl
	jz .skip_channel_rms		; No sample, skip RMS calculation
	dec bl
	mul bl
	mov ebp, [edx + mod_info.samples + eax + mod_sample_info.addr]
	cmp dword [edx + mod_info.samples + eax + mod_sample_info.rpt_len], 0
	jne .calc_rms
	cmp edi, [edx + mod_info.samples + eax + mod_sample_info.length]
	jb .calc_rms
	xor ebp, ebp
	jmp .next_channel_rms

	; Calculate RMS for the next 256 samples

.calc_rms:
	add edi, ebp
	xor ebp, ebp
	mov bh, 256

.calc_rms_loop:
	xor eax, eax
	mov al, byte [edi]		; AL: signed sample value
	mov ax, [rms_sqtab + eax * 2]	; EAX: signed sample value ^ 2
	add ebp, eax			; EBP: accumulate squared values
	inc edi				; Next sample
	dec bh
	jnz .calc_rms_loop

	xor eax, eax
	sar ebp, 8 + 6			; EBP: average squared value (0 - 255)
	adc ebp, 0			; Rounding
	mov al, [rms_sqrttab + ebp]	; AL: average squared root (0 - 64)
	movzx ebp, ch			; EBP: volume (0 - 64)
	imul ebp, eax			; EBP: RMS * volume
	sar ebp, 6			; EBP: volume corrected linear RMS
	adc ebp, 0			; Rounding

.next_channel_rms:
	movzx ebx, bl			; EBX: sample number (0 - 31)
	add [sample_rms + ebx * 4], ebp	; Add channel RMS to sample

.skip_channel_rms:
	add esi, mod_channel_info.strucsize
	dec cl
	jnz .channel_rms_loop

	ret


;------------------------------------------------------------------------------
; Displays RMS bars on the UI for each instrument.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_rms_sample:
	mov dh, 31			; DH: sample counter
	mov ebx, 4			; EBX: bar height
	mov edi, [sample_col_top]
	add edi, 12			; EDI: bar top Y position
	xor ebp, ebp			; EBP: sample RMS value pointer

.bar_loop:
	mov eax, [prev_sample_rms + ebp]
	mov ecx, [sample_rms + ebp]
	cmp ecx, 255			; Limit linear RMS to 255
	jbe .bar_diff
	mov ecx, 255

.bar_diff:
	mov cl, [rms_logtab + ecx]	; Convert to logarithmic scale
	cmp ecx, eax
	je .next_bar			; Same RMS, done
	jae .use_rms			; Higher RMS, apply instantly
	mov esi, eax			; Decay by as much as half of the
	shr esi, 1			; old value to reduce flicker and give
	cmp esi, ecx			; a visually more pleasant appearance
	jb .use_rms
	mov ecx, esi

.use_rms:
	mov [prev_sample_rms + ebp], ecx
	mov esi, eax			; ESI: old RMS value
	mov dl, RMS_BAR_COL
	sub ecx, eax			; ECX: RMS change
	jns .draw_bar
	mov dl, 0			; Wrapback: use background color

.draw_bar:
	cmp dh, 15			; Set horizontal position in ESI
	ja .first_column
	add esi, [sample_col_2_x]
	jmp .render_bar

.first_column:
	add esi, [sample_col_1_x]

.render_bar:
	call gui_draw_box		; Render RMS bar

.next_bar:
	cmp dh, 16			; Reset Y coordinate at start of 2nd col
	jne .no_column_wrap
	mov edi, [sample_col_top]
	add edi, 12 - 16

.no_column_wrap:
	add edi, 16			; Next row
	add ebp, 4			; Next sample
	dec dh
	jnz .bar_loop

	ret


;------------------------------------------------------------------------------
; Displays CPU usage on the UI.
;------------------------------------------------------------------------------
; -> EAX - CPU usage percent * 10
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_cpu_usage:
	mov bl, STR_CV_UNSIGNED		; Convert decimal to string
	mov esi, umsg_cpu_pct
	call str_int
	call str_len
	cmp ecx, 1
	ja .add_pct_chars
	mov ah, [esi + ecx - 1]
	mov al, '0'
	mov [esi + ecx - 1], ax		; Add extra 0 for 0.n%
	inc ecx

.add_pct_chars:
	mov al, [esi + ecx - 1]		; Add decimal point and % character
	mov [esi + ecx], al
	mov byte [esi + ecx - 1], '.'
	mov word [esi + ecx + 1], '%'

	mov al, GUI_AL_LEFT		; Print CPU usage %
	mov ebx, umsg_cpu
	mov ecx, 100
	mov edx, SEC_TEXT_COL
	mov esi, [pane_margin]
	xor edi, edi
	mov ebp, font_rpgsystem
	call gui_draw_text

	ret


;------------------------------------------------------------------------------
; Displays elapsed time since the playback started on the UI.
;------------------------------------------------------------------------------
; -> EAX - Seconds since playback start
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_elapsed_time:
	mov esi, umsg_time + 6		; Convert seconds
	mov ebx, 60
	xor edx, edx
	div ebx
	call .num_zf_2
	test eax, eax
	jz .first_minute
	cmp eax, 10			; Convert minutes
	jb .within_10_minutes
	mov esi, umsg_time + 3		; Minutes 10-59
	xor edx, edx
	div ebx
	call .num_zf_2
	test eax, eax
	jz .render
	cmp eax, 10			; Convert hours
	jb .within_10_hours
	mov esi, umsg_time		; Hours 10-23
	mov edx, eax
	call .num_zf_2

.render_hours:
	mov byte [umsg_time + 2], ':'

.render:
	mov al, GUI_AL_RIGHT		; Print elapsed time since start
	mov ebx, umsg_time
	mov ecx, 80
	mov edx, SEC_TEXT_COL
	mov esi, [gui_scr_width]
	sub esi, [pane_margin]
	sub esi, ecx
	mov ebp, font_rpgsystem
	xor edi, edi
	call gui_draw_text

	ret

.within_10_hours:
	mov ah, al			; Hours 1-9
	add ah, '0'
	mov al, ' '
	mov [umsg_time], ax
	jmp .render_hours

.within_10_minutes:
	add al, '0'			; Minutes 1-9
	mov dword [umsg_time], '    '
	mov ah, ':'
	mov [umsg_time + 4], ax
	jmp .render

.first_minute:
	mov dword [umsg_time], '    '	; Clear hours and set minute to 0
	mov word [umsg_time + 4], '0:'
	jmp .render

; Convert two-digit number to zero-padded decimal string
; -> EDX - Number to convert
;    ESI - Pointer to string buffer receiving ASCII characters
; <- Destroys ECX, EDX

.num_zf_2:
	push eax

	mov eax, edx
	xor edx, edx
	mov ecx, 10
	div ecx				; EAX: 10s, EDX: 1s
	add al, '0'
	add dl, '0'
	mov ah, dl
	mov [esi], ax

	pop eax
	ret


;------------------------------------------------------------------------------
; Draw the current output device, sample rate and number of channels to the GUI.
; Requires up-to-date data at mod_info_addr and in output_info.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_output_device:
	movzx eax, word [output_device]
	mov esi, umsg_devtab
	call lookup_message		; Get device name from lookup table
	call pmi(file_get_buf)
	jc .done
	mov ebp, esp
	push esi			; Device name
	push dword [output_info + mod_output_info.sample_rate]
	mov edi, eax
	mov esi, umsg_device
	call str_format			; Create device string
	mov esp, ebp

	push ecx			; Save for next string formatting
	push edi

	mov al, GUI_AL_RIGHT		; Print device name
	mov ebx, edi
	mov ecx, SCOPE_WIDTH * 64
	mov edx, INFO_TEXT_COL
	mov esi, [gui_scr_width]
	sub esi, [pane_margin]
	sub esi, ecx
	mov edi, SCOPE_TOP - 16
	mov ebp, font_digits
	call gui_draw_text

	pop edi
	pop ecx

	mov ebp, esp
	mov esi, [mod_info_addr]
	movzx esi, byte [esi + mod_info.num_channels]
	push esi
	mov esi, umsg_channels
	call str_format			; Create channels string
	mov esi, [ebp]
	mov esp, ebp
	cmp esi, 1			; Strip plural from string when 1-channel
	jne .render_channels
	mov esi, edi
	call str_len
	mov byte [esi + ecx - 1], 0

.render_channels:
	mov ecx, SCOPE_WIDTH * 64
	mov esi, [gui_scr_width]
	sub esi, [pane_margin]
	sub esi, ecx
	mov edi, SCOPE_TOP - 28
	mov ebp, font_digits
	call gui_draw_text

.done:
	ret


;------------------------------------------------------------------------------
; Draw the current stereo mode to the GUI.
; Requires up-to-date data at mod_info_addr and in output_info.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_stereo_mode:
	mov al, [output_info + mod_output_info.buffer_format]
	and al, MOD_BUF_CHANNEL
	cmp al, MOD_BUF_1CHN
	je .mono
	cmp al, MOD_BUF_2CHNL
	je .mono
	movzx eax, byte [dev_params + mod_dev_params.stereo_mode]
	cmp al, MOD_PAN_MONO
	jne .get_string

.mono:
	mov esi, umsg_pan_mono
	jmp .render_stereo_mode

.get_string:
	mov esi, umsg_pantab
	call lookup_message		; Get panning name from lookup table
	test esi, esi
	jz .done

.render_stereo_mode:
	mov al, GUI_AL_LEFT		; Print stereo panning mode
	mov ebx, esi
	mov ecx, SCOPE_WIDTH * 64
	mov edx, INFO_TEXT_COL
	mov esi, [pane_margin]
	mov edi, SCOPE_TOP - 28
	mov ebp, font_digits
	call gui_draw_text

.done:
	ret


;------------------------------------------------------------------------------
; Draw the current amplification and interpolation to the GUI.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_sound_settings:
	movzx eax, byte [dev_params + mod_dev_params.interpolation]
	mov esi, umsg_ipoltab
	call lookup_message		; Get interpolation name from lookup table
	call pmi(file_get_buf)
	jc .done
	mov ebp, esp
	movzx ebx, word [amplification]
	push ebx			; Amplification
	push esi			; Interpolation
	mov edi, eax
	mov esi, umsg_sound
	call str_format			; Create device string
	mov esp, ebp

	mov al, GUI_AL_LEFT		; Print sound settings
	mov ebx, edi
	mov ecx, SCOPE_WIDTH * 64
	mov edx, INFO_TEXT_COL
	mov esi, [pane_margin]
	mov edi, SCOPE_TOP - 16
	mov ebp, font_digits
	call gui_draw_text

.done:
	ret


;------------------------------------------------------------------------------
; Draw the MOD title to the GUI. Requires up-to-date data at mod_info_addr.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_mod_title:
	mov ebx, [mod_info_addr]
	add ebx, mod_info.title
	cmp byte [ebx], 0
	jne .render_title
	mov ah, '\'			; Print file name if MOD title is empty
	mov ecx, -1
	mov esi, [file_name]
	mov ebx, esi
	call str_char_rpos
	jc .render_title
	lea ebx, [ebx + eax + 1]	; Cut off path in front of filename

.render_title:
	mov al, GUI_AL_CENTER
	mov ecx, [gui_scr_width]
	mov edx, PRI_TEXT_COL
	xor esi, esi
	xor edi, edi
	mov ebp, font_rpgsystem
	call gui_draw_text

	ret


;------------------------------------------------------------------------------
; Clears the MOD info area (sample names, keyboard help).
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

clear_info_area:

	; Erase info area (fill with background color)

	mov ebx, SMP_COL_HEIGHT
	add ebx, 16
	mov ecx, [gui_scr_width]
	xor dl, dl
	xor esi, esi
	mov edi, [sample_col_top]
	sub edi, 16
	call gui_draw_box

	; Clear saved sample RMS

	xor eax, eax
	mov ecx, 31
	mov edi, sample_rms
	rep stosd
	mov ecx, 31
	mov edi, prev_sample_rms
	rep stosd

	ret


;------------------------------------------------------------------------------
; Draw the name of instruments to the GUI. Requires up-to-date data at
; mod_info_addr.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_sample_names:

	; Print header above sample names

	mov al, GUI_AL_CENTER
	mov ebx, umsg_samples
	mov ecx, SMP_COL_WIDTH
	mov edx, INFO_TEXT_COL
	mov esi, [sample_col_1_x]
	mov edi, [sample_col_top]
	sub edi, 16
	mov ebp, font_digits
	call gui_draw_text

	; Print help text

	mov ebx, umsg_help
	mov esi, [sample_col_2_x]
	call gui_draw_text

	; Print sample names in two columns

	mov al, GUI_AL_LEFT
	mov ebx, [mod_info_addr]
	lea ebx, [ebx + mod_info.samples + mod_sample_info.name]
	mov ecx, SMP_COL_WIDTH
	mov edx, SEC_TEXT_COL
	mov esi, [sample_col_1_x]
	mov edi, [sample_col_top]
	mov ebp, font_sgk075

	mov ah, 16

.sample_1_loop:
	call gui_draw_text
	lea ebx, [ebx + mod_sample_info.strucsize]
	add edi, 16
	dec ah
	jnz .sample_1_loop

	mov ah, 15			; Second column
	mov esi, [sample_col_2_x]
	mov edi, [sample_col_top]

.sample_2_loop:
	call gui_draw_text
	lea ebx, [ebx + mod_sample_info.strucsize]
	add edi, 16
	dec ah
	jnz .sample_2_loop

	; Print sample numbers before each sample row

	call pmi(file_get_buf)
	jc .done
	mov ebx, eax
	mov dword [ebx], ' 1'
	mov al, GUI_AL_RIGHT
	mov ecx, 16
	mov edx, INFO_TEXT_COL
	mov esi, [sample_col_1_x]
	sub esi, 26
	mov edi, [sample_col_top]
	mov ebp, font_rpgsystem
	mov ah, 16

.sample_num_1_loop:
	call gui_draw_text
	inc byte [ebx + 1]
	cmp byte [ebx + 1], '9'
	jbe .sample_num_1_next
	mov dword [ebx], '10'

.sample_num_1_next:
	add edi, 16
	dec ah
	jnz .sample_num_1_loop

	mov ah, 15			; Second column
	mov esi, [sample_col_2_x]
	sub esi, 26
	mov edi, [sample_col_top]

.sample_num_2_loop:
	call gui_draw_text
	inc byte [ebx + 1]
	cmp byte [ebx + 1], '9'
	jbe .sample_num_2_next
	inc byte [ebx]
	mov byte [ebx + 1], '0'

.sample_num_2_next:
	add edi, 16
	dec ah
	jnz .sample_num_2_loop

.done:
	ret


;------------------------------------------------------------------------------
; Draw the keyboard help screen.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

	; Keyboard help text, must be declared before function

section .data

	%assign umsg_kbd_idx 0
	%macro umsg_kbd_help 2

	%assign umsg_kbd_idx umsg_kbd_idx + 1
umsg_kbd_k_ %+ umsg_kbd_idx:
	db %1, 0
umsg_kbd_m_ %+ umsg_kbd_idx:
	db %2, 0

	%endmacro

	umsg_kbd_help 'Esc', 'Quit to DOS'
	umsg_kbd_help 'F1', 'Toggle keyboard help / sample name display'
	umsg_kbd_help '+/-, Volume +/-', 'Increase / decrease amplification'
	umsg_kbd_help '0', 'Reset amplification to 1.0x'
	umsg_kbd_help 'Left/Right, Up/Down', 'Jump to previous / next pattern sequence'
	umsg_kbd_help 'Home', 'Restart module'
	umsg_kbd_help 'Page Up/Page Down', 'Increase / decrease sample rate'
	umsg_kbd_help 'W', 'Use Watte trilinear sample interpolation'
	umsg_kbd_help 'L', 'Use linear sample interpolation'
	umsg_kbd_help 'N', 'Use nearest neighbor (no) sample interpolation'
	umsg_kbd_help 'I', 'Toggle interpolation method'
	umsg_kbd_help 'M', 'Force output to mono'
	umsg_kbd_help 'H', 'Set Amiga-style hard stereo panning'
	umsg_kbd_help 'X', 'Set hard stereo panning with 75% crossfade'
	umsg_kbd_help 'R', 'Set real stereo panning (MOD commands 8xx and E8x)'
	umsg_kbd_help 'S', 'Toggle stereo panning method'

section .text

draw_keyboard_help:

	; Print header

	mov al, GUI_AL_CENTER
	mov ebx, umsg_kbd
	mov ecx, [gui_scr_width]
	mov edx, INFO_TEXT_COL
	xor esi, esi
	mov edi, [sample_col_top]
	sub edi, 16
	mov ebp, font_digits
	call gui_draw_text

	; Print keys

	mov al, GUI_AL_RIGHT
	mov ecx, HELP_KEY_WIDTH - 10
	mov edx, INFO_TEXT_COL
	xor esi, esi
	mov edi, [sample_col_top]
	mov ebp, font_rpgsystem

	%assign idx 1
	%rep umsg_kbd_idx

	mov ebx, umsg_kbd_k_ %+ idx
	call gui_draw_text
	add edi, 16

	%assign idx idx + 1
	%endrep

	; Print key actions

	mov al, GUI_AL_LEFT
	mov ecx, [gui_scr_width]
	sub ecx, HELP_KEY_WIDTH
	mov edx, SEC_TEXT_COL
	mov esi, HELP_KEY_WIDTH
	mov edi, [sample_col_top]
	add edi, 3
	mov ebp, font_digits

	%assign idx 1
	%rep umsg_kbd_idx

	mov ebx, umsg_kbd_m_ %+ idx
	call gui_draw_text
	add edi, 16

	%assign idx idx + 1
	%endrep

	ret


;------------------------------------------------------------------------------
; Clear scope area.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

clear_scope_area:
	mov ebx, SCOPE_HEIGHT
	mov ecx, (SCOPE_WIDTH * 64 + SCOPE_PADDING * 8) * 2
	mov esi, [pane_margin]
	mov edi, SCOPE_TOP
	xor dl, dl
	call gui_draw_box

	mov edi, [scope_bmp_addr]	; Clear oscilloscope bitmap
	mov ecx, SCOPE_BMP_SIZE / 2
	xor eax, eax
	rep stosd
	ret


;------------------------------------------------------------------------------
; Draw scope lines to the GUI. Requires up-to-date data in output_info.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers.
;------------------------------------------------------------------------------

draw_scope_lines:
	mov bl, [output_info + mod_output_info.buffer_format]
	and bl, MOD_BUF_CHANNEL
	mov esi, [pane_margin]		; ESI: X coordinate
	cmp bl, MOD_BUF_2CHN
	jne .mono_scope

	mov ebx, 1
	mov ecx, SCOPE_WIDTH * 64
	call .draw_lines
	add esi, SCOPE_WIDTH * 64 + SCOPE_PADDING * 16
	call .draw_lines
	jmp .done

.mono_scope:
	mov ebx, 1
	mov ecx, SCOPE_WIDTH * 64 * 2
	add esi, SCOPE_PADDING * 8
	call .draw_lines

.done:
	ret

; Draw scope lines for zero, limit (0 dBFS) and -3 dBFS
; -> EBX - Width of scope lines
;    ECX - Length of scope lines
;    ESI - Start X coordinate of lines
; <- Destroys DL, EDI

.draw_lines:
	mov dl, SCOPE_LINE_COL
	mov edi, SCOPE_TOP + (SCOPE_HEIGHT * 292) / 2000
	call gui_draw_box
	mov edi, SCOPE_TOP + (SCOPE_HEIGHT / 2)
	call gui_draw_box
	mov edi, SCOPE_TOP + (SCOPE_HEIGHT * 708) / 2000 + (SCOPE_HEIGHT / 2)
	call gui_draw_box
	mov dl, SCOPE_LIMIT_COL
	mov edi, SCOPE_TOP
	call gui_draw_box
	mov edi, SCOPE_TOP + SCOPE_HEIGHT - 1
	call gui_draw_box
	ret


;------------------------------------------------------------------------------
; Draw scopes to the GUI. Requires up-to-date data in output_info.
;------------------------------------------------------------------------------
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

draw_scopes:
	mov bl, [output_info + mod_output_info.buffer_format]
	and bl, MOD_BUF_CHANNEL

	; Set common scope parameters

	mov dh, [scope_blit_ilv]	; DH: blit interleave
	mov eax, [scope_bmp_addr]
	mov dl, SCOPE_COL		; DL: scope color
	mov esi, [pane_margin]		; ESI: X coordinate
	mov edi, SCOPE_TOP		; EDI: Y coordinate

	cmp bl, MOD_BUF_2CHN
	jne .mono

	; Draw two scopes in stereo output mode

	mov ecx, SCOPE_HEIGHT * 256 + SCOPE_WIDTH
	call gui_blit_bitmap_interleave
	add esi, SCOPE_WIDTH * 64 + SCOPE_PADDING * 16
	call gui_blit_bitmap_interleave
	jmp .done

.mono:

	; Draw one larger scope in mono output mode

	add esi, SCOPE_PADDING * 8
	mov ecx, SCOPE_HEIGHT * 256 + SCOPE_WIDTH * 2
	call gui_blit_bitmap_interleave

.done:

	; Toggle blit interleave

	xor dh, 1
	mov [scope_blit_ilv], dh

	ret


;------------------------------------------------------------------------------
; Compare sample value to centerline in output buffer.
;------------------------------------------------------------------------------
; -> ESI - Pointer to sample in output buffer
;    %1 - Buffer bitdepth (MOD_BUF_DEPTH constants)
;    %2 - Sample range (signed/unsigned, MOD_BUF_RANGE constants)
;    %3 - 1 to check if above, -1 to check if below
;    %4 - Jump target if condition met
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

%macro	cmp_sample 4

	%if (%2 = MOD_BUF_UINT)

	%if (%1 = MOD_BUF_8BIT)
	%assign center 0x80
	%else
	%assign center 0x8000
	%endif				; Bit depth
	%define if_above ja
	%define if_below jb

	%else

	%assign center 0
	%define if_above jg
	%define if_below jl

	%endif

	%if (%1 = MOD_BUF_8BIT)
	cmp byte [esi], center
	%elif (%1 = MOD_BUF_16BIT)
	cmp word [esi], center
	%elif (%1 = MOD_BUF_1632BIT)
	cmp dword [esi], center
	%endif

	%if (%3 = 1)
	if_above %4
	%else
	if_below %4
	%endif

%endmacro


;------------------------------------------------------------------------------
; Update output device buffer scopes in the interleaved bitmap buffer.
;------------------------------------------------------------------------------
; -> DS - Application data segment
;    %1 - Buffer bitdepth (MOD_BUF_DEPTH constants)
;    %2 - Number of buffer channels (MOD_BUF_CHANNEL constants)
;    %3 - Sample range (signed/unsigned, MOD_BUF_RANGE constants)
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

%macro	update_scopes_for 3

	; Size of sample in bytes

	%assign sample_size 1
	%if (%1 = MOD_BUF_16BIT)
	%assign sample_size 2
	%elif (%1 = MOD_BUF_1632BIT)
	%assign sample_size 4
	%endif

	; Number of playback channels

	%assign channels 1
	%if (%2 = MOD_BUF_2CHN)
	%assign channels 2
	%endif

	; Number of bytes to add for next sample

	%assign sample_next sample_size * channels
	%if (%2 = MOD_BUF_2CHNL)
	%assign sample_next sample_size * 2
	%endif

	; Number of samples to skip at the start of oscilloscope after zero
	; crossing, so the scope won't start at zero at the left side

	%assign cross_offset 24 * sample_next

	; Register usage
	; AL: pixel bit mask
	; AH: scope width in characters
	; EBX: left channel sample
	; EBP: right channel sample
	; ECX: address of byte following output buffer
	; EDX: start of output buffer
	; ESI: output buffer position
	; EDI: scope bitmap top row position

	movzx edi, byte [scope_blit_ilv]
	add edi, [scope_bmp_addr]

	mov esi, [output_info + mod_output_info.buffer_addr]
	mov eax, esi
	mov edx, esi
	mov [.buffer_start], esi
	mov [.buffer_start_2], esi
	add eax, [output_info + mod_output_info.buffer_size]
	add esi, [output_info + mod_output_info.buffer_pos]
	mov ecx, eax
	mov [.buffer_end], eax
	mov [.buffer_end_2], eax

	; But first, let's try to find a crossing point where the waveform
	; crosses the centerline to stabilize the scope a little bit. This
	; simple algorithm is quite effective for "simple" waveforms and good
	; enough for the purpose.

	mov eax, [output_info + mod_output_info.sample_rate]
	shl eax, 2
	cmp eax, 0xc800
	jbe .search_cross
	mov ah, 200			; Maximum number of samples to check

.search_cross:
	mov ebx, esi

.loop_search_pos:
	cmp esi, ecx			; Reached past output buffer end?
	jae .reset_search_pos		; Yes, reset position

.search_pos:
	cmp_sample %1, %3, 1, .found_pos
	add esi, sample_next		; Below centerline, check next one
	dec ah
	jnz .loop_search_pos
	mov esi, ebx			; Nothing above centerline, fallback
	jmp .found_cross

.found_pos:
	add esi, sample_next		; Skip positive sample

.loop_search_neg:
	cmp esi, ecx			; Reached past output buffer end?
	jae .reset_search_neg		; Yes, reset position

.search_neg:
	cmp_sample %1, %3, -1, .found_cross
	add esi, sample_next		; Above centerline, check next one
	dec ah
	jnz .loop_search_neg
	mov esi, ebx			; Nothing below centerline, fallback
	jmp .start_render

.found_cross:
	add esi, cross_offset		; Add offset to not start at zero
	cmp esi, ecx
	jb .start_render
	sub esi, ecx
	add esi, edx

	; Now render the scopes to the interleaved bitmap buffer
	; AL: pixel bit mask
	; AH: scope width in characters
	; EBX: left channel sample
	; EBP: right channel sample
	; ECX: used to draw vertical line to next left channel sample
	; EDX: used to draw vertical line to next right channel sample
	; ESI: output buffer position
	; EDI: scope bitmap top row position

.start_render:
	%if (channels = 1)
	mov eax, SCOPE_WIDTH * 8 * 256 * 2 + 0x80
	%else
	mov eax, SCOPE_WIDTH * 8 * 256 + 0x80
	%endif

.loop_render:
	cmp esi, 0x12345678		; Reached past output buffer end?
	.buffer_end EQU $ - 4
	jae .reset_position		; Yes, reset position

.render:
	%if (channels = 1)

	;----------------------------------------------------------------------
	; Read samples for mono output

	%if (%1 = MOD_BUF_8BIT)

	%if (%3 = MOD_BUF_UINT)
	xor ebx, ebx
	mov bl, [esi]			; BX: mono sample (unsigned)
	%else
	movsx ebx, byte [esi]		; BX: mono sample (signed)
	%endif
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	%if (%3 = MOD_BUF_UINT)
	xor ecx, ecx
	mov cl, [edx]			; CX: next mono sample (unsigned)
	%else
	movsx ecx, byte [edx]		; CX: next mono sample (signed)
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	mov bx, [esi]			; BX: mono sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov cx, [edx]			; CX: next mono sample

	%elif (%1 = MOD_BUF_1632BIT)

	mov ebx, [esi]			; EBX: mono sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov ecx, [edx]			; ECX: next mono sample

	%endif				; Bit depth

	%else				; Mono/stereo

	;----------------------------------------------------------------------
	; Read samples for stereo output

	%if (%1 = MOD_BUF_8BIT)

	mov bx, [esi]			; BL: left, BH: right channel sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov cx, [edx]			; CL: next left, CH: next right sample
	%if (%3 = MOD_BUF_UINT)
	movzx ebp, bh			; BP: right channel sample (unsigned)
	movzx edx, ch			; DX: next right channel sample (unsign)
	xor bh, bh			; BX: left channel sample (unsigned)
	xor ch, ch			; CX: next left channel sample (unsignd)
	%else
	movsx ebp, bh			; BP: right channel sample (signed)
	movsx edx, ch			; DX: next right channel sample (signed)
	movsx ebx, bl			; BX: left channel sample (signed)
	movsx ecx, cl			; CX: next left channel sample (signed)
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	mov ebx, [esi]			; BX: left channel sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov ecx, [edx]			; CX: next left channel sample
	shld ebp, ebx, 16		; BP: right channel sample
	shld edx, ecx, 16		; DX: next right channel sample

	%elif (%1 = MOD_BUF_1632BIT)

	mov ebx, [esi]			; EBX: left channel sample
	mov ebp, [esi + sample_size]	; EBP: right channel sample
	lea edx, [esi + sample_next]
	cmp edx, 0x12345678
	.buffer_end_2 EQU $ - 4
	jae .wrap_next_sample

.get_next_sample:
	mov ecx, [edx]			; ECX: next left channel sample
	mov edx, [edx + sample_size]	; EDX: next right channel sample

	%endif				; Bit depth

	%endif				; Mono/stereo

	;----------------------------------------------------------------------
	; Convert signed to unsigned, clip 32-bit values to 16-bit

	%if (%3 = MOD_BUF_INT)

	%if (%1 = MOD_BUF_8BIT)

	add bl, 0x80
	add cl, 0x80
	%if (channels = 2)
	add bp, 0x80
	add dl, 0x80
	and bp, 0xff
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	add bx, 0x8000
	add cx, 0x8000
	%if (channels = 2)
	add bp, 0x8000
	add dx, 0x8000
	%endif

	%elif (%1 = MOD_BUF_1632BIT)

	add ebx, 0x8080
	add ecx, 0x8080
	%if (channels = 2)
	add ebp, 0x8080
	add edx, 0x8080
	%endif

	; Clip 32-bit values to 16-bit

	test ebx, 0xffff0000
	jnz .clip_left
	sar ebx, SCOPE_V_SHIFTS + 8

.check_clip_next_left:
	test ecx, 0xffff0000
	jnz .clip_next_left
	sar ecx, SCOPE_V_SHIFTS + 8

.check_clip_next_right:
	%if (channels = 2)
	test edx, 0xffff0000
	jnz .clip_next_right
	sar edx, SCOPE_V_SHIFTS + 8

.check_clip_right:
	test ebp, 0xffff0000
	jnz .clip_right
	sar ebp, SCOPE_V_SHIFTS + 8

.check_clip_done:
	%endif				; Stereo

	%endif				; Bit depth

	%endif				; Signed/unsigned

	;----------------------------------------------------------------------
	; Convert sample values to bitmap offset

	%if (%1 = MOD_BUF_8BIT)

	shr bx, SCOPE_V_SHIFTS		; Truncate to scope height
	shr cx, SCOPE_V_SHIFTS
	and ebx, 0xffff
	and ecx, 0xffff
	%if (channels = 2)
	shr bp, SCOPE_V_SHIFTS
	shr dx, SCOPE_V_SHIFTS
	and ebp, 0xffff
	and edx, 0xffff
	%endif

	%elif (%1 = MOD_BUF_16BIT)

	shr bx, SCOPE_V_SHIFTS + 8	; Truncate to scope height
	shr cx, SCOPE_V_SHIFTS + 8
	and ebx, 0xffff
	and ecx, 0xffff
	%if (channels = 2)
	shr bp, SCOPE_V_SHIFTS + 8
	shr dx, SCOPE_V_SHIFTS + 8
	and ebp, 0xffff
	and edx, 0xffff
	%endif

	%endif				; Bit depth

	sub cx, bx			; ECX: next left channel sample diff
	sets ch
	%if (channels = 2)
	sub dx, bp			; EDX: next right channel sample diff
	sets dh
	%endif
	add cl, ch			; CL: left sample diff for floor round

	%if (channels = 1)
	shl ebx, SCOPE_H_SHIFTS + 2	; Multiply by width in bytes
	%else
	shl ebx, SCOPE_H_SHIFTS + 1	; Multiply by width in bytes
	add dl, dh			; DL: right sample diff for floor round
	shl ebp, SCOPE_H_SHIFTS + 1
	%endif

	or [edi + ebx], al		; Set pixel in scope bitmap
	sar cl, 1			; CL: left sample diff / 2
	jz .render_right		; Next sample is right above/below

	;----------------------------------------------------------------------
	; Render a vertical line towards the next sample pixel - left channel

	%if (channels = 2)
	push edx			; Use DX for temporary storage
	%endif

	setns dl			; DL: 1 if next sample lower
	mov ch, cl
	%if (channels = 1)
	movzx edx, dl
	%else
	xor dh, dh
	%endif
	sar ch, 7
	xor cl, ch
	sub cl, ch			; CL: abs(left sample diff / 2)
	dec edx
	setz ch				; CH: 1 if next sample higher
	add dl, ch			; DL: next sample > current ? 1 : -1

	%if (channels = 1)
	shl edx, SCOPE_H_SHIFTS + 2	; DX: bytes to add for next row
	%else
	shl edx, SCOPE_H_SHIFTS + 1	; DX: bytes to add for next row
	%endif

	mov ch, cl			; CH: abs(left sample diff / 2)

.render_left_line_loop:
	add ebx, edx			; Adjust offset to next row
	or [edi + ebx], al		; Set pixel in scope bitmap
	dec cl
	jnz .render_left_line_loop
	ror al, 1			; Adjust pixel bitmask for next column
	jnc .render_left_line_2_loop
	cmp ah, 1			; Can render to next character?
	je .render_left_line_done
	add ebx, 2			; Yes, adjust offset

.render_left_line_2_loop:
	add ebx, edx			; Adjust offset to next row
	or [edi + ebx], al		; Set pixel in scope bitmap
	dec ch
	jnz .render_left_line_2_loop

.render_left_line_done:
	rol al, 1			; Restore pixel bitmask

	%if (channels = 2)
	pop edx
	%endif

.render_right:
	add esi, sample_next		; Adjust poitner to next sample

	%if (channels = 2)

	; Render right channel

	or [edi + ebp + SCOPE_BMP_SIZE], al
	sar dl, 1			; DL: right sample diff / 2
	jz .render_next_pixel		; Next sample is right above/below

	;----------------------------------------------------------------------
	; Render a vertical line towards the next sample pixel - right channel

	setns bl			; BL: 1 if next sample lower
	mov dh, dl
	xor bh, bh
	sar dh, 7
	xor dl, dh
	sub dl, dh			; DL: abs(right sample diff / 2)
	dec ebx
	setz dh				; DH: 1 if next sample higher
	add bl, dh			; BL: next sample > current ? 1 : -1
	shl ebx, SCOPE_H_SHIFTS + 1	; BX: bytes to add for next row
	mov dh, dl			; DH: abs(right sample diff / 2)

.render_right_line_loop:
	add ebp, ebx			; Adjust offset to next row
	or [edi + ebp + SCOPE_BMP_SIZE], al
	dec dl
	jnz .render_right_line_loop
	ror al, 1			; Adjust pixel bitmask for next column
	jnc .render_right_line_2_loop
	cmp ah, 1			; Can render to next character?
	je .render_right_line_done
	add ebp, 2			; Yes, adjust offset

.render_right_line_2_loop:
	add ebp, ebx			; Adjust offset to next row
	or [edi + ebp + SCOPE_BMP_SIZE], al
	dec dh
	jnz .render_right_line_2_loop

.render_right_line_done:
	rol al, 1			; Restore pixel bitmask

	%endif

.render_next_pixel:

	;----------------------------------------------------------------------
	; Check to render next pixel/character in scope bitmap

	ror al, 1			; Next pixel in character
	jnc .loop_render

.render_next_char:
	add edi, 2
	dec ah				; Next character
	jnz .loop_render

	ret

	;----------------------------------------------------------------------
	; Output buffer position reset jump targets when end of buffer reached

.reset_search_pos:
	mov esi, edx
	jmp .search_pos

.reset_search_neg:
	mov esi, edx
	jmp .search_neg

.reset_position:
	mov esi, 0x12345678
	.buffer_start EQU $ - 4
	jmp .render

.wrap_next_sample:
	mov edx, 0x12345678
	.buffer_start_2 EQU $ - 4
	jmp .get_next_sample

	;----------------------------------------------------------------------
	; Clip 16-bit sample in 32-bit render buffer to 16-bit values

	%if (%1 = MOD_BUF_1632BIT && %3 = MOD_BUF_INT)

.clip_left:
	cmp ebx, 0
	setg bl				; BL: 1 if positive clip, else 0
	neg bl				; BX: 255 if positive clip, else 0
	movzx ebx, bl
	shr ebx, SCOPE_V_SHIFTS
	jmp .check_clip_next_left

.clip_next_left:
	cmp ecx, 0
	setg cl				; CL: 1 if positive clip, else 0
	neg cl				; CX: 255 if positive clip, else 0
	movzx ecx, cl
	shr ecx, SCOPE_V_SHIFTS
	jmp .check_clip_next_right

	%if (channels = 2)

.clip_next_right:
	cmp edx, 0
	setg dl				; DL: 1 if positive clip, else 0
	neg dl				; DX: 255 if positive clip, else 0
	movzx edx, dl
	shr edx, SCOPE_V_SHIFTS
	jmp .check_clip_right

.clip_right:
	cmp ebp, 0
	setg dh				; DH: 1 if positive clip, else 0
	neg dh				; DH: 255 if positive clip, else 0
	movzx ebp, dh			; BP: 255 if positive clip, else 0
	shr ebp, SCOPE_V_SHIFTS
	jmp .check_clip_done

	%endif				; Stereo

	%endif				; 16-bit in 32-bit render buffer

%endmacro


;------------------------------------------------------------------------------
; Update output devices scopes in the interleaved bitmap buffer.
;------------------------------------------------------------------------------
; -> DS - Application data segment
; <- Destroys everything except segment registers
;------------------------------------------------------------------------------

update_scopes:
	mov ah, [output_info + mod_output_info.buffer_format]
	mov esi, renderfntab		; ESI: render function jump table
	mov al, RENDERFNTAB_SIZE	; AL: entries in jump table

.renderfn_loop:
	cmp [esi], ah			; Entry found in jump table?
	je .renderfn			; Yes, jump to render function
	add si, 8			; Go to next entry in jump table
	dec al
	jnz .renderfn_loop

	; No matching scope render function, don't render anything

	ret

.renderfn:
	jmp [esi + 4]

update_scopes_8_m_u:
	update_scopes_for MOD_BUF_8BIT, MOD_BUF_1CHN, MOD_BUF_UINT

update_scopes_8_s_u:
	update_scopes_for MOD_BUF_8BIT, MOD_BUF_2CHN, MOD_BUF_UINT

update_scopes_16_m_u:
	update_scopes_for MOD_BUF_16BIT, MOD_BUF_1CHN, MOD_BUF_UINT

update_scopes_16_s_u:
	update_scopes_for MOD_BUF_16BIT, MOD_BUF_2CHN, MOD_BUF_UINT

update_scopes_32_m_s:
	update_scopes_for MOD_BUF_1632BIT, MOD_BUF_2CHNL, MOD_BUF_INT

update_scopes_32_s_s:
	update_scopes_for MOD_BUF_1632BIT, MOD_BUF_2CHN, MOD_BUF_INT


;==============================================================================
; Data area
;==============================================================================

section .data

header		db 'Therapy MOD player - quarter century later edition', 13, 10, 13, 10, 0
		HEADER_SIZE equ $ - header - 1

		; UI messages

umsg_cpu	db 'cpu: '
umsg_cpu_pct	db '0.0%  ', 0
umsg_time	db '    0:00', 0
umsg_device	db '{s} at {u} Hz', 0
umsg_sound	db '{q16:256.2}x amplify, {s}', 0
umsg_dev_spkr	db 'PC speaker', 0
umsg_dev_lpt	db 'LPT DAC', 0
umsg_dev_lptst	db 'Stereo LPT DAC', 0
umsg_dev_lptd	db 'Dual LPT DAC', 0
umsg_dev_sb1	db 'Sound Blaster', 0
umsg_dev_sb2	db 'Sound Blaster 2.0', 0
umsg_dev_sbpro	db 'Sound Blaster Pro', 0
umsg_dev_sb16	db 'Sound Blaster 16', 0
umsg_dev_dunno	db 'Playing', 0
umsg_ipol_nn	db 'No interpolation (nearest)', 0
umsg_ipol_lin	db 'Linear interpolation', 0
umsg_ipol_watte	db 'Watte trilinear interpolation', 0
umsg_pan_mono	db 'Mono', 0
umsg_pan_hard	db 'Amiga hard panning', 0
umsg_pan_cross	db '75% crossfeed', 0
umsg_pan_real	db 'Real stereo', 0
umsg_channels	db '{u} channels', 0
umsg_samples	db 'Samples / song message', 0
umsg_kbd	db 'Keyboard commands', 0
umsg_help	db 'Press F1 for help', 0

		alignb 4
umsg_devtab	dd MOD_OUT_DAC * 256 + MOD_DAC_SPEAKER, umsg_dev_spkr
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPT, umsg_dev_lpt
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPTST, umsg_dev_lptst
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPTDUAL, umsg_dev_lptd
		dd MOD_OUT_SB * 256 + MOD_SB_1, umsg_dev_sb1
		dd MOD_OUT_SB * 256 + MOD_SB_2, umsg_dev_sb2
		dd MOD_OUT_SB * 256 + MOD_SB_PRO, umsg_dev_sbpro
		dd MOD_OUT_SB * 256 + MOD_SB_16, umsg_dev_sb16
		dd 0, umsg_dev_dunno

umsg_ipoltab	dd MOD_IPOL_LINEAR, umsg_ipol_lin
		dd MOD_IPOL_WATTE, umsg_ipol_watte
		dd MOD_IPOL_NN, umsg_ipol_nn ; Must be last, MOD_IPOL_NN = 0

umsg_pantab	dd MOD_PAN_HARD, umsg_pan_hard
		dd MOD_PAN_CROSS, umsg_pan_cross
		dd MOD_PAN_REAL, umsg_pan_real
		dd 0, 0

		; Command line usage

usage		db 'Usage: tmodplay <filename.mod> [options]',13, 10, 13, 10
		db 'Options: (case-sensitive)', 13, 10, 13, 10
		db '/o:device           Select output device. Available options for "device" are:', 13, 10
		db '                    - speaker: Internal PC speaker', 13, 10
		db '                    - lpt: One or two parallel port D/A converters *', 13, 10
		db '                    - lptst: Stereo parallel port D/A converter (stereo-on-1) *', 13, 10
		db '                    - sb: Sound Blaster (detect type from BLASTER env. var.) *', 13, 10
		db '                    - sb1: Sound Blaster (pre-2.0) *', 13, 10
		db '                    - sb2: Sound Blaster 2.0 *', 13, 10
		db '                    - sbpro: Sound Blaster Pro *', 13, 10
		db '                    - sb16: Sound Blaster 16 *', 13, 10
		db '                    * These devices use the software wavetable renderer.', 13, 10, 13, 10
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
		db '                    - mono: Force mono output (fastest)', 13, 10
		db '                    - hard: Hard left/right panning as on Amiga (fast)', 13, 10
		db '                    - cross: Hard left/right panning with 75% crossfade (slower)', 13, 10
		db '                    - real: Real panning via 8xx and E8x commands (slowest)', 13, 10, 13, 10
		db '                    "panpct" specifies the initial left/right panning from', 13, 10
		db '                    center for "real" mode (default: 0%).', 13, 10, 13, 10
		db '/amp:amplification  Output amplification between 0 - 4. Value is decimal.', 13, 10, 13, 10
		db '/ipol:mode          Set sample interpolation mode for software wavetable', 13, 10
		db '                    renderer. Accepted values for "mode":', 13, 10
		db '                    - nearest: Nearest neighbor (similar to Amiga)', 13, 10
		db '                    - linear: Linear interpolation (similar to GUS, slow)', 13, 10
		db '                    - watte: Watte tri-linear interpolation (HQ, very slow)', 13, 10, 13, 10
		db '/?                  Display this command line usage information.', 13, 10
		db 0

		; Command line named arguments

arg_out		db '/o', 0
arg_out_sb	db 'sb', 0
arg_out_sb16	db 'sb16', 0
arg_out_sbpro	db 'sbpro', 0
arg_out_sb2	db 'sb2', 0
arg_out_sb1	db 'sb1', 0
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
arg_ipol	db '/ipol', 0
arg_ipol_nn	db 'nearest', 0
arg_ipol_linear	db 'linear', 0
arg_ipol_watte	db 'watte', 0
arg_help	db '/?', 0

arg_flags	db 0

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
err_arg_ipol	db 'Invalid interpolation mode "{s}" for option {s}.', 13, 10, 0
err_arg_fname	db 'Please specify the name of the file to play.', 13, 10, 0
err_args	db 13, 10, 'Type tmodplay /? for help.', 13, 10, 0

err_pmi_irq	db 'Invalid IRQ number.', 13, 10, 0
err_pmi_dma	db 'Invalid DMA channel.', 13, 10, 0

err_dos_02	db 'File not found.', 13, 10, 0
err_dos_03	db 'Path not found.', 13, 10, 0
err_dos_04	db 'Too many open files.', 13, 10, 0
err_dos_05	db 'Access denied.', 13, 10, 0
err_dos_06	db 'Invalid handle.', 13, 10, 0
err_dos_07	db 'Memory corruption due to possible buffer overrun.', 13, 10, 0
err_dos_08	db 'Insufficient memory.', 13, 10, 0
err_dos_09	db 'Invalid memory block address.', 13, 10, 0
err_dos_0a	db 'Invalid environment.', 13, 10, 0
err_dos_0f	db 'Invalid drive.', 13, 10, 0

err_mod_invalid	db 'Invalid MOD file format.', 13, 10, 0
err_mod_nb_chan	db 'Too many channels in the MOD file.', 13, 10, 0
err_mod_device	db 'Cannot initialize output device.', 13, 10, 0

err_gui_novga	db 'This program requires a VGA display adapter.', 13, 10, 0

err_generic	db 'Unable to play the file.', 13, 10, 0

		alignb 4
errtab		dd PMI_E_MEM_LOW, err_dos_08
		dd PMI_E_MEM_INVL, err_dos_07
		dd PMI_E_INV_IRQ, err_pmi_irq
		dd PMI_E_INV_DMA, err_pmi_dma
		dd MOD_ERR_INVALID, err_mod_invalid
		dd MOD_ERR_NB_CHN, err_mod_nb_chan
		dd MOD_ERR_DEVICE, err_mod_device
		dd GUI_ERR_NOVGA, err_gui_novga
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

		; Output devices

out_unknown	db 'Initializing playback', 13, 10, 0
out_speaker	db 'Using internal PC speaker', 13, 10, 0
out_lpt		db 'Using parallel port DAC on port {X16}h', 13, 10, 0
out_lptst	db 'Using stereo parallel port DAC on port {X16}h', 13, 10, 0
out_lptdual	db 'Using dual parallel port DAC on ports {X16}h and {X16}h', 13, 10, 0
out_sb1		db 'Using Sound Blaster on port {X16}h{>}, IRQ {u8}{>}, DMA {u8}', 13, 10, 0
out_sb2		db 'Using Sound Blaster 2.0 on port {X16}h{>}, IRQ {u8}{>}, DMA {u8}', 13, 10, 0
out_sbpro	db 'Using Sound Blaster Pro on port {X16}h{>}, IRQ {u8}{>}, DMA {u8}', 13, 10, 0
out_sb16	db 'Using Sound Blaster 16 on port {X16}h{>}, IRQ {u8}{>}, DMA {>}{u8}', 13, 10, 0
msg_samplerate	db 'Playback sampling rate: {u} Hz', 13, 10, 0
msg_loading	db 'Loading file: {s}', 13, 10, 0

		alignb 4
outtab		dd MOD_OUT_DAC * 256 + MOD_DAC_SPEAKER, out_speaker
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPT, out_lpt
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPTST, out_lptst
		dd MOD_OUT_DAC * 256 + MOD_DAC_LPTDUAL, out_lptdual
		dd MOD_OUT_SB * 256 + MOD_SB_1, out_sb1
		dd MOD_OUT_SB * 256 + MOD_SB_2, out_sb2
		dd MOD_OUT_SB * 256 + MOD_SB_PRO, out_sbpro
		dd MOD_OUT_SB * 256 + MOD_SB_16, out_sb16
		dd 0, out_unknown

		; Scope update functions for possible output buffer combinations

		alignb 4
renderfntab	dd MOD_BUF_8BIT | MOD_BUF_1CHN | MOD_BUF_UINT, update_scopes_8_m_u
		dd MOD_BUF_8BIT | MOD_BUF_2CHN | MOD_BUF_UINT, update_scopes_8_s_u
		dd MOD_BUF_16BIT | MOD_BUF_1CHN | MOD_BUF_UINT, update_scopes_16_m_u
		dd MOD_BUF_16BIT | MOD_BUF_2CHN | MOD_BUF_UINT, update_scopes_16_s_u
		dd MOD_BUF_1632BIT | MOD_BUF_2CHNL | MOD_BUF_INT, update_scopes_32_m_s
		dd MOD_BUF_1632BIT | MOD_BUF_2CHN | MOD_BUF_INT, update_scopes_32_s_s
		RENDERFNTAB_SIZE EQU ($ - renderfntab) / 8

		; Output device parameters

		alignb 4
prev_sample_rms	dd 31 dup (0)		; Previous RMS values for each sample
dev_params	db mod_dev_params.strucsize dup (0)

file_name	dd 0			; Pointer to MOD file name
file_handle	dd 0			; MOD file handle
scope_bmp_addr	dd 0			; Address of scope bitmap interleave
mod_info_addr	dd 0			; Address of MOD info structure
chn_info_addr	dd 0			; Address of MOD channel info structure

scope_blit_ilv	db 0			; Current scope blit interleave number

		; Fonts for GUI rendering

%include "fonts/rpgsys.inc"
%include "fonts/sgk075.inc"
%include "fonts/digits.inc"

		; VGA palette

vga_palette	db  0,  0,  0		; 00: background (black)
		db  7, 15,  9		; 01: scope lines
		db  5, 11,  6		; 02: scope upper/lower limit lines
		db 63, 61, 58		; 03: primary text color
		db 12, 60, 22		; 04: scope
		db 13, 63, 24		; 05: scope over scope lines
		db 63, 24, 13		; 06: scope over limit lines
		db 44, 42, 40		; 07: secondary text color
		db  2, 16, 22		; 08: progress bar background color
		db  3, 36, 51		; 09: progress bar progress color
		db 55, 53, 50		; 0A: info text/label color
		db  9, 31, 33		; 0B: RMS meter bar color
		VGA_PALETTE_ENTRIES EQU ($ - vga_palette) / 3

		; List of "standard" sample rates

sr_stdtab	dd 8000, 11025, 16000, 22050, 32000, 44100
		SR_STDTAB_END EQU $

		; RMS square table

rms_sqtab	dw 0, 4, 8, 9, 16, 25, 36, 49
		dw 64, 81, 100, 121, 144, 169, 196, 225
		dw 256, 289, 324, 361, 400, 441, 484, 529
		dw 576, 625, 676, 729, 784, 841, 900, 961
		dw 1024, 1089, 1156, 1225, 1296, 1369, 1444, 1521
		dw 1600, 1681, 1764, 1849, 1936, 2025, 2116, 2209
		dw 2304, 2401, 2500, 2601, 2704, 2809, 2916, 3025
		dw 3136, 3249, 3364, 3481, 3600, 3721, 3844, 3969
		dw 4096, 4225, 4356, 4489, 4624, 4761, 4900, 5041
		dw 5184, 5329, 5476, 5625, 5776, 5929, 6084, 6241
		dw 6400, 6561, 6724, 6889, 7056, 7225, 7396, 7569
		dw 7744, 7921, 8100, 8280, 8463, 8648, 8835, 9024
		dw 9215, 9408, 9603, 9800, 9999, 10200, 10403, 10608
		dw 10815, 11024, 11235, 11448, 11663, 11880, 12099, 12320
		dw 12543, 12768, 12995, 13224, 13455, 13688, 13923, 14160
		dw 14399, 14640, 14883, 15128, 15375, 15624, 15875, 16128
		dw 16383, 16128, 15875, 15624, 15375, 15128, 14883, 14640
		dw 14399, 14160, 13923, 13688, 13455, 13224, 12995, 12768
		dw 12543, 12320, 12099, 11880, 11663, 11448, 11235, 11024
		dw 10815, 10608, 10403, 10200, 9999, 9800, 9603, 9408
		dw 9215, 9024, 8835, 8648, 8463, 8280, 8100, 7921
		dw 7744, 7569, 7396, 7225, 7056, 6889, 6724, 6561
		dw 6400, 6241, 6084, 5929, 5776, 5625, 5476, 5329
		dw 5184, 5041, 4900, 4761, 4624, 4489, 4356, 4225
		dw 4096, 3969, 3844, 3721, 3600, 3481, 3364, 3249
		dw 3136, 3025, 2916, 2809, 2704, 2601, 2500, 2401
		dw 2304, 2209, 2116, 2025, 1936, 1849, 1764, 1681
		dw 1600, 1521, 1444, 1369, 1296, 1225, 1156, 1089
		dw 1024, 961, 900, 841, 784, 729, 676, 625
		dw 576, 529, 484, 441, 400, 361, 324, 289
		dw 256, 225, 196, 169, 144, 121, 100, 81
		dw 64, 49, 36, 25, 16, 9, 4, 1

		; RMS square root table

rms_sqrttab	db 0, 4, 6, 7, 8, 9, 10, 11
		db 11, 12, 13, 13, 14, 14, 15, 16
		db 16, 17, 17, 17, 18, 18, 19, 19
		db 20, 20, 20, 21, 21, 22, 22, 22
		db 23, 23, 23, 24, 24, 24, 25, 25
		db 25, 26, 26, 26, 27, 27, 27, 27
		db 28, 28, 28, 29, 29, 29, 29, 30
		db 30, 30, 31, 31, 31, 31, 32, 32
		db 32, 32, 33, 33, 33, 33, 34, 34
		db 34, 34, 34, 35, 35, 35, 35, 36
		db 36, 36, 36, 37, 37, 37, 37, 37
		db 38, 38, 38, 38, 38, 39, 39, 39
		db 39, 39, 40, 40, 40, 40, 40, 41
		db 41, 41, 41, 41, 42, 42, 42, 42
		db 42, 43, 43, 43, 43, 43, 44, 44
		db 44, 44, 44, 44, 45, 45, 45, 45
		db 45, 46, 46, 46, 46, 46, 46, 47
		db 47, 47, 47, 47, 47, 48, 48, 48
		db 48, 48, 48, 49, 49, 49, 49, 49
		db 49, 50, 50, 50, 50, 50, 50, 51
		db 51, 51, 51, 51, 51, 51, 52, 52
		db 52, 52, 52, 52, 53, 53, 53, 53
		db 53, 53, 53, 54, 54, 54, 54, 54
		db 54, 55, 55, 55, 55, 55, 55, 55
		db 56, 56, 56, 56, 56, 56, 56, 57
		db 57, 57, 57, 57, 57, 57, 58, 58
		db 58, 58, 58, 58, 58, 58, 59, 59
		db 59, 59, 59, 59, 59, 60, 60, 60
		db 60, 60, 60, 60, 61, 61, 61, 61
		db 61, 61, 61, 61, 62, 62, 62, 62
		db 62, 62, 62, 62, 63, 63, 63, 63
		db 63, 63, 63, 63, 64, 64, 64, 64

		; RMS linear -> log conversion table, linear at very low values

rms_logtab	db 0, 1, 2, 3, 4, 5, 7, 13
		db 18, 23, 27, 31, 34, 37, 40, 43
		db 45, 48, 50, 52, 54, 56, 58, 60
		db 61, 63, 64, 66, 67, 69, 70, 71
		db 73, 74, 75, 76, 77, 78, 79, 80
		db 81, 82, 83, 84, 85, 86, 87, 88
		db 88, 89, 90, 91, 92, 92, 93, 94
		db 94, 95, 96, 96, 97, 98, 98, 99
		db 100, 100, 101, 101, 102, 103, 103, 104
		db 104, 105, 105, 106, 106, 107, 107, 108
		db 108, 109, 109, 110, 110, 111, 111, 112
		db 112, 113, 113, 113, 114, 114, 115, 115
		db 116, 116, 116, 117, 117, 118, 118, 118
		db 119, 119, 119, 120, 120, 120, 121, 121
		db 122, 122, 122, 123, 123, 123, 124, 124
		db 124, 125, 125, 125, 126, 126, 126, 126
		db 127, 127, 127, 128, 128, 128, 129, 129
		db 129, 129, 130, 130, 130, 131, 131, 131
		db 131, 132, 132, 132, 132, 133, 133, 133
		db 133, 134, 134, 134, 135, 135, 135, 135
		db 135, 136, 136, 136, 136, 137, 137, 137
		db 137, 138, 138, 138, 138, 139, 139, 139
		db 139, 139, 140, 140, 140, 140, 141, 141
		db 141, 141, 141, 142, 142, 142, 142, 142
		db 143, 143, 143, 143, 143, 144, 144, 144
		db 144, 144, 145, 145, 145, 145, 145, 146
		db 146, 146, 146, 146, 146, 147, 147, 147
		db 147, 147, 148, 148, 148, 148, 148, 148
		db 149, 149, 149, 149, 149, 150, 150, 150
		db 150, 150, 150, 151, 151, 151, 151, 151
		db 151, 152, 152, 152, 152, 152, 152, 152
		db 153, 153, 153, 153, 153, 153, 154, 154

section .bss

io_buf_addr	resd 1			; Address of I/O buffer
io_buf_size	resd 1			; Size of I/O buffer
prev_vblank	resd 1			; Systimer tick of previous vblank
perf_tick_start	resd 1			; Performance counter tick start value
sys_tick_start	resd 1			; CPU usage window timeout
play_start_tick	resd 1			; Systimer tick at playback start
play_seconds	resd 1			; Number of seconds since playback start
output_info	resd (mod_output_info.strucsize + 3) / 4
position_info	resd (mod_position_info.strucsize + 3) / 4
sample_col_1_x	resd 1			; Sample first column X position
sample_col_2_x	resd 1			; Sample second column X position
sample_col_top	resd 1			; Sample column Y position
pixel_per_row	resd 1			; Number of pixels per row
progress_pos	resd 1			; Progressbar current position
ui_flags	resd 1			; UI flags
pane_margin	resd 1			; Margin (X coordinate) of panes
sample_rms	resd 31			; RMS values for each sample

sample_rate	resd 1			; Current sample rate
output_device	resw 1			; Output device
amplification	resw 1			; Amplification
output_channels	resb 1			; Output bistream channel number info
