;==============================================================================
; MOD player - No sound output device
;------------------------------------------------------------------------------
; No sound output, but keeps the MOD playroutine ticking.
;------------------------------------------------------------------------------
; Utilizes the timer interrupt (IRQ 0), which makes it unavailable for other
; purposes.
;==============================================================================

	cpu 386

section .text

%include "pmi/api/pmi.inc"
%include "rtl/api/string.inc"
%include "rtl/api/log.inc"
%include "rtl/api/irq.inc"
%include "rtl/api/timer.inc"

%include "mod/api/routine.inc"
%include "mod/structs/dev.inc"

; Shortcut macros for easier access to nested structures

%define	set_api_fn(name, lbl) at mod_dev_api. %+ name, dd %+ (lbl)


;------------------------------------------------------------------------------
; Set up the no sound output device.
;------------------------------------------------------------------------------
; <- CF - Set if error
;    EAX - Error code if CF set or actual sample rate
;    EBX - 0 if no error
;    ECX - 0 if no error
;------------------------------------------------------------------------------

setup:
	push edx

	log {'Output device: No sound', 13, 10}

	; Run the timer at ~1 kHz

	mov edx, 1000
	call timer_calc_rate
	mov [pit_rate], bx
	mov [timer_rate], eax		; Save actual timer tick rate

	; Done

	xor ebx, ebx
	xor ecx, ecx
	clc

	pop edx
	ret


;------------------------------------------------------------------------------
; Dummy no-op functions.
;------------------------------------------------------------------------------

upload_sample:
	mov dh, 1			; No need to retain samples in memory

clcnoop:
	clc

noop:
	ret


;------------------------------------------------------------------------------
; Start playback on the no sound device.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

play:
	push eax
	push ebx
	push ecx
	push edx

	; Reset playroutine tick counters

	mov dword [play_irq_int], 0
	mov dword [play_irq_fr], 0

	; Setup and install IRQ 0 handler

	xor al, al
	call pmi(get_irq_hndlr)
	mov [irq0_prev_handler], edx
	mov [irq0_prev_handler + 4], cx
	mov [irq0_player_segment], ds
	mov cx, cs
	mov edx, null_irq0_handler
	call pmi(set_irq_hndlr)

	; Set the rate of the timer interrupt

	mov word [pit_tick_count], 0
	mov bx, [pit_rate]
	call timer_set_rate

	; Enable IRQ 0

	xor cl, cl
	call irq_enable

	clc

	pop edx
	pop ecx
	pop ebx
	pop eax
	ret


;------------------------------------------------------------------------------
; Stop playback on the no sound device.
;------------------------------------------------------------------------------
; <- CF - Cleared
;------------------------------------------------------------------------------

stop:
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

	clc

	pop edx
	pop ecx
	pop ebx
	pop eax
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

	mov eax, [timer_rate]
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
	shl eax, 16			; EBX.EAX: number of IRQs between ticks
	mov [play_tickr_int], ebx
	mov [play_tickr_fr], eax

	pop edx
	pop ebx
	pop eax
	ret


;==============================================================================
; Timer interrupt handler.
;==============================================================================

;------------------------------------------------------------------------------
; No sound IRQ 0 handler.
;------------------------------------------------------------------------------

	align 16

null_irq0_handler:
	push eax
	push edx
	push ds

	; DS: player instance segment

	mov ax, 0x1234
	irq0_player_segment EQU $ - 2
	mov ds, ax

	; Call playroutine tick when necessary

	mov edx, [play_irq_int]		; EDX: IRQs until playroutine tick
	test edx, edx
	jz .playroutine_tick
	dec edx

.exit:
	mov [play_irq_int], edx		; Update IRQs until playroutine tick

	mov ax, [pit_rate]		; Call old handler if internal tick
	add [pit_tick_count], ax	; counter overflows 16-bit
	jc .jmp_prev_handler

	irq_pic_eoi 0			; Send end-of-interrupt to PIC

	pop ds
	pop edx
	pop eax
	iret

	align 16

.jmp_prev_handler:
	pop ds
	pop edx
	pop eax

	jmp 0x1234:0x12345678		; Jump to previous handler
	irq0_prev_handler EQU $ - 6

	align 16

.playroutine_tick:
	push ebx
	push ecx
	push esi
	push edi
	push ebp
	push es
	mov es, ax
	call mod_playroutine_tick
	pop es
	pop ebp
	pop edi
	pop esi
	pop ecx
	pop ebx

	mov eax, [play_tickr_fr]
	mov edx, [play_tickr_int]
	add [play_irq_fr], eax
	adc edx, 0
	jmp .exit


;==============================================================================
; Data area
;==============================================================================

section .data

		; Output device API jump table

global mod_dev_none_api
mod_dev_none_api istruc mod_dev_api
		set_api_fn(setup, setup)
		set_api_fn(shutdown, noop)
		set_api_fn(upload_sample, upload_sample)
		set_api_fn(free_sample, clcnoop)
		set_api_fn(set_channels, noop)
		set_api_fn(set_amplify, noop)
		set_api_fn(set_interpol, noop)
		set_api_fn(set_stereomode, noop)
		set_api_fn(play, play)
		set_api_fn(stop, stop)
		set_api_fn(set_tick_rate, set_tick_rate)
		set_api_fn(set_mixer, noop)
		set_api_fn(set_sample, noop)
		set_api_fn(render, noop)
		set_api_fn(get_mixer_info, noop)
		set_api_fn(get_info, noop)
		set_api_fn(get_position, mod_playroutine_get_position_info)
		set_api_fn(reset_channels, noop)
		iend

section .bss

timer_rate	resd 1			; Actual timer frequency rate
play_tickr_int	resd 1			; Timer IRQs between player ticks
play_tickr_fr	resd 1			; Fraction part of the above
play_irq_int	resd 1			; Timer IRQs until next player tick
play_irq_fr	resd 1			; Fraction part of the above

pit_rate	resw 1			; IRQ 0 rate
pit_tick_count	resw 1			; Counter for old IRQ 0 handler callback
