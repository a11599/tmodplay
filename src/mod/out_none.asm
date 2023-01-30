;==============================================================================
; MOD player - No sound output device
;------------------------------------------------------------------------------
; No actual sound output, but keeps the MOD playroutine ticking.
;------------------------------------------------------------------------------
; Utilizes the timer interrupt (IRQ 0), which makes it unavailable for other
; purposes.
;==============================================================================

cpu 386

%include "system/api/memory.inc"
%include "system/api/pit.inc"
%include "system/api/pic.inc"
%include "mod/consts/global.inc"
%include "mod/consts/public.inc"
%include "mod/structs/global.inc"
%include "mod/structs/out_none.inc"
%include "mod/consts/out.inc"
%include "mod/api/routine.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	state(var) mod.out_state + mod_out_none_state. %+ var
%define	set_out_fn(name, lbl) at mod_out_fns. %+ name, dw %+ (lbl)

segment modplayer public use16 class=CODE align=16
segment modplayer


;------------------------------------------------------------------------------
; Set up the no sound output device.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- CF - Set if error
;    EAX - Error code if CF set or actual sample rate
;    EBX - 0 if no error
;------------------------------------------------------------------------------

setup:
	push edx

	log {'Output device: No sound', 13, 10}

	; Run the timer at ~1 kHz

	mov edx, 1000
	call far sys_pit_calc_rate
	mov [state(pit_rate)], bx
	mov [state(timer_rate)], eax	; Save actual timer tick rate

	; Done

	xor ebx, ebx
	clc

	pop edx
	retn


;------------------------------------------------------------------------------
; Shutdown the output device. No further playback is possible until the setup
; function is called again.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
;------------------------------------------------------------------------------

shutdown:
	retn


;------------------------------------------------------------------------------
; Wavetable-related dummy no-op functions.
;------------------------------------------------------------------------------

upload_sample:
	mov dh, 1			; No need to retain samples in memory

clcnoop:
	clc

noop:
	retn


;------------------------------------------------------------------------------
; Start playback on the no sound device.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- CF - Cleared
;------------------------------------------------------------------------------

play:
	push eax
	push ebx
	push ecx
	push es

	cli

	; Reset playroutine tick counters

	mov word [state(play_sam_int)], 0
	mov word [state(play_sam_fr)], 0

	; Setup and install IRQ 0 handler

	xor cl, cl
	call far sys_pic_irq_to_int
	call far sys_get_int_handler
	mov cs:[irq0_prev_handler + 2], es
	mov cs:[irq0_prev_handler], bx
	mov cs:[irq0_player_segment], ds
	mov ax, cs
	mov es, ax
	mov bx, lpt_dac_irq0_handler
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
	pop ecx
	pop ebx
	pop eax
	retn


;------------------------------------------------------------------------------
; Stop playback on the no sound device.
;------------------------------------------------------------------------------
; -> DS - Player instance segment
; <- CF - Cleared
;------------------------------------------------------------------------------

stop:
	push eax
	push bx
	push cx
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

	; Reset the PIT

	call far sys_pit_reset_irq0_rate

	sti

	pop es
	pop cx
	pop bx
	pop eax
	retn


;------------------------------------------------------------------------------
; Set the playroutine callback tick rate.
;------------------------------------------------------------------------------
; -> BX - Number of playroutine ticks per minute
;    DS - Player instance segment
;------------------------------------------------------------------------------

set_tick_rate:
	push eax
	push ebx
	push edx

	; Calculate number of samples between player ticks

	mov eax, [state(timer_rate)]
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
	shr ebx, 16			; BX.AX: Number of IRQ 0s between ticks
	mov [state(play_tickr_int)], bx
	mov [state(play_tickr_fr)], ax

	pop edx
	pop ebx
	pop eax
	retn


;==============================================================================
; Timer interrupt handler.
;==============================================================================


;------------------------------------------------------------------------------
; Macro to push registers onto the stack at IRQ 0 entry.
;------------------------------------------------------------------------------

%macro	irq0_start 0

	push eax
	push edx
	push ds

%endmacro


;------------------------------------------------------------------------------
; Macro to restore registers from the stack and return from the interrupt.
;------------------------------------------------------------------------------

%macro	irq0_exit 0

	pop ds
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

%%call_prev_handler:
	call prev_irq0_handler		; Call previous handler

	%if (%1 = 0)			; Exit IRQ 0 or jump to target label
	irq0_exit
	%else
	jmp %1
	%endif

%endmacro


;------------------------------------------------------------------------------
; Call the original IRQ 0 handler.
;------------------------------------------------------------------------------

prev_irq0_handler:
	pushf
	call 0x1234:0x1234
	irq0_prev_handler EQU $ - 4
	retn


;------------------------------------------------------------------------------
; Mono LPT DAC IRQ 0 handler.
;------------------------------------------------------------------------------

	alignb 4

lpt_dac_irq0_handler:
	irq0_start

	; DS: player instance segment

	mov ax, 0x1234
	irq0_player_segment EQU $ - 2
	mov ds, ax

	; Call playroutine tick when necessary

	mov dx, [state(play_sam_int)]	; BX: samples until playroutine tick
	test dx, dx
	jz .playroutine_tick
	dec dx
	mov [state(play_sam_int)], dx	; Update samples until playroutine tick

	irq0_eoi 0

.playroutine_tick:
	push ebx
	push ecx
	push esi
	push edi
	push ebp
	call mod_playroutine_tick
	pop ebp
	pop edi
	pop esi
	pop ecx
	pop ebx

	mov ax, [state(play_tickr_fr)]
	add [state(play_sam_fr)], ax
	setc dl
	movzx dx, dl
	add dx, [state(play_tickr_int)]
	mov [state(play_sam_int)], dx

	irq0_eoi 0


;==============================================================================
; Data area
;==============================================================================

		; Output device function pointers

		alignb 4

global mod_out_none_fns
mod_out_none_fns:
		istruc mod_out_fns
		set_out_fn(setup, setup)
		set_out_fn(shutdown, shutdown)
		set_out_fn(upload_sample, upload_sample)
		set_out_fn(free_sample, clcnoop)
		set_out_fn(set_amplify, noop)
		set_out_fn(set_interpol, noop)
		set_out_fn(set_stereomode, noop)
		set_out_fn(play, play)
		set_out_fn(stop, stop)
		set_out_fn(set_tick_rate, set_tick_rate)
		set_out_fn(set_mixer, noop)
		set_out_fn(set_sample, noop)
		set_out_fn(render, noop)
		iend
