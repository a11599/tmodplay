;==============================================================================
; System library - PIT (Programmable Interval Timer) handling
;==============================================================================

cpu 386

%include "debug/log.inc"

segment system public use16 class=CODE align=16
segment system


;------------------------------------------------------------------------------
; Calculate PIT and actual interrupt rate for a requested timer interrupt rate.
;------------------------------------------------------------------------------
; -> EDX - Requested interrupt rate
; <- EAX - Actual interrupt rate
;    BX - PIT rate
;------------------------------------------------------------------------------

	align 4

global sys_pit_calc_rate
sys_pit_calc_rate:
	push ecx
	push edx
	push ebx

	; Handle edge cases

	cmp edx, 18			; Lowest rate
	ja .check_max_rate
	mov eax, 18
	xor bx, bx
	jmp .exit

.check_max_rate:
	cmp edx, 1193182		; Highest rate
	jb .calc_pit_rate
	mov eax, 1193182
	mov bx, 1
	jmp .exit

.calc_pit_rate:

	; Calculate PIT rate

	mov ebx, edx			; EBX: requested rate
	xor edx, edx
	mov eax, 1193182		; EAX: PIT osc. frequency
	div ebx				; EAX: PIT rate

	; Round PIT rate

	mov ecx, ebx			; ECX: requested rate / 2
	shr ecx, 1
	cmp edx, ecx			; Remainder > requested rate / 2?
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx
	mov ebx, eax			; EBX: PIT rate

	; Calculate real interrupt rate

	xor edx, edx
	mov eax, 1193182		; EAX: 1193182 (PIT osc. frequency)
	div ebx				; EAX: real interrupt rate

	; Round real interrupt rate

	mov ecx, ebx			; ECX: real interrupt rate / 2
	shr ecx, 1
	cmp edx, ecx			; Remainder > real interrupt rate / 2?
	setae dl
	movzx edx, dl			; EDX: 1 when yes, 0 otherwise
	add eax, edx

.exit:
	mov ecx, ebx			; Restore high word of EBX
	pop ebx
	mov bx, cx
	pop edx
	pop ecx
	retf


;------------------------------------------------------------------------------
; Set the rate of the PIT for channel 0 (IRQ0).
;------------------------------------------------------------------------------
; -> BX - PIT rate
;------------------------------------------------------------------------------

	align 4

global sys_pit_set_irq0_rate
sys_pit_set_irq0_rate:
	push ax

	mov al, 0x34
	out 0x43, al
	mov al, bl
	out 0x40, al
	mov al, bh
	out 0x40, al

	pop ax
	retf


;------------------------------------------------------------------------------
; Restore channel 0 of the PIT (IRQ0) to original state.
;------------------------------------------------------------------------------

	align 4

global sys_pit_reset_irq0_rate
sys_pit_reset_irq0_rate:
	push ax

	mov al, 0x36
	out 0x43, al
	xor al, al
	out 0x40, al
	out 0x40, al

	pop ax
	retf
