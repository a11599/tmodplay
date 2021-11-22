;==============================================================================
; System library - PIC (Programmable Interrupt Controller) handling
;==============================================================================

cpu 386

segment system public use16 class=CODE align=16
segment system

%include "debug/log.inc"


;------------------------------------------------------------------------------
; Convert an IRQ to interrupt number.
;------------------------------------------------------------------------------
; -> CL - IRQ number
; <- CH - Interrupt number
;------------------------------------------------------------------------------

global sys_pic_irq_to_int
sys_pic_irq_to_int:
	mov ch, cl
	add ch, 0x08
	cmp cl, 8
	jb .exit
	add ch, (0x70 - 0x08 - 0x08)

.exit:
	retf


;------------------------------------------------------------------------------
; Check if an IRQ is masked (ignored) by the PIC.
;------------------------------------------------------------------------------
; -> CL - IRQ number
; <- ZF - Set if IRQ is masked (ignored)
;------------------------------------------------------------------------------

global sys_pic_irq_enabled
sys_pic_irq_enabled:
	push ax
	push cx
	push dx

	; Master PIC mask bits must be checked in any case, masking IRQ 2 masks
	; all IRQs (8 - 15) of the slave PIC

	in al, 0x21
	cmp cl, 8
	jb .get_irq_mask
	test al, 0x04			; IRQ 2 masked -> slave PIC masked
	jnz .exit

	; IRQ 2 not masked, get mask bit of slave PIC for IRQs 8 - 15

	sub cl, 8
	in al, 0xa1

.get_irq_mask:

	; Get and test IRQ mask bit from PIC

	mov ah, 1
	shl ah, cl
	test al, ah

.exit:
	pop dx
	pop cx
	pop ax
	retf


;------------------------------------------------------------------------------
; Disable IRQ (set mask bit). The PIC will ignore these IRQs and no interrupt
; will be raised.
;------------------------------------------------------------------------------
; -> CL - IRQ number
;------------------------------------------------------------------------------

global sys_pic_disable_irq
sys_pic_disable_irq:
	push ax
	push cx
	push dx

	; Setup input for mask bit clearing

	mov dx, 0x21
	cmp cl, 8
	jb .disable_irq
	sub cl, 8
	mov dx, 0xa1

.disable_irq:

	; Set mask bit of IRQ (enable)

	mov ah, 1
	shl ah, cl
	in al, dx
	or al, ah
	out dx, al

.exit:
	pop dx
	pop cx
	pop ax
	retf


;------------------------------------------------------------------------------
; Enable IRQ (clear mask bit).
;------------------------------------------------------------------------------
; -> CL - IRQ number
;    CH - Set to 1 to force enabling the slave PIC for IRQ 8-15, 0 otherwise
;------------------------------------------------------------------------------

global sys_pic_enable_irq
sys_pic_enable_irq:
	push ax
	push cx
	push dx

	; Setup input for mask bit clearing

	mov dx, 0x21
	cmp cl, 8
	jb .enable_irq
	sub cl, 8
	mov dx, 0xa1
	test ch, ch
	jz .enable_irq

	; IRQ 8 - 15: check mask bit of IRQ 2

	in al, 0x21
	test al, 0x04
	jz .enable_irq			; IRQ 2 not masked -> slave PIC masked
	and al, ~(0x04)
	out 0x21, al			; Unmask (enable) IRQ 2 (slave PIC)

.enable_irq:

	; Clear mask bit of IRQ (enable)

	mov ah, 1
	shl ah, cl
	not ah
	in al, dx
	and al, ah
	out dx, al

.exit:
	pop dx
	pop cx
	pop ax
	retf
