;==============================================================================
; System library - PIC (Programmable Interrupt Controller) handling
;------------------------------------------------------------------------------
; API
;==============================================================================

extern sys_pic_irq_to_int
extern sys_pic_irq_enabled
extern sys_pic_enable_irq
extern sys_pic_disable_irq


;------------------------------------------------------------------------------
; Macro to send end-of-interrupt signal to the PIC.
;------------------------------------------------------------------------------
; -> %1 - IRQ number
; <- Destroys AL
;------------------------------------------------------------------------------
; EOI is sent to slave PIC for IRQs 8 - 15. An EOI is sent to the master PIC
; for all IRQs.
;------------------------------------------------------------------------------

%macro	sys_pic_eoi 1

	%ifnum %1

	; %1 is a numeric constant, optimize code

	mov al, 0x20
	%if (%1 > 7)
	out 0xa0, al			; Send EOI to slave PIC
	%endif
	out 0x20, al			; Always send EOI to master PIC

	%else

	; %1 is register or memory data

	cmp %1, 7
	mov al, 0x20
	jbe %%eoi_master
	out 0xa0, al			; Send EOI to slave PIC

%%eoi_master:
	out 0x20, al			; Always send EOI to master PIC

	%endif

%endmacro


;------------------------------------------------------------------------------
; Macro to send end-of-interrupt signal to the PIC for a spurious IRQ 15.
;------------------------------------------------------------------------------
; <- Destroys AL
;------------------------------------------------------------------------------
; No EOI should be sent for spurious IRQ 7. For spurious IRQ 15, an EOI must
; be sent to the master PIC.
;------------------------------------------------------------------------------

%macro	sys_pic_spurious_irq15_eoi 0

	mov al, 0x20
	out 0x20, al			; Send EOI to master PIC

%endmacro


;------------------------------------------------------------------------------
; Macro to check if a hardware interrupt is raised (pending). Can be used to
; differentiate software interrupts and exceptions from hardware IRQs on
; shared interrupt numbers or to check for spurious IRQ.
;------------------------------------------------------------------------------
; -> %1 - IRQ number
; <- ZF - Set if no IRQ being serviced
;    Destroys AL and also CL, DX if %1 is register or memory data
;------------------------------------------------------------------------------

%macro	sys_pic_irq_serviced 1

	%ifnum %1

	; %1 is a numeric constant, optimize code

	%assign port 0x20
	%assign mask (1 << %1)
	%if (%1 > 7)
	%assign port 0xa0
	%assign mask (1 << (%1 - 8))
	%endif

	mov al, 0x0b
	out port, al
	in al, port
	test al, mask

	%else

	; %1 is register or memory data

	%ifnidni %1, cl
	mov cl, %1
	%endif
	mov dx, 0x20
	cmp cl, 7
	jbe %%get_isr
	mov dl, 0xa0
	sub cl, 8

%%get_isr:
	mov al, 0x0b
	out dx, al
	in al, dx
	mov dl, 1
	shl dl, cl
	test al, dl

	%endif

%endmacro


;------------------------------------------------------------------------------
; Macro to check if a hardware interrupt is spurious.
;------------------------------------------------------------------------------
; -> %1 - IRQ number (can't be AL and can't be the same as %2)
;    %2 - Byte register (r8) to set with result value
; <- %2 - Register set:
;         0 - Not spurious
;         1 - Spurious IRQ 7
;         2 - Spurious IRQ 15
;    Destroys AL and also DX if %1 is register or memory data
;------------------------------------------------------------------------------
; An IRQ is spurious if the PIC can't identify the source IRQ number after it
; raised an interrupt towards the CPU. Causes are usually incorrectly sent
; EOIs and/or noise on IRQ lines. In this case the PIC raises IRQ 7 or IRQ 15,
; depending on whether the PIC is master or slave. IRQ 7 and 15 handlers must
; check for spurious IRQ and treat it specially: no EOI should be sent for
; IRQ 7, but an EOI should be sent to the master PIC for IRQ 15. See macro
; sys_pic_spurious_irq15_eoi.
;------------------------------------------------------------------------------

%macro	sys_pic_irq_spurious 2

	%ifnum %1

	; %1 is a numeric constant, optimize code

	%if (%1 = 7 || %1 = 15)		; Only IRQ 7 and 15 can be spurious
	push cx
	sys_pic_irq_serviced %1		; Check if IRQ is serviced by the PIC
	pop cx
	setz %2				; ZF = 1: no IRQ -> spurious
	%else
	mov %2, 0
	%endif

	%else

	; %1 is register or memory data

	mov al, %1			; Only IRQ 7 and 15 can be spurious
	and al, 0x07
	cmp al, 0x07
	mov %2, 0
	jne %%exit

	push cx
	sys_pic_irq_serviced %1		; Check if IRQ is serviced by the PIC
	pop cx
	setz %2				; ZF = 1: no IRQ -> spurious
	add %2, %2

%%exit:

	%endif

%endmacro
