;==============================================================================
; System library - ISA DMA (Direct Memory Access) handling
;==============================================================================

cpu 386

segment system public use16 class=CODE align=16
segment system

%include "system/structs/dma.inc"
%include "debug/log.inc"

; Shortcut macros for easier access to nested structures

%define	port(register) dma_registers + dma_ports. %+ register


;------------------------------------------------------------------------------
; Disable DMA channel (set mask bit).
;------------------------------------------------------------------------------
; -> DL - DMA channel number
;------------------------------------------------------------------------------

global sys_dma_disable_channel
sys_dma_disable_channel:
	push ax
	push esi
	push dx

	movzx esi, dl			; ESI: channel for register lookup table
	mov al, dl
	and al, 0x03
	or al, 0x04
	movzx dx, byte cs:[esi * 8 + port(channel_mask)]
	out dx, al

	pop dx
	pop esi
	pop ax
	retf


;------------------------------------------------------------------------------
; Enable DMA channel (clear mask bit).
;------------------------------------------------------------------------------
; -> DL - DMA channel number
;------------------------------------------------------------------------------

global sys_dma_enable_channel
sys_dma_enable_channel:
	push ax
	push esi
	push dx

	movzx esi, dl			; ESI: channel for register lookup table
	mov al, dl
	and al, 0x03
	movzx dx, byte cs:[esi * 8 + port(channel_mask)]
	out dx, al

	pop dx
	pop esi
	pop ax
	retf


;------------------------------------------------------------------------------
; Setup DMA transfer. Disable the channel while DMA setup is in progress.
; Disabling interrupts is also a good idea so nothing can mess with the DMA
; controller while this is happening.
;------------------------------------------------------------------------------
; -> EBX - Linear address of DMA buffer (must be word aligned for channels 4-7)
;    ECX - Number of bytes to transfer (can't cross a 64 KB boundary)
;    DL - DMA channel number
;    DH - DMA mode (DMA_*)
;------------------------------------------------------------------------------

global sys_dma_setup_channel
sys_dma_setup_channel:
	push ax
	push ebx
	push ecx
	push dx
	push esi
	push di

	cmp dl, 3
	jbe .setup_channel
	shr ecx, 1			; Count must be in words for 16-bit
	mov esi, ebx			; WTF addressing for 16-bit
	shr esi, 1
	mov bx, si

.setup_channel:
	dec ecx				; 1 byte/word less for DMA count
	movzx esi, dl			; ESI: channel for register lookup table
	mov al, dl
	and al, 0x03
	and dh, 0xfc
	or al, dh
	mov di, ax			; DI: DMA mode + channel (low byte)

	; Set start address

	movzx dx, byte cs:[esi * 8 + port(flipflop_reset)]
	mov al, 0xff
	out dx, al			; Reset flip-flop
	movzx dx, byte cs:[esi * 8 + port(start_address)]
	mov al, bl
	out dx, al			; Low byte
	mov al, bh
	out dx, al			; High byte
	movzx dx, byte cs:[esi * 8 + port(page)]
	shr ebx, 16
	mov al, bl
	out dx, al			; Page

	; Set mode

	movzx dx, byte cs:[esi * 8 + port(mode)]
	mov ax, di
	out dx, al

	; Set count

	movzx dx, byte cs:[esi * 8 + port(flipflop_reset)]
	mov al, 0xff
	out dx, al			; Reset flip-flop again
	movzx dx, byte cs:[esi * 8 + port(count)]
	mov al, cl
	out dx, al			; Low byte
	mov al, ch
	out dx, al			; High byte

	pop di
	pop esi
	pop dx
	pop ecx
	pop ebx
	pop ax
	retf


;==============================================================================
; Data area
;==============================================================================

dma_registers:	; DMA 0 (unusable)
		istruc dma_ports
		at dma_ports.page, db 0x87
		at dma_ports.start_address, db 0x00
		at dma_ports.count, db 0x01
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 1
		istruc dma_ports
		at dma_ports.page, db 0x83
		at dma_ports.start_address, db 0x02
		at dma_ports.count, db 0x03
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 2
		istruc dma_ports
		at dma_ports.page, db 0x81
		at dma_ports.start_address, db 0x04
		at dma_ports.count, db 0x05
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 3
		istruc dma_ports
		at dma_ports.page, db 0x82
		at dma_ports.start_address, db 0x07
		at dma_ports.count, db 0x08
		at dma_ports.channel_mask, db 0x0a
		at dma_ports.mode, db 0x0b
		at dma_ports.flipflop_reset, db 0x0c
		iend

		; DMA 4 (unusable)
		istruc dma_ports
		at dma_ports.page, db 0x8f
		at dma_ports.start_address, db 0xc0
		at dma_ports.count, db 0xc2
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend

		; DMA 5
		istruc dma_ports
		at dma_ports.page, db 0x8b
		at dma_ports.start_address, db 0xc4
		at dma_ports.count, db 0xc6
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend

		; DMA 6
		istruc dma_ports
		at dma_ports.page, db 0x89
		at dma_ports.start_address, db 0xc8
		at dma_ports.count, db 0xca
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend

		; DMA 7
		istruc dma_ports
		at dma_ports.page, db 0x8a
		at dma_ports.start_address, db 0xcc
		at dma_ports.count, db 0xce
		at dma_ports.channel_mask, db 0xd4
		at dma_ports.mode, db 0xd6
		at dma_ports.flipflop_reset, db 0xd8
		iend
