;==============================================================================
; System library
;------------------------------------------------------------------------------
; Public constants
;==============================================================================

; Error codes

SYS_ERR_V86	EQU -1			; System in V86 mode

; Memory allocation modes

SYS_MEM_LO	EQU 0			; Conventional memory only
SYS_MEM_LO_HI	EQU 1			; Conventional, then high memory
SYS_MEM_HI	EQU 2			; High memory only
SYS_MEM_HI_LO	EQU 3			; High, then conventional memory
SYS_MEM_DMA	EQU 4			; Memory block for ISA DMA
SYS_MEM_DMA_LO	EQU 5			; Conventional memory block for ISA DMA

; DMA transfer modes

DMA_WRITE	EQU 0x04		; Peripheral writes to memory
DMA_READ	EQU 0x08		; Peripheral reads from memory
DMA_AUTOINIT	EQU 0x10		; Auto-init DMA
DMA_BACKWARDS	EQU 0x20		; Decrement address after each operation
DMA_ONDEMAND	EQU 0x00		; On-demand DMA transfer
DMA_SINGLE	EQU 0x40		; Single DMA (byte-by-byte) transfer
DMA_BLOCK	EQU 0X80		; Block DMA (block of memory) transfer
