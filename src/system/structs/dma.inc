;==============================================================================
; System library - ISA DMA (Direct Memory Access) handling
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Data structure of a DMA channel register port mapping table
;------------------------------------------------------------------------------

struc		dma_ports

.page		resb 1
.start_address	resb 1
.count		resb 1
.channel_mask	resb 1
.mode		resb 1
.flipflop_reset	resb 1
		resb 2			; Must be 8 bytes long!
.strucsize:

endstruc
