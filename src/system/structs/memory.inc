;==============================================================================
; Memory handling operations
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Data structure of a memory control block
;------------------------------------------------------------------------------

struc		mcb

.signature	resw 1			; Signature, used for health check
.status		resw 1			; Block status (MCB_FREE / MCB_USED)
.size		resd 1			; Size of the block in bytes
.prev		resd 1			; Linear address of previous block or -1
.next		resd 1			; Linear address of next block or -1
		alignb 4
.strucsize:

endstruc

struc		vcpi_v86_to_pm

.cr3_reg	resd 1			; Value of CR3 register
.gdtr_addr	resd 1			; Linear address of GDTR limit and base
.idtr_addr	resd 1			; Linear address of IDTR limit and base
.ldt_selector	resw 1			; LDT selector
.tss_selector	resw 1			; TSS selector
.eip_reg	resd 1			; EIP for PM entry point
.cs_reg		resw 1			; CS selector for PM entry point
.strucsize:

endstruc

;------------------------------------------------------------------------------
; Macro to create a GDT entry.
;------------------------------------------------------------------------------
; -> %1 - Linear base address (dword)
;    %2 - Limit (dword) in bytes or 4 KB
;    %3 - Flags (word)
;------------------------------------------------------------------------------

%macro	gdt_entry 3

	dw (%2 & 0xffff), (%1 & 0xffff)
	db ((%1 >> 16) & 0xff)
	dw (%3 & 0xf0ff) + ((%2 >> 8) & 0x0f00)
	db (%1 >> 24)

%endmacro