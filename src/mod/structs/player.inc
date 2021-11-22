;==============================================================================
; MOD player
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Internal function pointer structure. Keep order in sync with MOD_OUT_*
; constants!
;------------------------------------------------------------------------------

struc		mod_out_dev

.none		resw 1			; No sound output
.dac		resw 1			; PC speaker/LPT DAC/no sound output
.sb		resw 1			; Sound Blaster output
.strucsize:

endstruc
