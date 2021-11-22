;==============================================================================
; MOD player - No sound output device
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Internal state of the no sound output device
;------------------------------------------------------------------------------

struc	mod_out_none_state

.timer_rate	resd 1			; Actual timer tick rate
.pit_rate	resw 1			; IRQ 0 rate
.pit_tick_count	resw 1			; Counter for old IRQ 0 handler callback
.play_tickr_int	resw 1			; Timer ticks between player ticks
.play_tickr_fr	resw 1			; Fraction part of the above
.play_sam_int	resw 1			; Number of IRQ 0s until next tick
.play_sam_fr	resw 1			; Fraction part of the above
.strucsize:

endstruc
