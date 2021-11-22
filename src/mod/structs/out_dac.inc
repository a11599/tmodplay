;==============================================================================
; MOD player - DAC output device
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Internal state of the DAC output device
;------------------------------------------------------------------------------

struc	mod_out_dac_state

.period_base	resd 1			; Period to speed conversion base
.sample_rate	resd 1			; Actual playback sample rate
.pit_rate	resw 1			; IRQ 0 rate
.pit_tick_count	resw 1			; Counter for old IRQ 0 handler callback
.play_tickr_int	resw 1			; Number of samples between player ticks
.play_tickr_fr	resw 1			; Fraction part of the above
.play_sam_int	resw 1			; Number of samples until next tick
.play_sam_fr	resw 1			; Fraction part of the above
.amplify	resw 1			; Output amplification
		alignb 4
.buffer_addr	resd 1			; Linear address of output buffer
.buffer_ofs	resd 1			; Address of buffer relative to instance
.buffer_size	resd 1			; Size of the output buffer
.buffer_limit	resd 1			; End of current playing buffer
.buffer_pos	resd 1			; Buffer playback position
.buffer_playprt	resb 1			; Which of the double buffers is playing
.buffer_status	resb 1			; Flag to indicate need for rendering
.dev_type	resb 1			; Output DAC device type
.output_format	resb 1			; Output bitstream format
.lpt_prn_ctrl	resb 1			; Old LPT printer controls byte value
.speakertab	resb 256
.strucsize:

endstruc
