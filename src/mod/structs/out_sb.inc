;==============================================================================
; MOD player - Sound Blaster output device
;------------------------------------------------------------------------------
; Structures
;==============================================================================

;------------------------------------------------------------------------------
; Internal state of the Sound Blaster output device
;------------------------------------------------------------------------------

struc		mod_out_sb_state

.period_base	resd 1			; Period to speed conversion base
.sample_rate	resd 1			; Actual playback sample rate
.play_tickr_int	resw 1			; Number of samples between player ticks
.play_tickr_fr	resw 1			; Fraction part of the above
.play_sam_int	resw 1			; Number of samples until next tick
.play_sam_fr	resw 1			; Fraction part of the above
.amplify	resw 1			; Output amplification
		alignb 4
.buffer_addr	resd 1			; Linear address of output buffer
.buffer_size	resd 1			; Size of the output buffer
.buffer_playprt	resb 1			; Which of the double buffers is playing
.buffer_status	resb 1			; Flag to indicate need for rendering
.buffer_pending	resb 1			; Pending render into buffer
.dev_type	resb 1			; Output DAC device type
.output_format	resb 1			; Output bitstream format
.time_constant	resb 1			; Time constant value
.sbpro_filter	resb 1			; Output filter state for SB Pro
.sbpro_init	resb 1			; Flag for SB Pro stereo fix init stage
.playing	resb 1			; Flag for playback ongoing
.strucsize:

endstruc
